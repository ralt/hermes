#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <errno.h>
#include <security/pam_modules.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <libssh/libssh.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#define RSA_TYPE 1

#define COMMAND_GET_KEYS 1

#define RET_NO_HERMES_DEVICE 0
#define RET_HERMES_DEVICE_FOUND 1

struct hermes_device
{
	uint8_t type;
	uint32_t public_key_length;
	char *public_key;
	uint32_t private_key_length;
	char *private_key;
};

static bool hermes_new_device(struct hermes_device *device, uint8_t *buffer);
static bool can_login(struct hermes_device*, const char*);
static bool is_authenticated(const char*);


PAM_EXTERN int pam_sm_setcred(pam_handle_t *pamh, int flags, int argc, const char **argv)
{
	char *user;
	int retval;
	retval = pam_get_user(pamh, (const char**) &user, NULL);
	if (retval != PAM_SUCCESS)
	{
		return retval;
	}

	return is_authenticated(user) ? PAM_SUCCESS : PAM_AUTH_ERR;
}

PAM_EXTERN int pam_sm_authenticate(pam_handle_t *pamh, int flags, int argc, const char **argv)
{
	char *user;
	int retval;
	retval = pam_get_user(pamh, (const char**) &user, NULL);
	if (retval != PAM_SUCCESS)
	{
		return retval;
	}

	return is_authenticated(user) ? PAM_SUCCESS : PAM_AUTH_ERR;
}

static bool can_login(struct hermes_device *device, const char *user)
{
	ssh_session sess;
	int rc;
	ssh_key public_key, private_key;
	bool ret;

	sess = ssh_new();
	if (sess == NULL)
	{
		fprintf(stderr, "%s: can't create ssh session\n", strerror(errno));
		return false;
	}

	ssh_options_set(sess, SSH_OPTIONS_HOST, "127.0.0.1");

	rc = ssh_connect(sess);
	if (rc != SSH_OK)
	{
		fprintf(stderr, "%s: can't connect to local ssh\n", strerror(errno));
		ret = false;
		goto clean_ssh;
	}

	if ((ssh_pki_import_pubkey_base64(device->public_key,
					  SSH_KEYTYPE_RSA,
					  &public_key)) != SSH_OK)
	{
		fprintf(stderr, "%s: can't import the public key\n", strerror(errno));
		ret = false;
		goto clean_connection;
	}

	if ((ssh_userauth_try_publickey(sess, user, public_key)) != SSH_AUTH_SUCCESS)
	{
		fprintf(stderr, "%s: the public key isn't authorized\n", strerror(errno));
		ret = false;
		goto clean_public_key;
	}

	if ((ssh_pki_import_privkey_base64(device->private_key,
					   NULL,
					   NULL,
					   NULL,
					   &private_key)) != SSH_OK)
	{
		fprintf(stderr, "%s: can't import the private key\n", strerror(errno));
		ret = false;
		goto clean_public_key;
	}

	if ((ssh_userauth_publickey(sess, user, private_key)) != SSH_AUTH_SUCCESS)
	{
		fprintf(stderr, "%s: the private key is invalid\n", strerror(errno));
		ret = false;
		goto clean_private_key;
	}

	ret = true;

clean_private_key:
	ssh_key_free(private_key);

clean_public_key:
	ssh_key_free(public_key);

clean_connection:
	ssh_disconnect(sess);

clean_ssh:
	ssh_free(sess);

	return ret;
}

/*
  - first byte is the type
  - next 4 bytes are the public key length
  - next x bytes are the public key itself
  - next 4 bytes are the private key length
  - next y bytes are the private key itself
 */
static bool hermes_new_device(struct hermes_device *device, uint8_t *buffer)
{
	const size_t type_offset = 1;
	const size_t public_key_length_offset = 4;
	const size_t private_key_length_offset = 4;
	size_t offset;

	device->type = buffer[0];
	for (size_t i = 0; i < sizeof(device->public_key_length); i++)
	{
		(&device->public_key_length)[i] = buffer[i + type_offset];
	}

	device->public_key = malloc((sizeof(uint8_t) * device->public_key_length) + 1);
	if (device->public_key == NULL)
	{
		fprintf(stderr, "%s: can't malloc the public key\n", strerror(errno));
		return false;
	}

	offset = type_offset + public_key_length_offset;
	for (size_t i = 0; i < device->public_key_length; i++)
	{
		device->public_key[i] = buffer[i + offset];
	}
	device->public_key[device->public_key_length] = '\0';

	offset = offset + device->public_key_length + 1;
	for (size_t i = 0; i < sizeof(device->private_key_length); i++)
	{
		(&device->private_key_length)[i] = buffer[i + offset];
	}

	device->private_key = malloc((sizeof(uint8_t) * device->private_key_length) + 1);
	if (device->public_key == NULL)
	{
		fprintf(stderr, "%s: can't malloc the private key\n", strerror(errno));
		return false;
	}

	offset = type_offset + public_key_length_offset + device->public_key_length + 1 + private_key_length_offset;
	for (size_t i = 0; i < device->private_key_length; i++)
	{
		device->private_key[i] = buffer[i + offset];
	}
	device->private_key[device->private_key_length] = '\0';

	return true;
}

static void hermes_free_device(struct hermes_device *device)
{
	free(device->public_key);
	free(device->private_key);
	free(device);
}

static bool is_authenticated(const char *user)
{
	const char *socket_path = "/var/run/hermes.sock";
	struct sockaddr_un addr;
	int fd, rc;
	uint32_t command = COMMAND_GET_KEYS;
	uint32_t data_length;
	bool retval;
	struct hermes_device *device;
	uint8_t ret;
	uint8_t *buffer;

	if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
	{
		perror("socket");
		return false;
	}

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strncpy(addr.sun_path, socket_path, sizeof(addr.sun_path) - 1);

	if (connect(fd, (const struct sockaddr*) &addr, sizeof(addr)) == -1)
	{
		perror("connect");
		return false;
	}

	write(fd, &command, sizeof(command));

	if ((rc = read(fd, &ret, sizeof(ret))) != sizeof(ret))
	{
		perror("read ret");
		return false;
	}

	if (ret != RET_NO_HERMES_DEVICE && ret != RET_HERMES_DEVICE_FOUND)
	{
		fprintf(stderr, "Read returned unexpected value\n");
		return false;
	}

	if (ret == RET_NO_HERMES_DEVICE)
	{
		fprintf(stderr, "No hermes device was found\n");
		return false;
	}

	if (ret == RET_HERMES_DEVICE_FOUND)
	{
		if ((rc = read(fd, &data_length, sizeof(data_length))) !=
		    sizeof(data_length))
		{
			perror("read data_length");
			return false;
		}

		device = malloc(sizeof(struct hermes_device));
		if (device == NULL)
		{
			perror("malloc hermes device");
			return false;
		}

		buffer = malloc(sizeof(uint8_t) * data_length);
		if (buffer == NULL)
		{
			perror("malloc buffer");
			return false;
		}

		if ((rc = read(fd, buffer, data_length)) != data_length)
		{
			perror("read buffer");
			free(device);
			return false;
		}

		if (hermes_new_device(device, buffer) != true)
		{
			retval = false;
			goto error_exit;
		}

		retval = can_login(device, user);
		goto free_hermes_device;
	}

error_exit:
	retval = false;

free_hermes_device:
	hermes_free_device(device);

	return retval;
}
