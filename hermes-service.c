/*
  The service, run as root, will listen on the /var/run/hermes.sock
  socket. The only command it will understand is a four-bytes long
  command, being a number mapping an action.

  Really, only one action is needed: get me the content of the
  connected hermes device. Having 4 bytes is cheap enough, and leaves
  other possibilities later on, if needed.

  The "get me the content" command needs the value "1". It can return
  one of these 2 results:
    - first byte 0, means "there is no hermes device"
    - first byte 1, means "there is an hermes device". It is followed
      by 4 bytes specifying the length of the data coming afterwards,
      namely:
	- 1 byte for the type
	- 4 bytes for the length of the public key
	- x bytes for the public key
	- 4 bytes for the length of the private key
	- y bytes for the private key
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/stat.h>
#include <grp.h>
#include <errno.h>
#include <glob.h>

#define FINGERPRINT_LENGTH 5

#define COMMAND_GET_KEYS 1

#define RET_NO_HERMES_DEVICE 0
#define RET_HERMES_DEVICE_FOUND 1

static int globerr(const char*, int);
static bool is_block_device(const char*);
static bool has_hermes_fingerprint(const char*);
static bool is_hermes_device(const char*);
static size_t handle_command(uint32_t command, uint8_t **buffer);
static size_t read_hermes_device(char *path, uint8_t **buffer);

int main(int argc, char *argv[])
{
	const char *socket_path = "/var/run/hermes.sock";
	struct sockaddr_un addr;
	uint32_t command;
	int fd, rc, cl;

	if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
		perror("socket");
		exit(EXIT_FAILURE);
	}

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strncpy(addr.sun_path, socket_path, sizeof(addr.sun_path) - 1);

	unlink(socket_path);

	if (bind(fd, (const struct sockaddr*) &addr, sizeof(addr)) == -1)
	{
		perror("bind");
		exit(EXIT_FAILURE);
	}

	if (chmod(socket_path, S_IRGRP | S_IWGRP) == -1)
	{
		perror("chmod");
		exit(EXIT_FAILURE);
	}

	struct group *hermes_group = getgrnam("hermes");
	if (hermes_group == NULL)
	{
		perror("hermes group not found");
		exit(EXIT_FAILURE);
	}

	if (chown(socket_path, 0, hermes_group->gr_gid) == -1)
	{
		perror("chown");
		exit(EXIT_FAILURE);
	}

	if (listen(fd, 5) == -1)
	{
		perror("listen");
		exit(EXIT_FAILURE);
	}

	while (true)
	{
		if ((cl = accept(fd, NULL, NULL)) == -1)
		{
			perror("accept");
			continue;
		}

		while ((rc = read(cl, &command, sizeof(command))) > 0)
		{
			uint8_t *buffer;
			size_t buffer_length = handle_command(command, &buffer);

			if (write(cl, buffer, buffer_length) != buffer_length)
			{
				perror("write");
				exit(EXIT_FAILURE);
			}

			free(buffer);
		}

		if (rc == -1)
		{
			perror("read");
			exit(EXIT_FAILURE);
		}

		if (rc == 0)
		{
			continue;
		}
	}

	return EXIT_SUCCESS;
}

static size_t handle_command(uint32_t command, uint8_t **buffer)
{
	glob_t files;
	int retval;
	size_t ret;
	bool device_found = false;
	char *hermes_device;

	retval = glob("/dev/*", GLOB_ERR | GLOB_NOSORT, globerr, &files);
	if (retval != 0)
	{
		return false;
	}

	for (size_t i = 0; i < files.gl_pathc; i++)
	{
		if (is_hermes_device(files.gl_pathv[i]))
		{
			device_found = true;
			hermes_device = malloc(strlen(files.gl_pathv[i]) * sizeof(char));
			if (hermes_device == NULL)
			{
				perror("malloc hermes_device");
				exit(EXIT_FAILURE);
			}

			memcpy(hermes_device, files.gl_pathv[i], strlen(files.gl_pathv[i]) + 1);
		}
	}

	*buffer = malloc(sizeof(uint8_t));
	if (*buffer == NULL)
	{
		perror("malloc buffer");
		exit(EXIT_FAILURE);
	}

	if (!device_found)
	{
		*buffer[0] = RET_NO_HERMES_DEVICE;
		ret = 1;
	}

	if (device_found)
	{
		*buffer[0] = RET_HERMES_DEVICE_FOUND;
		ret = read_hermes_device(hermes_device, buffer);
		free(hermes_device);
	}

	globfree(&files);

	return ret;
}

static size_t read_hermes_device(char *path, uint8_t **buffer)
{
	size_t offset = 1;
	FILE *fd;
	size_t bytes_read;

	uint8_t type;
	uint32_t public_key_length, private_key_length;
	uint8_t *public_key, *private_key;

	uint32_t ret = 0;

	fd = fopen(path, "rb");
	if (fd == NULL)
	{
		fprintf(stderr, "%s: can't read %s\n", strerror(errno), path);
		return false;
	}

	if ((fseek(fd, FINGERPRINT_LENGTH, 0)) != 0)
	{
		fprintf(stderr, "%s: can't fseek %d bytes in %s\n",
			strerror(errno), FINGERPRINT_LENGTH, path);
		goto safe_close;
	}

	bytes_read = fread(&type, sizeof(uint8_t), 1, fd);
	if (bytes_read < 1)
	{
		fprintf(stderr, "%s: can't read the type\n", strerror(errno));
		goto safe_close;
	}

	bytes_read = fread(&public_key_length, sizeof(uint32_t), 1, fd);
	if (bytes_read != 1)
	{
		fprintf(stderr, "%s: can't read the public key length\n", strerror(errno));
		goto safe_close;
	}

	public_key = malloc((sizeof(uint8_t) * public_key_length) + 1);
	if (public_key == NULL)
	{
		fprintf(stderr, "%s: can't malloc the public key\n", strerror(errno));
		goto safe_close;
	}

	bytes_read = fread(public_key,
			   sizeof(uint8_t),
			   public_key_length,
			   fd);
	if (bytes_read != public_key_length)
	{
		fprintf(stderr, "%s: can't read the public key\n", strerror(errno));
		goto safe_close;
	}

	bytes_read = fread(&private_key_length, sizeof(uint32_t), 1, fd);
	if (bytes_read != 1)
	{
		fprintf(stderr, "%s: can't read the private key length\n", strerror(errno));
		goto safe_close;
	}

	private_key = malloc((sizeof(uint8_t) * private_key_length) + 1);
	if (private_key == NULL)
	{
		fprintf(stderr, "%s: can't malloc the private key\n", strerror(errno));
		goto safe_close;
	}

	bytes_read = fread(private_key,
			   sizeof(uint8_t),
			   private_key_length,
			   fd);
	if (bytes_read != private_key_length)
	{
		fprintf(stderr, "%s: can't read the private key\n", strerror(errno));
		goto safe_close;
	}

	/* Now copy all the data to the buffer */
	ret = sizeof(type) + sizeof(public_key_length) + public_key_length +
		sizeof(private_key_length) + private_key_length;

	/* Let's not forget that there is 4 bytes for the total size
	   and 1 already-malloced byte for the command.
	 */
	*buffer = realloc(*buffer, sizeof(uint8_t) * (1 + ret + sizeof(ret)));
	if (*buffer == NULL)
	{
		fprintf(stderr, "%s: can't malloc buffer\n", strerror(errno));
		exit(EXIT_FAILURE);
	}

	/* data length */
	for (size_t i = 0; i < sizeof(ret); i++)
	{
		(*buffer)[i + offset] = (&ret)[i];
	}

	offset = offset + sizeof(ret);

	/* type

	   one byte so no need for pointer magic. I could write it as such:
	   buffer[offset] = (&type)[0];
	 */
	(*buffer)[offset] = type;

	offset = offset + sizeof(type);

	/* public key length */
	for (size_t i = 0; i < sizeof(public_key_length); i++)
	{
		(*buffer)[i + offset] = (&public_key_length)[i];
	}

	offset = offset + sizeof(public_key_length);

	/* public key */
	for (size_t i = 0; i < public_key_length; i++)
	{
		(*buffer)[i + offset] = public_key[i];
	}

	offset = offset + public_key_length;

	/* private key length */
	for (size_t i = 0; i < sizeof(private_key_length); i++)
	{
		(*buffer)[i + offset] = (&private_key_length)[i];
	}

	offset = offset + sizeof(private_key_length);

	/* private key */
	for (size_t i = 0; i < private_key_length; i++)
	{
		(*buffer)[i + offset] = private_key[i];
	}

	offset = offset + private_key_length;

 safe_close:
	if (fclose(fd) != 0)
	{
		fprintf(stderr, "%s: can't close %s\n", strerror(errno), path);
		return false;
	}

	return ret + sizeof(ret);
}

static int globerr(const char *path, int eerrno)
{
	return true;
}

static bool is_block_device(const char *path)
{
	int retval;
	struct stat sb;

	retval = stat(path, &sb);
	if (retval == -1)
	{
		return false;
	}

	return S_ISBLK(sb.st_mode);
}

static bool has_hermes_fingerprint(const char *path)
{
	bool ret;
	FILE *fd;
	size_t bytes_read;
	uint8_t bytes[FINGERPRINT_LENGTH];
	const uint8_t expected_bytes[FINGERPRINT_LENGTH] = { 82, 111, 98, 105, 110 };

	fd = fopen(path, "rb");
	if (fd == NULL)
	{
		fprintf(stderr, "%s: can't read %s\n", strerror(errno), path);
		return false;
	}

	bytes_read = fread(&bytes, sizeof(uint8_t), FINGERPRINT_LENGTH, fd);
	if (bytes_read < 1)
	{
		fprintf(stderr, "%s: can't read enough bytes\n", strerror(errno));
		ret = false;
		goto safe_exit;
	}

	for (size_t i = 0; i < bytes_read; i++)
	{
		if (bytes[i] != expected_bytes[i])
		{
			ret = false;
			goto safe_exit;
		}
	}

	ret = true;
	goto safe_exit;

safe_exit:
	if (fclose(fd) != 0)
	{
		fprintf(stderr, "%s: can't close %s\n", strerror(errno), path);
		return false;
	}

	return ret;
}

static bool is_hermes_device(const char *path)
{
	if (!is_block_device(path))
	{
		return false;
	}

	if (!has_hermes_fingerprint(path))
	{
		return false;
	}

	return true;
}
