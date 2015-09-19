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
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <arpa/inet.h>
#include <pwd.h>

#define COMMAND_GET_TOKEN 1

#define RET_NO_HERMES_DEVICE 0
#define RET_HERMES_DEVICE_FOUND 1

#define TOKEN_LENGTH 128

static bool can_login(const uint8_t*, const char*);
static bool is_authenticated(const char*);
static bool timing_safe_compare(const char*, const size_t, const char*, const size_t);

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

static bool timing_safe_compare(const char *known,
			 const size_t known_len,
			 const char *unknown,
			 const size_t unknown_len) {
	/* Safe since all strings **will** be null terminated */
	const size_t mod_len = known_len + 1;
	int result = 0;

	result = known_len - unknown_len;
	for (size_t i = 0; i < unknown_len; i++) {
		result |= known[i % mod_len] ^ unknown[i];
	}

	return result == 0 ? true : false;
}

static bool can_login(const uint8_t *token, const char *user)
{
	FILE *fd;
	size_t bytes_read;
	bool retval = false;

	char *local_token_path;
	const char *suffix = "/.hermes";

	struct passwd *pwd = getpwnam(user);
	if (pwd == NULL)
	{
		perror("getpwnam");
		return false;
	}

	char local_token[TOKEN_LENGTH + 1];

	local_token_path = malloc(sizeof(uint8_t) *
				  (strlen(pwd->pw_dir) + strlen(suffix) + 1));
	if (local_token_path == NULL)
	{
		perror("malloc local_token_path");
		return false;
	}

	memcpy(local_token_path, pwd->pw_dir, strlen(pwd->pw_dir));
	memcpy(local_token_path + strlen(pwd->pw_dir), suffix, strlen(suffix));

	fd = fopen(local_token_path, "rb");
	if (fd == NULL)
	{
		perror("open local_token_path");
		return false;
	}

	free(local_token_path);

	bytes_read = fread(local_token, sizeof(uint8_t), TOKEN_LENGTH, fd);
	if (bytes_read != TOKEN_LENGTH)
	{
		perror("read local_token");
		goto safe_close;
	}

	retval = timing_safe_compare(local_token,
				     TOKEN_LENGTH,
				     (const char*) token,
				     TOKEN_LENGTH);

 safe_close:
	if (fclose(fd) != 0)
	{
		perror("close local_token_path");
		return false;
	}

	return retval;
}

static bool is_authenticated(const char *user)
{
	const char *socket_path = "/var/run/hermes.sock";
	struct sockaddr_un addr;
	int fd, rc;
	uint32_t command = COMMAND_GET_TOKEN;
	uint32_t data_length = 0;
	bool retval;
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

	if ((rc = read(fd, &data_length, sizeof(data_length))) != sizeof(data_length))
	{
		perror("read data_length");
		return false;
	}

	data_length = ntohl(data_length);

	buffer = malloc(sizeof(uint8_t) * data_length);
	if (buffer == NULL)
	{
		perror("malloc buffer");
		return false;
	}

	if ((rc = read(fd, buffer, data_length)) != data_length)
	{
		perror("read buffer");
		goto error_exit;
	}

	if (buffer[0] != RET_NO_HERMES_DEVICE && buffer[0] != RET_HERMES_DEVICE_FOUND)
	{
		fprintf(stderr, "Read returned unexpected value\n");
		goto error_exit;
	}

	if (buffer[0] == RET_NO_HERMES_DEVICE)
	{
		fprintf(stderr, "No hermes device was found\n");
		goto error_exit;
	}

	if (buffer[0] == RET_HERMES_DEVICE_FOUND)
	{
		retval = can_login(buffer + 1, user);
	}

	goto safe_exit;

 error_exit:
	retval = false;

 safe_exit:
	free(buffer);

	if (close(fd) != 0)
	{
		perror("close socket");
	}

	return retval;
}
