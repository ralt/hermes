/*
  The service, run as root, will listen on the /var/run/hermes.sock
  socket. It is waiting for a user char* to come in, after which it
  will check if the user can login. It then sends a boolean back into
  the UNIX socket.
 */

#define _GNU_SOURCE

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
#include <arpa/inet.h>
#include <syscall.h>
#include <linux/random.h>

#define FINGERPRINT_LENGTH 5
#define TOKEN_LENGTH 128
#define MAX_USERNAME_LENGTH 33

static int globerr(const char*, int);
static bool is_block_device(const char*);
static bool has_hermes_fingerprint(const char*);
static bool is_hermes_device(const char*);
static bool can_login(char[MAX_USERNAME_LENGTH]);
static bool regenerate_token(char user[MAX_USERNAME_LENGTH]);
static bool find_hermes_device(char**);
static bool read_device_token(char*, uint8_t[TOKEN_LENGTH]);
static bool read_user_token(char user[MAX_USERNAME_LENGTH],
			    uint8_t token[TOKEN_LENGTH]);
static bool timing_safe_compare(uint8_t[TOKEN_LENGTH], uint8_t[TOKEN_LENGTH]);
static bool get_random_token(uint8_t token[TOKEN_LENGTH]);
static bool write_device_token(char *hermes_device, uint8_t token[TOKEN_LENGTH]);
static bool write_user_token(char *user, uint8_t token[TOKEN_LENGTH]);

int main(int argc, char *argv[])
{
	const char *socket_path = "/var/run/hermes.sock";
	struct sockaddr_un addr;

	/* usernames are max 32 chars + 1 for null char */
	char user[MAX_USERNAME_LENGTH];

	int fd, cl, rc;

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
		fprintf(stderr, "%s: can't chmod %s\n", strerror(errno), socket_path);
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
		fprintf(stderr, "%s: can't chown %s to hermes group\n",
			strerror(errno), socket_path);
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

		while ((rc = read(cl, &user, MAX_USERNAME_LENGTH)) > 0)
		{
			bool result = can_login(user);

			if (result)
			{
				result = regenerate_token(user);
			}

			if (write(cl, &result, sizeof(bool)) != 1)
			{
				perror("write result");
				exit(EXIT_FAILURE);
			}
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

	if (close(fd) != 0)
	{
		exit(EXIT_FAILURE);
	}

	return EXIT_SUCCESS;
}

static bool can_login(char user[MAX_USERNAME_LENGTH])
{
	bool retval = false;

	char *hermes_device;
	bool device_found = find_hermes_device(&hermes_device);
	if (!device_found)
	{
		return false;
	}

	uint8_t device_token[TOKEN_LENGTH];
	if (!read_device_token(hermes_device, device_token))
	{
		goto clean_exit;
	}

	uint8_t user_token[TOKEN_LENGTH];
	if (!read_user_token(user, user_token))
	{
		goto clean_exit;
	}

	retval = timing_safe_compare(device_token, user_token);

clean_exit:
	free(hermes_device);

	return retval;
}

static bool regenerate_token(char user[MAX_USERNAME_LENGTH])
{
	bool retval = false;

	uint8_t new_token[TOKEN_LENGTH];
	if (!get_random_token(new_token))
	{
		return false;
	}

	char *hermes_device;
	bool device_found = find_hermes_device(&hermes_device);
	if (!device_found)
	{
		return false;
	}

	if (!write_device_token(hermes_device, new_token))
	{
		goto clean_exit;
	}

	if (!write_user_token(user, new_token))
	{
		/* @TODO
		   There should be a way to recover here.
		   Because at this point, the new token is written on
		   the device, but not in the user's file. A very
		   inconsistent state. */
		goto clean_exit;
	}

	retval = true;

clean_exit:
	free(hermes_device);

	return retval;
}

static bool get_random_token(uint8_t token[TOKEN_LENGTH])
{
	if (syscall(SYS_getrandom, token, TOKEN_LENGTH, GRND_NONBLOCK) !=
	    TOKEN_LENGTH)
	{
		perror("getrandom");
		return false;
	}
	return true;
}

static bool write_device_token(char *hermes_device, uint8_t token[TOKEN_LENGTH])
{
	return true;
}

static bool write_user_token(char *user, uint8_t token[TOKEN_LENGTH])
{
	return true;
}

static bool find_hermes_device(char **hermes_device)
{
	glob_t files;
	bool device_found = false;

	if (glob("/dev/sd*", GLOB_ERR | GLOB_NOSORT, globerr, &files) != 0)
	{
		return false;
	}

	for (size_t i = 0; i < files.gl_pathc; i++)
	{
		if (is_hermes_device(files.gl_pathv[i]))
		{
			*hermes_device = malloc(strlen(files.gl_pathv[i]) *
					       sizeof(uint8_t));
			if (*hermes_device == NULL)
			{
				perror("malloc hermes_device");
				exit(EXIT_FAILURE);
			}

			memcpy(*hermes_device,
			       files.gl_pathv[i],
			       strlen(files.gl_pathv[i]) + 1);

			device_found = true;
			break;
		}
	}

	globfree(&files);

	return device_found;
}

static bool read_device_token(char *path, uint8_t token[TOKEN_LENGTH])
{
	FILE *fd;
	size_t bytes_read;
	bool retval = false;

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

	bytes_read = fread(token,
			   sizeof(uint8_t),
			   TOKEN_LENGTH,
			   fd);
	if (bytes_read != TOKEN_LENGTH)
	{
		fprintf(stderr, "%s: can't read the token\n", strerror(errno));
		goto safe_close;
	}

	retval = true;

 safe_close:
	if (fclose(fd) != 0)
	{
		fprintf(stderr, "%s: can't close %s\n", strerror(errno), path);
	}

	return retval;
}

static bool read_user_token(char user[MAX_USERNAME_LENGTH],
			    uint8_t token[TOKEN_LENGTH])
{
	const char *prefix = "/etc/hermes/";
	char user_token_path[strlen(prefix) + MAX_USERNAME_LENGTH];

	memcpy(user_token_path, prefix, strlen(prefix));
	memcpy(user_token_path + strlen(prefix), user, MAX_USERNAME_LENGTH);

	FILE *fd;
	size_t bytes_read;
	bool retval = false;

	fd = fopen(user_token_path, "rb");
	if (fd == NULL)
	{
		perror("open user_token_path");
		return false;
	}

	bytes_read = fread(token, sizeof(uint8_t), TOKEN_LENGTH, fd);
	if (bytes_read != TOKEN_LENGTH)
	{
		perror("read token");
		goto safe_close;
	}

	retval = true;

 safe_close:
	if (fclose(fd) != 0)
	{
		perror("close user_token_path");
	}

	return retval;
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

static bool timing_safe_compare(uint8_t device_token[TOKEN_LENGTH],
				uint8_t user_token[TOKEN_LENGTH])
{
	int result = 0;

	for (size_t i = 0; i < TOKEN_LENGTH; i++) {
		result |= device_token[i % TOKEN_LENGTH] ^ user_token[i];
	}

	return result == 0;
}
