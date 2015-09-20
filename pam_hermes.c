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

static bool is_authenticated(const char *user)
{
	const char *socket_path = "/var/run/hermes.sock";
	struct sockaddr_un addr;
	int fd;
	bool result;

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

	write(fd, user, strlen(user));

	if (read(fd, &result, sizeof(bool)) != sizeof(bool))
	{
		perror("read result");
		return false;
	}

	if (close(fd) != 0)
	{
		perror("close socket");
	}

	return result;
}
