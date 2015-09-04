#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <errno.h>
#include <security/pam_modules.h>
#include <glob.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>

#define FINGERPRINT_LENGTH 5

static int globerr(const char*, int);
static bool is_block_device(const char*);
static bool has_hermes_fingerprint(const char*);
static bool is_hermes_device(const char*);
static bool can_login(const char*, const char*);
static bool is_authenticated(const char*);

PAM_EXTERN int pam_sm_setcred(pam_handle_t *pamh, int flags, int argc, const char **argv)
{
	return PAM_SUCCESS;
}

PAM_EXTERN int pam_sm_acct_mgmt(pam_handle_t *pamh, int flags, int argc, const char **argv)
{
	return PAM_SUCCESS;
}

PAM_EXTERN int pam_sm_authenticate(pam_handle_t *pamh, int flags,int argc, const char **argv)
{
	char *user;
	int retval;
	retval = pam_get_user(pamh, &user, "Username: ");
	if (retval != PAM_SUCCESS) {
		return retval;
	}
	return is_authenticated(user) ? PAM_SUCCESS : PAM_AUTH_ERR;
}

static int globerr(const char *path, int eerrno)
{
	fprintf(stderr, "%s: %s\n", path, strerror(eerrno));
	return true; /* let glob() keep going */
}

static bool is_block_device(const char *path)
{
	int retval;
	struct stat sb;

	retval = stat(path, &sb);
	if (retval == -1) {
		return false;
	}

	return (sb.st_mode & S_IFMT) == S_IFBLK;
}

static bool has_hermes_fingerprint(const char *path)
{
	bool ret;
	FILE *fd;
	size_t bytes_read;
	uint8_t bytes[FINGERPRINT_LENGTH];
	const uint8_t expected_bytes[FINGERPRINT_LENGTH] = { 82, 111, 98, 105, 110 };

	fd = fopen(path, "rb");
	if (fd == NULL) {
		fprintf(stderr, "fatal error: %s\n", strerror(errno));
		return false;
	}

	bytes_read = fread(&bytes, sizeof(uint8_t), FINGERPRINT_LENGTH, fd);
	if (bytes_read < 1) {
		fprintf(stderr, "fatal error: %s\n", strerror(errno));
		ret = false;
		goto safe_exit;
	}

	for (size_t i = 0; i < bytes_read; i++) {
		if (bytes[i] != expected_bytes[i]) {
			ret = false;
			goto safe_exit;
		}
	}

	ret = true;
	goto safe_exit;

 safe_exit:

	if (fclose(fd) != 0) {
		fprintf(stderr, "fatal error: %s\n", strerror(errno));
		return false;
	}

	return ret;
}

static bool is_hermes_device(const char *path)
{
	if (!is_block_device(path)) {
		return false;
	}

	if (!has_hermes_fingerprint(path)) {
		return false;
	}

	return true;
}

static bool can_login(const char *path, const char *user)
{
	return true;
}

static bool is_authenticated(const char *user)
{
	int retval;
	bool ret;
	glob_t files;

	retval = glob("/dev/loop3", GLOB_ERR | GLOB_NOSORT, globerr, &files);
	if (retval != 0) {
		return false;
	}

	for (size_t i = 0; i < files.gl_pathc; i++) {
		if (is_hermes_device(files.gl_pathv[i])) {
			ret = can_login(files.gl_pathv[i], user);
			goto safe_exit;
		}
	}

	ret = false;
	goto safe_exit;

 safe_exit:
	globfree(&files);
	return ret;
}
