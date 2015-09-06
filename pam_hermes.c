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
#include <libssh/libssh.h>
#include <stdlib.h>

#define FINGERPRINT_LENGTH 5
#define RSA_PUBLIC_KEY_LENGTH 372

struct hermes_device {
	uint8_t type;
	char *public_key;
	uint32_t private_key_length;
	char *private_key;
};

static int globerr(const char*, int);
static bool is_block_device(const char*);
static bool has_hermes_fingerprint(const char*);
static bool is_hermes_device(const char*);
static bool can_login(struct hermes_device*, const char*);
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
	retval = pam_get_user(pamh, (const char**) &user, "Username: ");
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

static bool can_login(struct hermes_device *device, const char *user)
{
	ssh_session sess;
	int rc;
	ssh_key public_key, private_key;

	sess = ssh_new();
	if (sess == NULL) {
		return false;
	}

	ssh_options_set(sess, SSH_OPTIONS_HOST, "127.0.0.1");

	rc = ssh_connect(sess);
	if (rc != SSH_OK) {
		return false;
	}

	if ((ssh_pki_import_pubkey_base64(device->public_key,
					  SSH_KEYTYPE_RSA,
					  &public_key)) != SSH_OK) {
		return false;
	}

	if ((ssh_userauth_try_publickey(sess, user, public_key)) != SSH_AUTH_SUCCESS) {
		return false;
	}

	if ((ssh_pki_import_privkey_base64(device->private_key,
					   NULL,
					   NULL,
					   NULL,
					   &private_key)) != SSH_OK) {
		return false;
	}

	if ((ssh_userauth_publickey(sess, user, private_key)) != SSH_AUTH_SUCCESS) {
		return false;
	}

	ssh_key_free(public_key);
	ssh_key_free(private_key);

	ssh_disconnect(sess);
	ssh_free(sess);

	return true;
}

static bool hermes_new_device(struct hermes_device *device, char *path)
{
	FILE *fd;
	size_t bytes_read;

	fd = fopen(path, "rb");
	if (fd == NULL) {
		return false;
	}

	if ((fseek(fd, FINGERPRINT_LENGTH, 0)) != 0) {
		return false;
	}

	bytes_read = fread(&device->type, sizeof(uint8_t), 1, fd);
	if (bytes_read < 1) {
		return false;
	}

	device->public_key = malloc(sizeof(char) * RSA_PUBLIC_KEY_LENGTH);
	if (device->public_key == NULL) {
		return false;
	}

	bytes_read = fread(device->public_key,
			   sizeof(char),
			   RSA_PUBLIC_KEY_LENGTH,
			   fd);
	if (bytes_read != RSA_PUBLIC_KEY_LENGTH) {
		return false;
	}

	bytes_read = fread(&device->private_key_length, sizeof(uint32_t), 1, fd);
	if (bytes_read != 1) {
		return false;
	}

	printf("private key is %d bytes\n", device->private_key_length);
	device->private_key = malloc(sizeof(uint8_t) * device->private_key_length);
	bytes_read = fread(device->private_key,
			   sizeof(uint8_t),
			   device->private_key_length,
			   fd);
	if (bytes_read != device->private_key_length) {
		return false;
	}

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
	int retval;
	bool ret;
	glob_t files;
	struct hermes_device *device;

	retval = glob("/dev/*", GLOB_ERR | GLOB_NOSORT, globerr, &files);
	if (retval != 0) {
		return false;
	}

	for (size_t i = 0; i < files.gl_pathc; i++) {
		if (is_hermes_device(files.gl_pathv[i])) {
			device = malloc(sizeof(struct hermes_device));
			if (device == NULL) {
				break;
			}

			if ((hermes_new_device(device, files.gl_pathv[i])) != true) {
				break;
			}

			ret = can_login(device, user);
			goto free_hermes_device;
		}
	}

	ret = false;
	goto safe_exit;

 free_hermes_device:
	hermes_free_device(device);

 safe_exit:
	globfree(&files);
	return ret;
}
