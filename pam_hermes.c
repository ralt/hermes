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

static bool import_base64_usb_keys(const char *path,
			      const char **pubkey,
			      const char **privkey)
{
	FILE *fd;
	uint8_t type_byte;
	size_t bytes_read;

	fd = fopen(path, "rb");
	if (fd == NULL) {
		fprintf(stderr, "fatal error: %s\n", strerror(errno));
		return false;
	}

	if ((fseek(fd, FINGERPRINT_LENGTH, 0)) != 0) {
		return false;
	}

	bytes_read = fread(&type_byte, sizeof(uint8_t), 1, fd);
	if (bytes_read < 1) {
		return false;
	}

	switch (type_byte) {
	case 1:
		*pubkey = "AAAAB3NzaC1yc2EAAAADAQABAAABAQCp6RnrGvGpRyy1XVr5xA40HG22Lc9mSHaRUFP8gh4ZhPgJwmhCl86j7Isi3TySrlZMqDajmHHbE59gI2gticIU7yg0gpcHd6TzlMsTFKqOTsi/HLqICMOowHxLY43pNRVee2NnOGo9eYHk5nrvqMycgFIshCLkarQk3bzewLzU8wCbk98+mqLLYvcfrsMo3zlUjLW0Z2kKBL2RpOl8mPoF2Mipd/dtT4Lg+ShAPnmhJ++ZrWOI8ZOP/cv/6iT6NV49LJSlXdhbsZYWnaBt+TlN/TQ0Zoo0fpnXyq0mzC23TcwYBx9e0QtXT+IZpPCxtdvKt/Ld2wnh0UT5iwEmHcBp";

		*privkey = "-----BEGIN RSA PRIVATE KEY-----\n"
"MIIEogIBAAKCAQEAqekZ6xrxqUcstV1a+cQONBxtti3PZkh2kVBT/IIeGYT4CcJo\n"
"QpfOo+yLIt08kq5WTKg2o5hx2xOfYCNoLYnCFO8oNIKXB3ek85TLExSqjk7Ivxy6\n"
"iAjDqMB8S2ON6TUVXntjZzhqPXmB5OZ676jMnIBSLIQi5Gq0JN283sC81PMAm5Pf\n"
"Ppqiy2L3H67DKN85VIy1tGdpCgS9kaTpfJj6BdjIqXf3bU+C4PkoQD55oSfvma1j\n"
"iPGTj/3L/+ok+jVePSyUpV3YW7GWFp2gbfk5Tf00NGaKNH6Z18qtJswtt03MGAcf\n"
"XtELV0/iGaTwsbXbyrfy3dsJ4dFE+YsBJh3AaQIDAQABAoIBAAWfkl0GmkLo+SDT\n"
"gyRLCdFNs4/Y+kk/UrVCfFUaFVbw4KiqB3tUvOEN/hjcS5nrLS4CTzSg4fvHLXoo\n"
"EdWX/pVkyObb/5WerxXkscfi4jYtg1VX5RCFgbw/Cp4QIG08dYWX/dU3t8RrFhJ2\n"
"UlFGO+deE9onUWRP58BkmlCg/l7p0h4KtcJOBno2rbCPLaZxg9+twBTxF2V9V6P6\n"
"yu/68/ruyGTQG44+fcBoLsLtFVtyGXeySS/Pu/SWqCTQK5II6z70c9y24ADGeWKa\n"
"NqrBc9ndTQfgyE6weRn0F75KBdhaz+h/8k6sjNnV37IFmM02x1qYKns5IvQTtpWV\n"
"t7/L+dECgYEA4QA6dJKHkIfteG5mxAc8B2i3nm69oPaNamwrs+IJcEUbUX+zahA4\n"
"zRnObGig8hpjwARqFIjEh5SX/owPj+SJBP9a4KWg8M7ZvPOJcRnPeJ7K6l36NpM/\n"
"N3MRVUOWOlWTCFxI4U2qF7LvzbMfDn/Fs8JNdtyt92Yz8fX7/IA19tUCgYEAwVHW\n"
"yNsTGNhFtJpIFwR3IjNmpWmkE4tpgBOZrTEOxozWGDMIX8OHx7rmYFvS2BOEsp3o\n"
"cRlTMmuv1j/EWRpWJ+vTfr/MZjvwQsMNbPE+GYVlpHYEcPZ9IxStz/5iblIwwqJi\n"
"cL7JS2iReGGmPmVygkd/uGNrXpW84JxeHZvn1UUCgYBL5cLSLddy5pcxR7R8raAP\n"
"M8C0vdBTqrd7Ta/URww+BeG4NSZ1QbGXZfwez5By+nnpfNO3x4bb4UEASYi6VjHu\n"
"MKEcJGLMuEn0lgYn07gLjS0Pr9HGdRcfAj63j0vus40chdzDu2oEAoUn0yNHxcwP\n"
"3hw5WymHyb7+AKcWvrz1XQKBgFl8CG/w+GYHtjxjLft5lau/H0RyIgDqB2vBpuEE\n"
"VFKT3oZVGQs69x785Ka12mMqpcIIVhAEKP6t2jbSTZDxH/BauQtyvKqWFCDTKOPE\n"
"x3oIxYPbHTMs5b/XUp10oxtt8CQoBmkp5wOA49VVXh6D1v13Gye+3XRq24Mc3nPW\n"
"NvoBAoGAVAYKZ7kCCL2taHPmslpIrf6yu8JAMAWQ8u5wLAZ3jmSxFJRiEa6TDJME\n"
"V0l3mzkb45hIBamB8RdaKSs1JFu+IMNm0vLmQzR1aSxY0uWY9DO8WLb6G3LKAgni\n"
"BhmXoxDfYC0sVMk1Y/fnK1Odhur/OlXKCugB4p7TrSbNSnDkAhk=\n"
"-----END RSA PRIVATE KEY-----";
	}

	return true;
}

static bool can_login(const char *path, const char *user)
{
	ssh_session sess = ssh_new();
	if (sess == NULL) {
		return false;
	}

	ssh_options_set(sess, SSH_OPTIONS_HOST, "127.0.0.1");

	int rc = ssh_connect(sess);
	if (rc != SSH_OK) {
		return false;
	}

	const char *usb_public_key;
	const char *usb_private_key;

	if ((import_base64_usb_keys(path,
				    &usb_public_key,
				    &usb_private_key)) != true) {
		return false;
	}

	ssh_key public_key;
	ssh_key private_key;

	if ((ssh_pki_import_pubkey_base64(usb_public_key,
					  SSH_KEYTYPE_RSA,
					  &public_key)) != SSH_OK) {
		return false;
	}

	if ((ssh_userauth_try_publickey(sess, user, public_key)) != SSH_AUTH_SUCCESS) {
		return false;
	}

	if ((ssh_pki_import_privkey_base64(usb_private_key,
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

static bool is_authenticated(const char *user)
{
	int retval;
	bool ret;
	glob_t files;

	retval = glob("/dev/loop7", GLOB_ERR | GLOB_NOSORT, globerr, &files);
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
