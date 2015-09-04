lib_LTLIBRARIES = pam_hermes.la
pam_hermes_la_LDFLAGS = -avoid-version -module -Wl
pam_hermes_la_LIBADD = @LTLIBOBJS@
pam_hermes_la_SOURCES = pam_hermes.c

AM_CFLAGS			= -Wall
AUTOMAKE_OPTIONS		= dist-bzip2 dist-xz

LIBTOOL_DEPS			= @LIBTOOL_DEPS@

libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck
