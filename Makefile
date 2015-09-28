CC ?= gcc
FLAGS = -Wall -Werror -std=c11 -pedantic -O3
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
CLI_SOURCES := $(wildcard cli/*.lisp) $(wildcard cli/*.asd)
DAEMON_SOURCES := $(wildcard daemon/*.lisp) $(wildcard daemon/*.asd)
QL_LOCAL=$(PWD)/.quicklocal/quicklisp
LOCAL_OPTS=--noinform --noprint --disable-debugger --no-sysinit --no-userinit
QL_OPTS=--load $(QL_LOCAL)/setup.lisp

all: pam_hermes.so daemon/hermes-daemon cli/hermes

pam_hermes.so: pam_hermes.o
	$(CC) -shared -Xlinker -x -o $@ $^ -lpam

pam_hermes.o: pam_hermes.c
	$(CC) $(FLAGS) -fPIC -fno-stack-protector -c $^

daemon-deps:
	@sbcl $(LOCAL_OPTS) $(QL_OPTS) \
		--eval '(push "$(PWD)/daemon/" asdf:*central-registry*)' \
		--eval '(ql:quickload :hermes-daemon)' \
		--eval '(quit)'
	@touch $@

daemon/hermes-daemon: $(DAEMON_SOURCES) $(QL_LOCAL)/setup.lisp daemon-deps
	@sbcl $(LOCAL_OPTS) $(QL_OPTS) \
		--eval "(push \"$(PWD)/daemon/\" asdf:*central-registry*)" \
		--eval '(asdf:operate :build-op :hermes-daemon)' \
		--quit

.PHONY: clean install

clean:
	rm -rf *.o *.so daemon/hermes-daemon cli/hermes .quicklocal/ daemon-deps

cli/hermes: $(CLI_SOURCES) $(QL_LOCAL)/setup.lisp
	@sbcl $(LOCAL_OPTS) $(QL_OPTS) \
		--eval "(push \"$(PWD)/cli/\" asdf:*central-registry*)" \
		--eval '(asdf:operate :build-op :hermes)' \
		--quit

$(QL_LOCAL)/setup.lisp:
	@sbcl $(LOCAL_OPTS) \
		--load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(QL_LOCAL)" :dist-url "http://beta.quicklisp.org/dist/quicklisp/2015-08-04/distinfo.txt")' \
		--quit

install:
	mkdir -p $(DESTDIR)/usr/share/hermes
	mkdir -p $(DESTDIR)/etc/hermes
	chmod 500 $(DESTDIR)/etc/hermes
	install -c -m 644 debian/services/hermes.service $(DESTDIR)/etc/systemd/system
	install -c -m 755 cli/hermes $(DESTDIR)/usr/bin
	install -c -m 755 daemon/hermes-daemon $(DESTDIR)/usr/share/hermes
	install -c -m 644 pam_hermes.so $(DESTDIR)/lib/security
	install -c -m 644 debian/pam-configs/hermes $(DESTDIR)/usr/share/pam-configs
