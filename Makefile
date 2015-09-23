CC ?= gcc
FLAGS = -Wall -Werror -std=c11 -pedantic -O3
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
CLI_SOURCES := $(wildcard cli/*.lisp) $(wildcard cli/*.asd)
DAEMON_SOURCES := $(wildcard daemon/*.lisp) $(wildcard daemon/*.asd)
QL_LOCAL=$(PWD)/.quicklocal/quicklisp
LOCAL_OPTS=--noinform --noprint --disable-debugger --no-sysinit --no-userinit
QL_OPTS=--load $(QL_LOCAL)/setup.lisp

all: pam_hermes.so hermes-daemon hermes

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

hermes-daemon: $(DAEMON_SOURCES) $(QL_LOCAL)/setup.lisp daemon-deps
	@buildapp \
		--asdf-tree $(QL_LOCAL)/local-projects \
		--asdf-tree $(QL_LOCAL)/dists \
		--asdf-path daemon/ \
		--load-system hermes-daemon \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' \
		--compress-core \
		--output hermes-daemon --entry hermes-daemon:main

.PHONY: clean install

clean:
	rm -rf *.o *.so hermes-daemon hermes .quicklocal/ daemon-deps

hermes: $(CLI_SOURCES) $(QL_LOCAL)/setup.lisp
	@buildapp \
		--asdf-tree $(QL_LOCAL)/local-projects \
		--asdf-tree $(QL_LOCAL)/dists \
		--asdf-path cli/ \
		--load-system hermes \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' \
		--compress-core \
		--output hermes --entry hermes:main

$(QL_LOCAL)/setup.lisp:
	@sbcl --noinform --noprint --disable-debugger --no-sysinit --no-userinit \
		--load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(QL_LOCAL)" :dist-url "http://beta.quicklisp.org/dist/quicklisp/2015-08-04/distinfo.txt")' \
		--eval '(quit)'

install:
	mkdir -p $(DESTDIR)/usr/share/hermes
	mkdir -p $(DESTDIR)/etc/hermes
	chmod 500 $(DESTDIR)/etc/hermes
	install -c -m 644 debian/services/hermes.service $(DESTDIR)/etc/systemd/system
	install -c -m 755 hermes $(DESTDIR)/usr/bin
	install -c -m 755 hermes-daemon $(DESTDIR)/usr/share/hermes
	install -c -m 644 pam_hermes.so $(DESTDIR)/lib/security
	install -c -m 644 debian/pam-configs/hermes $(DESTDIR)/usr/share/pam-configs
