CC ?= gcc
FLAGS = -Wall -Werror -std=c99
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
CLI_SOURCES := $(wildcard cli/*.lisp) $(wildcard cli/*.asd)
QL_LOCAL=$(PWD)/.quicklocal/quicklisp

all: pam_hermes.so hermes-service hermes

pam_hermes.so: pam_hermes.o
	$(CC) -shared -Xlinker -x -o $@ $^ -lpam

pam_hermes.o: pam_hermes.c
	$(CC) $(FLAGS) -fPIC -fno-stack-protector -c $^

hermes-service: hermes-service.c
	$(CC) $(FLAGS) -o $@ $^

.PHONY: clean install

clean:
	rm -rf *.o *.so hermes-service hermes .quicklocal/ quicklisp.lisp

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
	@wget http://beta.quicklisp.org/quicklisp.lisp
	@sbcl --noinform --noprint --disable-debugger --no-sysinit --no-userinit \
		--load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(QL_LOCAL)")' \
		--eval '(quit)'

install:
	install -c -m 644 debian/services/hermes.service $(DESTDIR)/etc/systemd/system
	install -c -m 755 hermes $(DESTDIR)/usr/bin
	install -c -m 755 hermes-service $(DESTDIR)/usr/share/hermes
	install -c -m 644 pam_hermes.so $(DESTDIR)/lib/security
	install -c -m 644 debian/pam-configs/hermes $(DESTDIR)/usr/share/pam-configs
