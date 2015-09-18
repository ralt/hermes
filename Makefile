CC ?= gcc
FLAGS = -Wall -Werror -std=c99
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
CLI_SOURCES := $(wildcard cli/*.lisp) $(wildcard cli/*.asd)
QUICKLISP_SCRIPT=http://beta.quicklisp.org/quicklisp.lisp
LOCAL_OPTS=--noinform --noprint --disable-debugger --no-sysinit --no-userinit
QL_OPTS=--load $(QL_LOCAL)/setup.lisp
QL_LOCAL=$(PWD)/.quicklocal/quicklisp

all: pam_hermes.so hermes-service hermes

pam_hermes.so: pam_hermes.o
	$(CC) -shared -Xlinker -x -o $@ $^ -lpam

pam_hermes.o: pam_hermes.c
	$(CC) $(FLAGS) -fPIC -fno-stack-protector -c $^

hermes-service: hermes-service.c
	$(CC) $(FLAGS) -o $@ $^

.PHONY: clean

clean:
	rm -rf *.o *.so hermes-service hermes .quicklocal/ quicklisp.lisp bin/

bin:
	@mkdir -p bin

bin/buildapp: $(QL_LOCAL)/setup.lisp bin
	@cd $(shell sbcl $(LOCAL_OPTS) $(QL_OPTS) \
				--eval '(ql:quickload :buildapp :silent t)' \
				--eval '(format t "~A~%" (asdf:system-source-directory :buildapp))' \
				--eval '(quit)') && \
	$(MAKE) DESTDIR=$(PWD) install

hermes: $(CLI_SOURCES) bin/buildapp
	@bin/buildapp \
		--asdf-tree $(QL_LOCAL)/local-projects \
		--asdf-tree $(QL_LOCAL)/dists \
		--asdf-path cli/ \
		--load-system hermes \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' \
		--compress-core \
		--output hermes --entry hermes:main

$(QL_LOCAL)/setup.lisp:
	@curl -O $(QUICKLISP_SCRIPT)
	@sbcl $(LOCAL_OPTS) \
		--load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(QL_LOCAL)")' \
		--eval '(quit)'
