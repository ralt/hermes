CC ?= gcc
FLAGS = -Wall -Werror -std=c99
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
CLI_SOURCES := $(wildcard cli/*.lisp) $(wildcard cli/*.asd)

all: pam_hermes.so hermes-service hermes

pam_hermes.so: pam_hermes.o
	$(CC) -shared -Xlinker -x -o $@ $^ -lpam

pam_hermes.o: pam_hermes.c
	$(CC) $(FLAGS) -fPIC -fno-stack-protector -c $^

hermes-service: hermes-service.c
	$(CC) $(FLAGS) -o $@ $^

.PHONY: clean

clean:
	rm -f *.o *.so hermes-service

hermes: quicklisp-manifest.txt $(CLI_SOURCES)
	@buildapp \
		--manifest-file quicklisp-manifest.txt \
		--asdf-path cli/ \
		--load-system hermes \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' \
		--compress-core \
		--output hermes --entry hermes:main

quicklisp-manifest.txt:
	@sbcl --non-interactive \
		--eval '(push #P"$(ROOT_DIR)/cli/" asdf:*central-registry*)' \
		--eval '(ql:quickload :hermes)' \
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'
