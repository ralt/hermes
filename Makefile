CC ?= gcc
FLAGS = -Wall -Werror -std=c99

all: pam_hermes.so hermes

pam_hermes.so: pam_hermes.o
	$(CC) -shared -Xlinker -x -o $@ $^ -lpam -lssh

pam_hermes.o: pam_hermes.c
	$(CC) $(FLAGS) -fPIC -fno-stack-protector -c $^

hermes: hermes.c
	$(CC) $(FLAGS) -Werror -o $@ $^

.PHONY: clean

clean:
	rm -f *.o *.so hermes
