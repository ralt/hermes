CC ?= gcc
FLAGS = -Wall -Werror -std=c99

all: pam_hermes.so hermes-service

pam_hermes.so: pam_hermes.o
	$(CC) -shared -Xlinker -x -o $@ $^ -lpam

pam_hermes.o: pam_hermes.c
	$(CC) $(FLAGS) -fPIC -fno-stack-protector -c $^

hermes-service: hermes-service.c
	$(CC) $(FLAGS) -o $@ $^

.PHONY: clean

clean:
	rm -f *.o *.so hermes-service
