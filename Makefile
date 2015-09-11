all: pam_hermes.so

pam_hermes.so: pam_hermes.o
	gcc -shared -Xlinker -x -o $@ $^ -lpam -lssh

pam_hermes.o: pam_hermes.c
	gcc -Wall -Werror -fPIC -fno-stack-protector -c $^
