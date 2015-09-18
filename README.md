# hermes

Authenticate on linux by plugging your USB stick!

Demo: https://www.youtube.com/watch?v=W7L9FrIDYgU

## howto

```
$ hermes help
```

- add your user to the "hermes" group
- see the debian/services/hermes.service sample to see how to start
  the hermes service
- when you want to login, plug in your usb stick, hit enter, yay!

## todo

- better error handling. Right now it's fprintf(stderr) or perror()
  everywhere, randomly. Make it consistent.
- regenerate the token at every login: see if it's interesting.

## process

The PAM module will send one byte 00000001 to the /var/run/hermes.sock
UNIX socket. The hermes-service daemon will understand that this means
"get me the token from the hermes device". It will look in /dev/* for
a hermes device (starts with bytes 82, 111, 98, 105, 110). If none
exists, it sends "0" to the UNIX socket. If one exists, it sends "1",
followed by the 128-bytes token. The PAM module can then compare it
with the token in $HOME/.hermes.
