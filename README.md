# hermes

Authenticate on linux by plugging your USB stick!

Demo: https://www.youtube.com/watch?v=W7L9FrIDYgU

## howto

**/!\ WARNING /!\** The next step will delete **ALL PARTITIONS AND DATA** of the usb key.

- run ./scripts/write-test-device.sh on your usb stick (err, developer
  help needed if you can't read the script.)
- add your user to the "hermes" group
- see the debian/services/hermes.service sample to see how to start
  the hermes service
- when you want to login, plug in your usb stick, hit enter, yay!

## todo

- read a fat32 filesystem instead of raw block devices
- better error handling. Right now it's fprintf(stderr) or perror()
  everywhere, randomly. Make it consistent.

## process

The PAM module will send one byte 00000001 to the /var/run/hermes.sock
UNIX socket. The hermes-service daemon will understand that this means
"get me the token from the hermes device". It will look in /dev/* for
a hermes device (starts with bytes 82, 111, 98, 105, 110). If none
exists, it sends "0" to the UNIX socket. If one exists, it sends "1",
followed by the 128-bytes token. The PAM module can then compare it
with the token in $HOME/.hermes.
