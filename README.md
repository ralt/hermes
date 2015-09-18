# hermes

Authenticate on linux by plugging your USB stick!

Demo: https://www.youtube.com/watch?v=W7L9FrIDYgU

## Installation

### Installing hermes

*Note: for now, only a debian/ubuntu package is provided.*

Download the [debian package][0] and install it:

```
$ sudo dpkg -i hermes_1.0_amd64.deb
$ sudo apt-get -f install
```

Add yourself to the `hermes` group:

```
$ sudo usermod -a -G hermes <user>
```

### Preparing the usb stick

I recommend having 2 partitions on your usb stick: a very small one
(hermes needs 133 bytes as of now, so allocate 2048 bytes to be safe),
and another partition for your data.

Let's assume that the partition for hermes is `/dev/sdb1`, and your
user is `foo`.

To make your usb stick an hermes device, run:

```
$ sudo hermes write /dev/sdb1 foo
```

You can see what `hermes` can do with `hermes help`. (Although for
now, it can only run the write command seen above.)

### Done!

You can now do exactly as in the video. When you need to login, plug
in the usb stick, hit enter with an empty password, and you should be
logged in!

## Manual installation

If you're not on debian, here are the steps to manually install
hermes.

Download the following (equivalent to your distribution) dependencies:
gcc, sbcl, wget, buildapp, libpam0g-dev.

Then run:

```
# Build hermes
$ git clone https://github.com/ralt/hermes
$ cd hermes/
$ make

# Install the built files
$ sudo install -m 755 hermes /usr/bin
$ sudo mkdir -p /usr/share/hermes
$ sudo install -m 755 hermes-service /usr/share/hermes
$ sudo install -m 644 pam_hermes.so /lib/security
$ sudo install -m 644 debian/pam-configs/hermes /usr/share/pam-configs

# Configure your system
$ sudo pam-auth-update # Select "Hermes"
$ sudo addgroup --system hermes

# Configure your user
$ sudo usermod -a -G hermes <user>
```

If you're using systemd, you can then run the following:

```
$ sudo cp debian/services/hermes.service /etc/systemd/system
$ sudo systemctl enable hermes
$ sudo systemctl start hermes
```

If you're not using systemd, please look at the
`debian/services/hermes.service` file and reproduce the equivalent for
your init system.

From then on, you can follow the steps in "Preparing the usb stick".

## Developer todo

- better error handling. Right now it's fprintf(stderr) or perror()
  everywhere, randomly. Make it consistent.
- regenerate the token at every login: see if it's interesting.

## Technical process

The `hermes write <device> <user>` command will generate a 128-bytes
token, and write it to both `/home/$USER/.hermes` and the device. On
the device, it will first write 5 bytes to recognize an hermes device.

When login is needed, the PAM module will send one byte of value "1"
to the /var/run/hermes.sock UNIX socket. The hermes-service daemon
will understand that this means "get me the token from the hermes
device". It will look in /dev/* for a hermes device (starts with bytes
82, 111, 98, 105, 110). If none exists, it sends "0" to the UNIX
socket. If one exists, it sends "1", followed by the 128-bytes
token. The PAM module can then compare it with the token in
$HOME/.hermes.


  [0]: https://github.com/ralt/hermes/releases/download/1.0/hermes_1.0_amd64.deb
