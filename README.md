# hermes

Authenticate on linux by plugging your USB stick!

Demo: https://www.youtube.com/watch?v=W7L9FrIDYgU

## Installation

### Installing hermes

*Note: for now, only a debian/ubuntu package is provided.*

Download the [debian package][0] and install it:

```
$ sudo dpkg -i hermes_1.2_amd64.deb
$ sudo apt-get -f install
```

Add yourself to the `hermes` group:

```
$ sudo usermod -a -G hermes <user>
```

### Preparing the usb stick

I recommend having 2 partitions on your usb stick: a very small one
(hermes needs 266 bytes as of now, so allocate 2048 bytes to be safe),
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
gcc, sbcl, wget, buildapp, libpam0g-dev, libfixposix-dev.

Then run:

```
# Build hermes
$ git clone https://github.com/ralt/hermes
$ cd hermes/
$ make

# Install the built files
$ sudo make install

# Configure your system
$ sudo pam-auth-update # Select "Hermes"
$ sudo addgroup hermes

# Configure your user
$ sudo usermod -a -G hermes <user>
```

If you're using systemd, you can then run the following:

```
$ sudo systemctl enable hermes
$ sudo systemctl start hermes
```

If you're not using systemd, please look at the
`debian/services/hermes.service` file and reproduce the equivalent for
your init system.

From then on, you can follow the steps in
[Preparing the usb stick][1].

## How does it work?

A string of 128 characters is put on the both usb stick and in the
user's home directory. When connection time comes, the tokens are
checked to see if they match. If they do, the user can be connected.

Every time there is a successful login, the token is regenerated, for
security reasons.

## Developer todo

- better error handling. Right now it's fprintf(stderr) or perror()
  everywhere, randomly. Make it consistent.

## Technical process

The `hermes write <device> <user>` command will generate a 128-bytes
token, and write it to both `/etc/hermes/<user>` and the device. On
the device, it will first write 5 bytes to recognize an hermes device.

When login is needed, the PAM module will send the user to the
/var/run/hermes.sock UNIX socket. The hermes-daemon service will then
look in /dev/sd* for a hermes device (starts with bytes 82, 111, 98,
105, 110). If none exists, it sends "false" to the UNIX socket. If one
exists, it compares with the token in `/etc/hermes/<user>`, and sends
either "true" or "false" back to the UNIX socket. The PAM module just
has to get the boolean result back and it can return it.

## License

The project is provided under the MIT license.


  [0]: https://github.com/ralt/hermes/releases/download/1.2/hermes_1.2_amd64.deb
  [1]: https://github.com/ralt/hermes#preparing-the-usb-stick
