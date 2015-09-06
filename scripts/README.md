# scripts

A bunch of scripts to have a virtual block device and a key pair to
run some tests.

Basically, just run `./runme.sh /dev/loop0` and it should mount a
custom block device on `/dev/loop0`, which will let you test.

You're encouraged to look at the `runme.sh` script, since it's fairly
small, ~5 relevant lines.
