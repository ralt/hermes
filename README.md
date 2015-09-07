# hermes

Authenticate on linux by plugging your USB stick!


## How to

- run ./scripts/write-test-device.sh on your usb stick (err, developer
  help needed if you can't read the script.)
- install openssh-server
- run ssh-copy-id to your local user
- disable UsePAM in your /etc/ssh/sshd_config
- add yourself to the `disk` group (to be able to read /dev/sd*, I have to write a custom daemon to handle this better.)
- when you want to login, plug in your usb stick, hit enter, yay!
