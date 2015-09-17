# hermes

Authenticate on linux by plugging your USB stick!


## How to

**/!\ WARNING /!\** The next step will delete **ALL PARTITIONS AND DATA** of the usb key.

- run ./scripts/write-test-device.sh on your usb stick (err, developer
  help needed if you can't read the script.)
- add your user to the "hermes" group
- see the debian/services/hermes.service sample to see how to start
  the hermes service
- when you want to login, plug in your usb stick, hit enter, yay!
