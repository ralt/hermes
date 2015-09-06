#!/bin/bash

if [ ! $1 ]; then
    echo "usage: ./mount-test-device.sh [mount point]"
    echo "example: ./mount-test-device.sh /dev/loop0"
    exit 1
fi

# Remove previous dirty virtual block device
rm -f test.bin

# Create a 0-byte block device
./create-test-device.sh

# Write the key pair in the virtual block device
./write-test-device.sh

# Mount the block device to e.g. /dev/loop0
sudo ./mount-test-device.sh $1

# Make sure we can read the mounted block device
sudo chown $USER $1
