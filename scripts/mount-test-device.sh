#!/bin/bash

if [ ! $1 ]; then
    echo "usage: ./mount-test-device.sh [mount point]"
    echo "example: ./mount-test-device.sh /dev/loop0"
    exit 1
fi

losetup $1 test.bin
