#!/bin/bash

dd if=/dev/zero of=test.bin count 1000k
losetup /dev/loop0 test.bin
