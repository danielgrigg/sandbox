#!/bin/bash

# copy partition layout (buggy?)

sgdisk -R=/dev/sdb /dev/sda
sgdisk -G /dev/sdb
