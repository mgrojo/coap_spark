#!/bin/bash

# This script can be used to validate changes in WolfSSL befoe submitting a PR
# Install WolfSSL in Debian systems with DTLS support.
# See wolfssl/INSTALL for information on installing dependencies in other systems

sudo apt-get install autoconf automake libtool

cd wolfssl
./autogen.sh
./configure --enable-dtls --enable-dtls13 --enable-psk
make
make distcheck