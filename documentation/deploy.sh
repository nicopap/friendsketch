#!/bin/bash
make frontend
ssh nicola@pi 'rm -r /home/nicola/server-setup/friendsketch/*'
scp -rC build/* nicola@pi:/home/nicola/server-setup/friendsketch/
cargo build --release --target armv7-unknown-linux-gnueabihf
/usr/bin/arm-linux-gnueabihf-strip -s target/armv7-unknown-linux-gnueabihf/release/friendsketch
ssh nicola@pi 'pkill friendsketch'
scp -C target/armv7-unknown-linux-gnueabihf/release/friendsketch nicola@pi:/home/nicola/server-setup/friendsketch-bin
ssh nicola@pi 'source /home/nicola/server-setup/deploy-friendsketch.sh'
