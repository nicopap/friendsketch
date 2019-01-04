#!/bin/bash
make frontend
scp -rC build/* nicola@pi:/home/nicola/friendk/static
cargo build --release --target armv7-unknown-linux-gnueabihf
/usr/bin/arm-linux-gnueabihf-strip -s target/armv7-unknown-linux-gnueabihf/release/friendsketch
ssh nicola@pi 'pkill friendsketch'
scp -C target/armv7-unknown-linux-gnueabihf/release/friendsketch nicola@pi:/home/nicola/friendk/
ssh nicola@pi 'source /home/nicola/deploy-front.sh'
