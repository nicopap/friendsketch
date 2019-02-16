#!/bin/bash
touch front/Pintclone.elm
touch lobby/Main.elm
make release-frontend
for matchedfile in $(find build -name '*.js') ; do
	sed -i "s|://localhost:8080|s://friendsketch.com|g" $matchedfile
done
ssh nicola@pi 'rm -r /home/nicola/server-setup/friendsketch/*'
scp -rC build/* nicola@pi:/home/nicola/server-setup/friendsketch/
/usr/bin/rm -r build/*
cargo build --release --target armv7-unknown-linux-gnueabihf
/usr/bin/arm-linux-gnueabihf-strip -s target/armv7-unknown-linux-gnueabihf/release/friendsketch
ssh nicola@pi 'rm /home/nicola/server-setup/friendsketch-bin'
scp -C target/armv7-unknown-linux-gnueabihf/release/friendsketch nicola@pi:/home/nicola/server-setup/friendsketch-bin
ssh nicola@pi 'systemctl --user restart friendsketch.service'

