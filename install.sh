#!/bin/sh

set -ex

unset POSIXLY_CORRECT
ghc --version || $(ghc-select ghc-7.10.3)

stack install
sudo /etc/init.d/supervisor stop
sleep 1
sudo cp ~/.local/bin/ajk-lomake-server /usr/local/bin/
sudo /etc/init.d/supervisor start
