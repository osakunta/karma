#!/bin/sh

set -ex

stack install
sudo /etc/init.d/supervisor stop
sudo cp ~/.local/bin/karma-server /usr/local/bin/
sudo /etc/init.d/supervisor start
