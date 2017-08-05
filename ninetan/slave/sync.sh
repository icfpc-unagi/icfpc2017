#!/bin/bash

set -e -u

rsync -a --delete --exclude='.git' --delete-excluded master:~/github/ ~/github/
rsync -a --delete --exclude='.git' --delete-excluded master:~/Dropbox/ICFPC2017/ ~/Dropbox/ICFPC2017/
sudo mkdir -p /binary
sudo chown ninetan:ninetan /binary
rsync -a --delete master:/binary/ /binary/

if ! mountpoint /dropbox; then
  sudo mkdir -p /dropbox
  sudo chmod 0755 /dropbox
  sudo mount --rbind /home/ninetan/Dropbox/ICFPC2017 /dropbox
fi

if ! mountpoint /github; then
  sudo mkdir -p /github
  sudo chmod 0755 /github
  sudo mount --rbind /home/ninetan/github /github
fi

sleep 10
