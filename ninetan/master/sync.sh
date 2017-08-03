#!/bin/bash

set -e -u

rsync -a --delete --exclude='.git' --delete-excluded \
    ~/github/ ~/Dropbox/ICFPC2017/github/

if ! mountpoint /dropbox; then
  bindfs -p 0777 -u ninetan -g ninetan /home/ninetan/Dropbox/ICFPC2017 /dropbox
fi

if ! mountpoint /github; then
  mount --rbind /home/ninetan/github /github
fi
