#!/bin/bash

set -e -u

rsync -a --delete --exclude='.git' --delete-excluded master:~/github/ ~/github/
rsync -a --delete --exclude='.git' --delete-excluded master:~/Dropbox/ICFPC2017/ ~/Dropbox/ICFPC2017/

sleep 10
