#!/bin/bash

set -e -u

rsync -a --delete --exclude='.git' --delete-excluded \
    ~/github/ ~/Dropbox/ICFPC2017/github/
