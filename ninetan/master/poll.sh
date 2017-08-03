#!/bin/bash

set -e -u

cd ~/github
git pull
rsync -a --delete --exclude='.git' --delete-excluded \
    ~/github/ ~/Dropbox/ICFPC2017/github/
{
  echo 'HTTP/1.0 200 OK'
  echo 'Content-Type: text/html'
  echo
  echo 'OK'
  sleep 10
} | nc -l 18080
