#!/bin/bash

set -e -u

cd ~/icfpc2017
git pull
rsync -a --delete --exclude='.git' --delete-excluded \
    ~/icfpc2017/ ~/Dropbox/ICFPC2017/github/
{
  echo 'HTTP/1.0 200 OK'
  echo 'Content-Type: text/html'
  echo
  echo 'OK'
  sleep 10
} | nc -l 18080
