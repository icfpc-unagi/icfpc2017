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

# Reload systemd configs
sudo rm /etc/systemd/system/ninetan-*
sudo cp "$(dirname "${BASH_SOURCE}")"/ninetan-* /etc/systemd/system/
sudo systemctl daemon-reload

SERVICES=(
    ninetan-dropbox
    ninetan-forward-github
    ninetan-forward-ssh
    ninetan-poll
)
for service in "$(SERVICES)"; do
  sudo systemctl enable "${SERVICE}"
  sudo systemctl start "${SERVICE}"
done
