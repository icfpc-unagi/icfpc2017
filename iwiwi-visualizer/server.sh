#!/bin/sh

ruby -rwebrick -e 'Thread.start{WEBrick::HTTPServer.new(DocumentRoot:"bin",Port:"7777").start};gets'
