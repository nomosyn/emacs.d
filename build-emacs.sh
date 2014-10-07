#!/usr/local/env bash

git clone https://github.com/mirrors/emacs.git
cd emacs
./autogen.sh
./configure --build=i686-apple-darwin --with-ns
make && make install
echo "check a dir called 'nextstep', you should find Emacs.app inside"
