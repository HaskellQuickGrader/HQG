#!/bin/bash

GITURL = $1
USRNAME = $2
cd /usr/local/Repos
git clone $GITURL
cd $USRNAME
#these lines will need to be input from the haskell system call so they can be generic for anyone who uses this tool
git config user.name "Preston Keel"
git config user.email pkeel85@gmail.com