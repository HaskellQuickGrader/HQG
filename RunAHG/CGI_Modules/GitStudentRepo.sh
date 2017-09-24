#!/bin/bash
REPOPATH=$1
BRANCH=$2
echo "Setting git global config username and email" >> output.txt
echo "Full repo path: $REPOPATH" >> output.txt
export HOME=/var/www
echo $HOME >> output.txt
git config --global user.name "michael"
git config --global user.email "michael@gmail.com"
git -C $REPOPATH pull --all
git -C $REPOPATH checkout $BRANCH
echo "Finished setting git global configs" >> output.txt