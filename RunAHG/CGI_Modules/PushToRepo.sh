#!/bin/bash

GITURL=$1
REPOFOLDER=$2
echo "Setting git global config username and email" >> output.txt
echo "Repo folder about to push from:"$REPOFOLDER >> output.txt
export HOME=/var/www
echo $HOME >> output.txt
git config --global user.name "michael"
git config --global user.email "michael@gmail.com"
echo "Adding, committing, and pushing student grade report." >> output.txt
git -C $REPOFOLDER add --all
git -C $REPOFOLDER commit -a -m "Pushing student grade report"
git -C $REPOFOLDER push $GITURL
echo "Finished pushing student grade report" >> output.txt

