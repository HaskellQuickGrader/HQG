#!/bin/bash

GITURL = $1
echo "Setting git global config username and email" >> output.txt
export HOME=/var/www
echo $HOME >> output.txt
git config --global user.name "michael"
git config --global user.email "michael@gmail.com"
echo "Adding, committing, and pushing student grade report." >> output.txt
git -C "/usr/lib/cgi-bin/Repos/Hwk_1" add --all
git -C "/usr/lib/cgi-bin/Repos/Hwk_1" commit -a -m "Pushing student grade report"
git -C "/usr/lib/cgi-bin/Repos/Hwk_1" push $GITURL
echo "Finished pushing student grade report" >> output.txt

