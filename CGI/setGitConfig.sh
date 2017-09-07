#!/bin/bash

echo "Setting git global config username and email" >> output.txt
export HOME=/var/www
echo $HOME >> output.txt
git config --global user.name "michael"
git config --global user.email "michael@gmail.com"
git -C "/usr/lib/cgi-bin/Repos/Hwk_1" pull --all
git -C "/usr/lib/cgi-bin/Repos/Hwk_1" checkout solution
echo "Finished setting git global configs" >> output.txt

