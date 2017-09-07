#!/bin/bash

echo "Setting git global config username and email" >> output.txt
echo $HOME >> output.txt
git config --global user.name "michael"
git config --global user.email "michael@gmail.com"
echo "Finished setting git global configs" >> output.txt

