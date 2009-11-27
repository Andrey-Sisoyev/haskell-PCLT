#! /bin/sh
clear

sudo runghc Setup unregister
sudo runghc Setup configure --user
sudo runghc Setup build
sudo runghc Setup haddock
sudo runghc Setup install
