#!/bin/bash

function fail {
  echo $1
  exit 1
}

home_dir=$HOME
home_dir=`pwd`
ssh_dir="${home_dir}/.ssh"
private_keyfile="${ssh_dir}/github_rsa"
public_keyfile="${private_keyfile}.pub"
ssh_config="${ssh_dir}/config"

if grep "Host github.com" > /dev/null 2>/dev/null $ssh_config; then
  echo "There is already configuration for github.com in $ssh_config"
  exit 1
fi

if [ -f $private_keyfile ]; then
  echo "A github keyfile already exists at $private_keyfile!"
  exit 1
fi


mkdir -p $ssh_dir || fail "Could not create $ssh_dir."
ssh-keygen -t rsa -b 4096 -C $EMAIL -f $private_keyfile || fail "Could not create keyfile!!!!"


touch $ssh_config
echo "Host github.com" >> $ssh_config
echo "\tUser git" >> $ssh_config
echo "\tHostname github.com" >> $ssh_config
echo "\tPreferredAuthentications publickey" >> $ssh_config
echo "\tIdentifyFile $private_keyfile" >> $ssh_config

echo ""
echo ""
echo ""
echo "Now, add the public key to github:"
echo "https://github.com/settings/keys"
echo "Paste the following:"
cat $public_keyfile
echo ""
