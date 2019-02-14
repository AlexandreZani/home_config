#!/bin/bash

# Run using
#
# curl -s "https://raw.githubusercontent.com/AlexandreZani/home_config/master/setup/setup.sh" | bash

set +e

function check_needed {
  if [ "${EMAIL}" == "" ] || [ "${NAME}" == "" ]; then
    echo "You need to set the following variables: "
    echo "export EMAIL=${EMAIL}"
    echo "export NAME=${NAME}"
    exit 1
  fi
}

function fail {
  echo $1
  exit 1
}

check_needed

#home_dir=$HOME
home_dir=`pwd`
repo_dir="${home_dir}/home_config"

if [ -f $repo_dir ] || [ -d $repo_dir ]; then
  echo "Delete $repo_dir and run again."
  exit 1
fi

remote_git_user="git"
git_server="github.com"
remote_git_repo="AlexandreZani/home_config.git"

ssh -T "${remote_git_user}@${git_server}" > /dev/null 2>/dev/null
if [ $? == 255 ]; then
  cat <<END
You might need to generate some new keys:

Run:

curl -s "https://raw.githubusercontent.com/AlexandreZani/home_config/master/setup/keygen.sh" | bash
END
  exit 1
fi

git clone "${remote_git_user}@${git_server}:${remote_git_repo}" ${repo_dir} || exit 1

cp -rf "${repo_dir}/." $home_dir || fail "Could not copy contents of $repo_dir!!!!"
rm -rf ${repo_dir}

local_conf="$home_dir/.local.conf"
mkdir -p $local_conf || fail "Could not create .local.conf!!!"

touch "${local_conf}/.tmux.conf" || fail "Could not create .tmux.conf!!!"
