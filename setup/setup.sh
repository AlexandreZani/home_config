#!/bin/bash

# Run using
#
# curl "https://raw.githubusercontent.com/AlexandreZani/home_config/master/setup/setup.sh" -o setup.sh
# bash setup.sh

set +e

function check_needed {
  if [ "${EMAIL}" == "" ] || [ "${NAME}" == "" ]; then
    if [ "${NAME}" == "" ]; then
      NAME="Alexandre Zani"
    fi
    echo "You need to set the following variables: "
    echo "export EMAIL=\"${EMAIL}\""
    echo "export NAME=\"${NAME}\""
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

curl "https://raw.githubusercontent.com/AlexandreZani/home_config/master/setup/keygen.sh" -o keygen.sh
bash keygen.sh
END
  exit 1
fi

git clone "${remote_git_user}@${git_server}:${remote_git_repo}" ${repo_dir} || fail "Cloning failed!!!"
git submodule update --init --recursive || fail "git submodule update failed!!!"

cp -rf "${repo_dir}/." $home_dir || fail "Could not copy contents of $repo_dir!!!!"
rm -rf ${repo_dir} || fail "Could not delete ${repo_dir}"

local_conf="$home_dir/.local.conf"
mkdir -p $local_conf || fail "Could not create .local.conf!!!"

touch "${local_conf}/.tmux.conf" || fail "Could not create .tmux.conf!!!"

git config --global user.email "${EMAIL}"
git config --global user.name "${NAME}"
