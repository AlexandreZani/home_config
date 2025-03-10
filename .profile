# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export TERMINAL="terminator"
export SUDO_ASKPASS="$HOME/bin/dpass"
LOCAL_PROFILE="$HOME/.local.conf/.profile"

if [ -f $LOCAL_PROFILE ]
then
  . $LOCAL_PROFILE
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

dropbox_script="${HOME}/bin/dropbox"
if [ -f "${dropbox_script}" ]
then
  if ${dropbox_script} running
  then
    ${dropbox_script} start 2> /dev/null
  fi
fi
. "$HOME/.cargo/env"
