# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

set -o vi

[[ $- == *i* ]] && stty -ixon

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Detect if we are running on WSL.
if grep -iq -- "-WSL" /proc/sys/kernel/osrelease; then
  export IS_WSL=1
fi

# If we are running on WSL, setup DISPLAY to connect to XWindows.
if [ -n "$IS_WSL" ]; then
  export DISPLAY="`grep nameserver /etc/resolv.conf | sed 's/nameserver //'`:0"
fi

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

function print_git_branch()
{
  branch=`git rev-parse --symbolic-full-name --abbrev-ref HEAD 2> /dev/null`
  branch="($branch)"
  echo $branch
}

function enable_prompt()
{
  if [ "$color_prompt" = yes ]; then
      PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] $(print_git_branch)\n\$ '
  else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h$(date +"(%y-%m-%d %H:%M:%S)"):\w $(print_git_branch)\n\$ '
  fi
  unset color_prompt force_color_prompt
}

function disable_prompt()
{
  PS1='$ '
}
enable_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    # WARNING: enabling this can cause multi-second delays due to NFS latency
    #alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
    alias cgrep='grep --color=always' 
fi

# some more ls aliases
alias sdr='screen -dr'
alias ls='ls --color'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Preserve colors when piping to less.
alias less='less -R'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

LOCAL_BASHRC=$HOME/.local.conf/.bashrc

export LOCAL_HOME=/usr/local/google/home/$USER
export PATH=$HOME/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH="${HOME}/.local/bin:${PATH}"
alias vim="vim -O"
alias vs="vim -"
alias v="vim"
alias e="vim ~/.bashrc"
alias el="vim $LOCAL_BASHRC"
alias r='source ~/.bashrc'
export EDITOR='vim'
alias lh='cd $LOCAL_HOME'
alias i='history'
alias c='clear'
alias py='python'
alias ipy='ipython'
alias :w='echo "This is not vim"'
alias :q='echo "This is not vim"'
alias gb="git branch | grep '*'"
alias de="xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'"
alias :e='vim'
alias clang-format='clang-format -i -style=google'
alias emacs='emacs --no-window-system'

# Work-related things.

alias s="grep -R -T --exclude-dir=out --exclude-dir=bazel-* --exclude-dir=.git --exclude-dir=sysroot --exclude-dir=third_party"
alias sa="grep -R -T --exclude-dir=out --exclude-dir=.git"
alias cs="s"
alias fs="find . -name"
alias review="git push origin HEAD:refs/for/master"
alias draft="git push origin HEAD:refs/drafts/master"

export PATH=$HOME/depot_tools:"$PATH"
export PATH=$HOME/.opam/default/bin:"$PATH"
export PATH=$HOME/.elan/bin:"$PATH"
export GOMA_OAUTH2_CONFIG_FILE=$HOME/.goma_oauth2_config
export GOMA_DIR=$HOME/goma
export PATH=$HOME/cobalt/sysroot/bin:$PATH
export PATH=$HOME/go/bin:$PATH

# Set touch gestures for X1 Carbon
xinput set-prop "Synaptics TM3289-021" "libinput Tapping Enabled" 1
xinput set-prop "Synaptics TM3289-021" "libinput Natural Scrolling Enabled" 1

function sr()
{
  find . -type d \( -path ./out -o -path ./.git -o -path ./sysroot \) -prune -o -type f -exec sed -i "$1" {} +
}

ROOT_DIR=$HOME/root/bin/thisroot.sh
[ -f $ROOT_DIR ] && source $ROOT_DIR

CARGO_ENV=$HOME/.cargo/env
[ -f $CARGO_ENV ] && source $CARGO_ENV

[ -f $LOCAL_BASHRC ] && source $LOCAL_BASHRC

if command -v opam &> /dev/null
then
  eval `opam env`
fi
