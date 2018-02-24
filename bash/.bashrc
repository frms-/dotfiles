# ~/.bashrc: executed by bash(1) for non-login shells.

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="auto"

export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=1048576
export HISTFILESIZE=1048576
export HISTTIMEFORMAT="%F %T "
export PROMPT_COMMAND='history -a'
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

GIT_PS1_FORMAT="(%.10s)"
PS1='\[\033[31m\][\t]\[\033[m\][\u@\h]\[\033[32m\] \w`__git_ps1 $GIT_PS1_FORMAT`\[\033[m\]\$ '

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -t 3000 -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi


/usr/bin/mint-fortune

alias tmux="TERM=screen-256color-bce tmux"

export EDITOR=vim

man() {
  env LESS_TERMCAP_mb=$'\E[0;103m' \
  LESS_TERMCAP_md=$'\E[0;93m' \
  LESS_TERMCAP_me=$'\E[0m' \
  LESS_TERMCAP_se=$'\E[0m' \
  LESS_TERMCAP_so=$(tput bold; tput setaf 8; tput setab 3) \
  LESS_TERMCAP_ue=$'\E[0m' \
  LESS_TERMCAP_us=$'\E[04;32m' \
  LESS_TERMCAP_mr=$(tput rev) \
  LESS_TERMCAP_mh=$(tput dim) \
  LESS_TERMCAP_ZN=$(tput ssubm) \
  LESS_TERMCAP_ZV=$(tput rsubm) \
  LESS_TERMCAP_ZO=$(tput ssupm) \
  LESS_TERMCAP_ZW=$(tput rsupm) \
  man "$@"
}

SSH_AGENT_ENV=$HOME/.ssh/agent-environment
function start-ssh-agent {
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > $SSH_AGENT_ENV
    chmod 600 $SSH_AGENT_ENV
    source $SSH_AGENT_ENV > /dev/null
}

if [ -f $SSH_AGENT_ENV ] ; then
    source $SSH_AGENT_ENV > /dev/null
    ps $SSH_AGENT_PID | grep -q ssh-agent || {
	start-ssh-agent
    }
else
    start-ssh-agent
fi

if [ -f ${HOME}/.bashrc-local ]; then
    . ${HOME}/.bashrc-local
fi


############### Locale settings ##############
export LANG=en_DK.UTF-8
export LANGUAGE=en_US
export LC_CTYPE="en_DK.UTF-8"
export LC_NUMERIC=en_DK.UTF-8
export LC_TIME="en_DK.UTF-8"
export LC_COLLATE="en_DK.UTF-8"
export LC_MONETARY=sv_SE.UTF-8
export LC_MESSAGES="en_DK.UTF-8"
export LC_PAPER=en_DK.UTF-8
export LC_NAME=en_DK.UTF-8
export LC_ADDRESS=en_DK.UTF-8
export LC_TELEPHONE=en_DK.UTF-8
export LC_MEASUREMENT=en_DK.UTF-8
export LC_IDENTIFICATION=en_DK.UTF-8
export LC_ALL=

eval "$(stack --bash-completion-script stack)"

#stty -ixon
#stty -ixoff

