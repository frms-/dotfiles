# ~/.bashrc: executed by bash(1) for non-login shells.
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="auto"

export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=-1
unset HISTFILESIZE

export HISTTIMEFORMAT="%F %T "
export PROMPT_COMMAND='history -a'
# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm|xterm-color|*-256color) color_prompt=yes;;
esac

unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

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


if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

if [ -x /usr/bin/mint-fortune ]; then
     /usr/bin/mint-fortune
fi

alias tmux="TERM=screen-256color-bce tmux"

export EDITOR=vim

man() {
  env LESS_TERMCAP_mb=$'\E[0;103m' \
  LESS_TERMCAP_md=$'\E[0;93m' \
  LESS_TERMCAP_me=$'\E[0m' \
  LESS_TERMCAP_se=$'\E[0m' \
  LESS_TERMCAP_so="$(tput bold; tput setaf 8; tput setab 3)" \
  LESS_TERMCAP_ue=$'\E[0m' \
  LESS_TERMCAP_us=$'\E[04;32m' \
  LESS_TERMCAP_mr="$(tput rev)" \
  LESS_TERMCAP_mh="$(tput dim)" \
  LESS_TERMCAP_ZN="$(tput ssubm)" \
  LESS_TERMCAP_ZV="$(tput rsubm)" \
  LESS_TERMCAP_ZO="$(tput ssupm)" \
  LESS_TERMCAP_ZW="$(tput rsupm)" \
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

function man_flag () { LESS=+/^[[:blank:]]+"$2" man "$1" ;}
alias mf=man_flag

############### Locale settings ##############
export LANG=en_DK.UTF-8
export LANGUAGE=en_US
export LC_MONETARY=sv_SE.UTF-8
export LC_PAPER=en_DK.UTF-8
export LC_NAME=en_DK.UTF-8
export LC_ADDRESS=en_DK.UTF-8
export LC_TELEPHONE=en_DK.UTF-8
export LC_MEASUREMENT=en_DK.UTF-8
export LC_IDENTIFICATION=en_DK.UTF-8

if hash stack 2>/dev/null; then
  eval "$(stack --bash-completion-script stack)"
fi

[ -t 0 ] && stty -ixon # make C-s search forward work

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

eval "$(direnv hook bash)"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
source "$HOME/.cargo/env"
