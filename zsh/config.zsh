#!/bin/zsh

if [[ -n $SSH_CONNECTION ]]; then
  export PS1='%m:%3~$(git_info_for_prompt)%# '
else
  export PS1='%3~$(git_info_for_prompt)%# '
fi

export LSCOLORS="exfxcxdxbxegedabagacad"
export CLICOLOR=true

fpath=($ZSH/zsh/functions $fpath)

autoload -U $ZSH/functions/*(:t)

# ============
# HISTORY
# ============
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

setopt NO_BG_NICE # don't nice background tasks
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS # allow functions to have local options
setopt LOCAL_TRAPS # allow functions to have local traps
setopt HIST_VERIFY
setopt SHARE_HISTORY # share history between sessions ???
setopt EXTENDED_HISTORY # add timestamps to history
setopt PROMPT_SUBST
setopt CORRECT
setopt COMPLETE_IN_WORD
setopt IGNORE_EOF

setopt APPEND_HISTORY # adds history
setopt INC_APPEND_HISTORY SHARE_HISTORY  # adds history incrementally and share it across sessions
setopt HIST_IGNORE_ALL_DUPS  # don't record dupes in history
setopt HIST_REDUCE_BLANKS

# don't expand aliases _before_ completion has finished
#   like: git comm-[tab]
setopt complete_aliases

zle -N newtab

bindkey '^[^[[D' backward-word
bindkey '^[^[[C' forward-word
bindkey '^[[5D' beginning-of-line
bindkey '^[[5C' end-of-line
bindkey '^[[3~' delete-char
bindkey '^[^N' newtab
bindkey '^?' backward-delete-char

# ZSH help files (when installed with homebrew)
#unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles


## GPG Stuff
#export GPG_AGENT_INFO_FILE=$HOME/.gpg-agent-info  
#gpg-agent --daemon --enable-ssh-support --write-env-file "${GPG_AGENT_INFO_FILE}"

#if [ -f "${GPG_AGENT_INFO_FILE}" ]; then
#  . "${GPG_AGENT_INFO_FILE}"
#  export GPG_AGENT_INFO
#  export SSH_AUTH_SOCK
#  export SSH_AGENT_PID
#fi

#export GPG_TTY=$(tty)


# OCaml support
. /Users/bsima/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
