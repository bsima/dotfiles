#!/usr/env zsh
# grc overides for ls
#   Made possible through contributions from generous benefactors like
#   `brew install coreutils`
if $(gls &>/dev/null)
then
  alias ls="gls -F --color"
  alias l="gls -lAh --color"
  alias ll="gls -l --color"
  alias la='gls -A --color'
fi


alias ss='./script/server'
alias sc='./script/console'
alias tag="ctags -R config -R app -R lib -R script -R spec"
alias tag!="ctags -R ."
alias be="bundle exec"

alias easy_off='sudo kextunload -v /System/Library/Extensions/EasyTetherUSBEthernet.kext'

alias c='bundle exec cucumber'
alias s='bundle exec rspec'
alias myip="ifconfig | grep 'inet ' | grep -v 127.0.0.1 | 
   cut -d\   -f2"

alias last_migration="ls db/migrate | tail -n1 | head -c 14"

# reloads passenger and pow
function restart! () {
  touch tmp/restart.txt
}

# better than rm -rf
function trash () {
  mv $* ~/.Trash
}

# =============
# Workding dir
# =============
# 
# `setproj` sets to the current working directory to
#           the project directory variable
# `cdproj`  changes to the project directory

function cdproj {
  export wdir=`cat $HOME/.dotfiles/bin/config/current_project_path`
  cd $wdir
}

function setproj {
  pwd > ~/.dotfiles/bin/config/current_project_path
}
#cdefault

# =============
# Fun Shit
# =============

function :w () {
  echo "Ugh. You're not in vim, and your shits all retarded"
}

function internet {
  # count 3 packets
  # timeout 3 seconds
  # /dev/null unix devices that doesn't go anywhere
  # 1 is stdout, 2 is stderr, 2 follow 1
  if (ping -c 3 -t 3 google.com > /dev/null 2>&1)
  then
    echo 'yep'
  else
    echo 'nope'
  fi
}
