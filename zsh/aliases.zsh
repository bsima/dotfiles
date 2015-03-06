#!/bin/zsh

alias streak="curl -s https://github.com/bsima | pup '#contributions-calendar > div:nth-child(5) > span.contrib-number text{}'"

alias reload!='. ~/.zshrc'

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Easy clear
alias clr=clear

# Super user
alias _='sudo'
alias please='sudo'
alias plz='sudo'

alias g='grep -in'

# Show history
alias history='fc -l 1'

# List direcory contents
alias lsa='ls -lah'
alias l='ls -lA1'
alias ll='ls -l'
alias la='ls -lA'
alias sl=ls # often screw this up

alias afind='ack-grep -il'

# Save my ass
alias rm='rm -i'

# Capify is so much more climactic...
alias capify="bundle exec cap install"
alias capify!="bundle exec cap install"
