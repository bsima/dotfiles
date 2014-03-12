#!/bin/sh
#
# Homebrew
#
# This installs some of the common dependencies needed (or at least desired)
# using Homebrew.

# Check for Homebrew
if test ! $(which brew)
then
  echo "  x You should probably install Homebrew first:"
  echo "    https://github.com/mxcl/homebrew/wiki/installation"
  exit
fi

# Install homebrew packages
#
# grc          - Generic Colouriser is yet another colouriser (written in python) for beautifying your logfiles or output of commands
#                  For the impatient - try following commands:
#                      grc netstat
#                      grc ping hostname
#                      grc tail /var/log/syslog
#                      grc ps aux
# coreutils    - Basic file, shell and text manipulation utilities of the GNU operating system
# wget         - Why doesn't OSX ship with this?
# vim          - Might as well use brew to manage my vim
# git          - Might as well use brew to manage my git, too
# ranger       - Python-based file-browser in your terminal
# tree         - A cool commandline utility for visualizing directory structures
# zsh          - Zshell, managed by brew
# ack          - Like grep, but better for programmers 
# drip         - Makes the JVM start superfast
# autojump     - A tool for jumping around dirs, I don't really know how to use it yet...
# transmission - Commandline utils for Transmission.app BitTorrent client
brew install grc coreutils spark wget vim git ranger tree zsh ack drip autojump transmission

exit 0
