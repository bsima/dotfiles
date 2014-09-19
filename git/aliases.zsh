# Use `hub` as our git wrapper:
#   http://defunkt.github.com/hub/
hub_path=$(which hub)
if (( $+commands[hub] ))
then
  alias git=$hub_path
fi

# The rest of my fun git aliases
alias gl='git pull --prune'
alias glog="git log --graph --pretty=format:'%Cred%h%Creset %an: %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gp='git push origin HEAD'
alias gd='git diff'
alias gc='git commit'
alias gca='git commit -a'
alias gco='git checkout'
alias gb='git branch'
alias gs='git status -sb' # upgrade your git if -sb breaks for you. it's fun.

# I think these two are the same... but I have to test to be sure
alias grm="git status | grep deleted | awk '{print \$3}' | xargs git rm"
alias gitrm="git rm $(git ls-files --deleted)"

alias pull="git pull"
alias ci="git commit"
alias st="git st"
alias fetch="git fetch"
alias "log"="git log"
alias push="git push"

# Two aliases for adding files. The first ignores delete files,
# the second includes them.
alias add="git add --ignore-removal"
alias ga="git add -A"

alias fx='git fetch && gitx'
alias giff='git diff | gitx'
alias gitx='gitx --all'

# I've never used this before, but I assume it is for when you finish
# working on the staging branch and want to push your code, then push
# to staging and merge staging with master
alias booya="git pull && git push && git push staging staging:master"

# This doesn't work...
function card () {
  git co `git branch -a | grep $1 | tail -n1 | sed 's/.*\///'`
}
# Neither does this...
function git-check() {
  git log --format="%H %d" | ack $@
}

# For using with the `git_diff_wrapper` script in `../bin/`
# http://technotales.wordpress.com/2009/05/17/git-diff-with-vimdiff/
function git_diff() {
  git diff --no-ext-diff -w "$@" | vim -R -
}

# It looks like this removes all branches except master...
function rmb {
  current_branch=$(git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
  if [ "$current_branch" != "master" ]; then
    echo "WARNING: You are on branch $current_branch, NOT master."
  fi
    echo "Fetching merged branches..."
  git remote prune origin
  remote_branches=$(git branch -r --merged | grep -v '/master$' | grep -v "/$current_branch$")
  local_branches=$(git branch --merged | grep -v 'master$' | grep -v "$current_branch$")
  if [ -z "$remote_branches" ] && [ -z "$local_branches" ]; then
    echo "No existing branches have been merged into $current_branch."
  else
    echo "This will remove the following branches:"
    if [ -n "$remote_branches" ]; then
      echo "$remote_branches"
    fi
    if [ -n "$local_branches" ]; then
      echo "$local_branches"
    fi
    echo -n "Continue? (y/n):"
    read choice
    echo
    if [ "$choice" = "y" ] || [ "$choice" = "Y" ]; then
      # Remove remote branches
      git push origin `git branch -r --merged | grep -v '/master$' | grep -v "/$current_branch$" | sed 's/origin\//:/g' | tr -d '\n'`
      # Remove local branches
      git branch -d `git branch --merged | grep -v 'master$' | grep -v "$current_branch$" | sed 's/origin\///g' | tr -d '\n'`
    else
      echo "No branches removed."
    fi
  fi
}
