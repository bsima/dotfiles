# Author: @mbriggs
# Source: https://github.com/mbriggs/.zsh/blob/master/src/bundler.zsh

BUNDLED_COMMANDS=(cap
                  capify
                  cucumber
                  foreman
                  guard
                  haml
                  html2haml
                  jekyll
                  rackup
                  rails
                  rake2thor
                  puma
                  rspec
                  sass
                  sass-convert
                  serve
                  shotgun
                  spec
                  spork
                  thin
                  thor
                  tilt
                  tt
                  unicorn
                  unicorn_rails)

is-bundler-installed()
{
  which bundle > /dev/null 2>&1
}

is-within-bundled-project()
{
  local dir="$(pwd)"
  while [ "$(dirname $dir)" != "/" ]; do
    [ -f "$dir/Gemfile" ] && return
    dir="$(dirname $dir)"
  done
  false
}

run-with-bundler()
{
  if is-bundler-installed && is-within-bundled-project; then
    bundle exec $@
  else
    $@
  fi
}

for CMD in $BUNDLED_COMMANDS; do
  alias $CMD="run-with-bundler $CMD"
done