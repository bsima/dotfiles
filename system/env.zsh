export EDITOR='emacs'

# Add nix to my shell environment
if [ -e /home/bsima/.nix-profile/etc/profile.d/nix.sh ]; then . /home/bsima/.nix-profile/etc/profile.d/nix.sh; fi


SHELLY_HOME=/Users/bsima/.shelly; [ -s "$SHELLY_HOME/lib/shelly/init.sh" ] && . "$SHELLY_HOME/lib/shelly/init.sh"