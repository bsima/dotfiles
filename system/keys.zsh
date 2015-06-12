# Pipe my public key to my clipboard

#if [ "$(uname -s)" == "Darwin" ]; then
#   alias pubkey="more ~/.ssh/id_dsa.public | pbcopy | echo '=> Public key copied to pasteboard.'"
#else
   alias pubkey="xclip -sel clip < ~/.ssh/id_rsa.pub | echo '=> Public key copied to pasteboard.'"
#fi