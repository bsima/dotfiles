#! /bin/zsh

nix-install(){ nix-env -iA $1; }
nix-search(){ echo "Searching..."; nix-env -qaP --description "$1"; }

function zsh-stats() {
  history | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n20
}


function take() {
  mkdir -p $1
  cd $1
}

function speed-test() {
  wget -O /dev/null http://speedtest.wdc01.softlayer.com/downloads/test10.zip
}
