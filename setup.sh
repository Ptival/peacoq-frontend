#!/usr/bin/env bash
set -euv

if [[ -f /etc/NIXOS && -z ${name:=} ]]; then
  echo "You must run: nix-shell"
  exit 1
fi

npm install
bower install

(
cd node_modules
./requirejs/bin/r.js -convert s-expression s-expression-amd
)
