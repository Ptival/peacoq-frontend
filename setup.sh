#!/usr/bin/env bash
set -euv

if [ -f /etc/NIXOS ]; then
  if [ -z ${name:=} ]; then
    echo "You must run: nix-shell"
  else
    echo "Nothing to do yet"
  fi
else
  echo "No setup for non NixOS machines yet"
fi
