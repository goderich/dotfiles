#!/usr/bin/env bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

run fcitx
run dropbox
run unclutter
run nm-applet
# run pulseaudio --start
