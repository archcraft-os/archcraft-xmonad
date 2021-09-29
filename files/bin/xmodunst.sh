#!/usr/bin/env bash

## Copyright (C) 2020-2021 Aditya Shakya <adi1090x@gmail.com>
## Everyone is permitted to copy and distribute copies of this file under GNU-GPL3

## Launch dunst daemon

BD="#E6DFE0"
UBD="#BB553F"
FG="#E6DFE0"
BG="#090A0B"

if [[ `pidof dunst` ]]; then
	pkill dunst
fi

dunst \
-geom "280x50-10+40" -frame_width "1" -font "Iosevka 10" \
-lb "$BG" -lf "$FG" -lfr "$BD" \
-nb "$BG" -nf "$FG" -nfr "$BD" \
-cb "$BG" -cf "$UBD" -cfr "$UBD" &
