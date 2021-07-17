#!/usr/bin/env bash

# Kill already running process
_ps=(compton ksuperkey mpd polybar xfce-polkit xfce4-power-manager)
for _prs in "${_ps[@]}"; do
	if [[ `pidof ${_prs}` ]]; then
		killall -9 ${_prs}
	fi
done

# polkit agent
/usr/lib/xfce-polkit/xfce-polkit &

# Enable power management
xfce4-power-manager &

# Enable Super Keys For Menu
ksuperkey -e 'Super_L=Alt_L|F1' &
ksuperkey -e 'Super_R=Alt_L|F1' &

# Fix mouse cursor
xsetroot -cursor_name left_ptr

# Restore wallpaper
hsetroot -cover ~/.xmonad/wallpaper.png

# Lauch polybar
polybar main -c ~/.xmonad/polybar/config.ini &

# Lauch compositor
compton --config ~/.xmonad/compton.conf &

# Lauch notification daemon
if [[ `pidof dunst` ]]; then
	pkill dunst
fi

dunst \
-geom "280x50-10+40" -frame_width "1" -font "Iosevka 10" \
-lb "#090A0B" -lf "#E6DFE0" -lfr "#E6DFE0" \
-nb "#090A0B" -nf "#E6DFE0" -nfr "#E6DFE0" \
-cb "#090A0B" -cf "#BB553F" -cfr "#BB553F" &

# Start mpd
exec mpd &
