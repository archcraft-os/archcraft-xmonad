#!/usr/bin/env bash

## Copyright (C) 2020-2023 Aditya Shakya <adi1090x@gmail.com>

## Files and Directories
DIR="$HOME/.xmonad/polybar"
SFILE="$DIR/system"
RFILE="$DIR/.system"
MFILE="$DIR/.module"

## Get system variable values for various modules
get_values() {
	CARD=$(light -L | grep 'backlight' | head -n1 | cut -d'/' -f3)
	BATTERY=$(upower -i `upower -e | grep 'BAT'` | grep 'native-path' | cut -d':' -f2 | tr -d '[:blank:]')
	ADAPTER=$(upower -i `upower -e | grep 'AC'` | grep 'native-path' | cut -d':' -f2 | tr -d '[:blank:]')
	INTERFACE=$(ip link | awk '/state UP/ {print $2}' | tr -d :)
}

## Write values to `system` file
set_values() {
	if [[ "$ADAPTER" ]]; then
		sed -i -e "s/sys_adapter = .*/sys_adapter = $ADAPTER/g" 						${SFILE}
	fi
	if [[ "$BATTERY" ]]; then
		sed -i -e "s/sys_battery = .*/sys_battery = $BATTERY/g" 						${SFILE}
	fi
	if [[ "$CARD" ]]; then
		sed -i -e "s/sys_graphics_card = .*/sys_graphics_card = $CARD/g" 				${SFILE}
	fi
	if [[ "$INTERFACE" ]]; then
		sed -i -e "s/sys_network_interface = .*/sys_network_interface = $INTERFACE/g" 	${SFILE}
	fi
}

## Launch Polybar with selected style
launch_bar() {
	CARD=$(light -L | grep 'backlight' | head -n1 | cut -d'/' -f3)
	INTERFACE=$(ip link | awk '/state UP/ {print $2}' | tr -d :)

	if [[ ! -f "$MFILE" ]]; then
		if [[ -z "$CARD" ]]; then
			sed -i -e 's/backlight/bna/g' "$DIR"/config
		elif [[ "$CARD" != *"intel_"* ]]; then
			sed -i -e 's/backlight/brightness/g' "$DIR"/config
		fi

		if [[ "$INTERFACE" == e* ]]; then
			sed -i -e 's/network/ethernet/g' "$DIR"/config
		fi
		touch "$MFILE"
	fi
		
	# Terminate already running bar instances
	killall -q polybar

	# Wait until the processes have been shut down
	while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

	# Launch the bar
	for mon in $(polybar --list-monitors | cut -d":" -f1); do
		MONITOR=$mon polybar -q main -c "$DIR"/config &
	done
}

# Execute functions
if [[ ! -f "$RFILE" ]]; then
	get_values
	set_values
	touch ${RFILE}
fi
launch_bar
