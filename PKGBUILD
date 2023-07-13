# Maintainer: Aditya Shakya <adi1090x@gmail.com>

pkgname=archcraft-xmonad
pkgver=2.0
pkgrel=4
pkgdesc="Xmonad Configurations for Archcraft"
url="https://github.com/archcraft-os/archcraft-xmonad"
arch=('any')
license=('GPL3')
makedepends=('git')
depends=('xmonad' 'xmonad-contrib' 'xmonad-utils' 'hsetroot' 'xorg-xmessage'
		'alacritty' 'thunar' 'geany'
		'rofi' 'polybar' 'dunst'
		'mpd' 'mpc'
		'maim' 'xclip' 'viewnior'
		'ksuperkey' 
		'betterlockscreen'
		'xfce4-power-manager' 
		'xsettingsd'
		'xorg-xsetroot'
		'wmname'
		'pulsemixer' 'light' 'xcolor'
)
conflicts=()
provides=("${pkgname}")
options=(!strip !emptydirs)
install="${pkgname}.install"

prepare() {
	cp -af ../files/. ${srcdir}
}

package() {
	local _config=${pkgdir}/etc/skel/.xmonad
	mkdir -p "$_config"

	# Copy xmonad config files
	cp -r ${srcdir}/alacritty 		"$_config"
	cp -r ${srcdir}/scripts 		"$_config"
	cp -r ${srcdir}/theme 			"$_config"

	chmod +x "$_config"/scripts/*
	chmod +x "$_config"/theme/polybar.sh
	chmod +x "$_config"/theme/polybar/launch.sh
	chmod +x "$_config"/theme/polybar/scripts/bluetooth.sh

	install -Dm 644 dunstrc   				"$_config"/dunstrc
	install -Dm 644 picom.conf   			"$_config"/picom.conf
	install -Dm 644 picom-ibhagwan.conf   	"$_config"/picom-ibhagwan.conf
	install -Dm 644 picom-jonaburg.conf   	"$_config"/picom-jonaburg.conf
	install -Dm 644 picom-original.conf   	"$_config"/picom-original.conf
	install -Dm 644 xmonad.hs   			"$_config"/xmonad.hs
	install -Dm 644 xsettingsd   			"$_config"/xsettingsd

	# Installing pacman hooks
	install -Dm 644 recompile-xmonad.hook      	${pkgdir}/usr/share/libalpm/hooks/recompile-xmonad.hook
	install -Dm 644 recompile-xmonadh.hook      ${pkgdir}/usr/share/libalpm/hooks/recompile-xmonadh.hook
}
