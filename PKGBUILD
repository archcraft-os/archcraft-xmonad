# Maintainer: Aditya Shakya <adi1090x@gmail.com>

pkgname=archcraft-xmonad
pkgver=1.0
pkgrel=1
pkgdesc="Xmonad Configurations for Archcraft"
url="https://github.com/archcraft-os/archcraft-xmonad"
arch=('any')
license=('GPL3')
makedepends=('git')
depends=('xmonad' 'xmonad-contrib' 'xmonad-utils' 'hsetroot' 'xorg-xmessage')
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

	# Copy i3wm config files
	cp -r ${srcdir}/alacritty 		"$_config"
	cp -r ${srcdir}/bin 			"$_config"
	cp -r ${srcdir}/polybar 		"$_config"
	cp -r ${srcdir}/rofi 			"$_config"

	chmod +x "$_config"/bin/*
	chmod +x "$_config"/rofi/bin/*

	install -Dm 644 xmonad.hs   	"$_config"/xmonad.hs
	install -Dm 644 picom.conf   	"$_config"/picom.conf
	install -Dm 644 wallpaper.png   "$_config"/wallpaper.png
}
