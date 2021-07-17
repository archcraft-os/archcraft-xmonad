# Maintainer: Aditya Shakya <@adi1090x>

pkgname=archcraft-xmonad
pkgver=1.0
pkgrel=1
pkgdesc="xmonad configurations for Archcraft"
arch=('any')
license=('GPL3')
makedepends=('git')
depends=('xmonad' 'xmonad-contrib' 'xmonad-utils' 'hsetroot' 'xorg-xmessage')
conflicts=()
provides=("${pkgname}")
options=(!strip !emptydirs)
install="${pkgname}.install"

package() {
	local _config=${pkgdir}/etc/skel/.xmonad
	mkdir -p "$_config"
	cp -r ${srcdir}/${pkgname}/*		"$_config"
}
