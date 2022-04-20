game: mineswpr.rx retroImage
	retro --with mineswpr.rx

mineswpr.rx: ../web/mineswpr.org
	../tangle.el ../web/mineswpr.org

retroImage: setup.rx color.rx sets.rx ng.rx
	cp ../lib/retro/retroImage .
	retro --with setup.rx
