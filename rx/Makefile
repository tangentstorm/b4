game: mineswpr.rx retroImage
	retro --with mineswpr.rx

mineswpr.rx: ../ref/mineswpr.org
	../tangle.el ../ref/mineswpr.org

retroImage: setup.rx color.rx sets.rx
	cp ../lib/retro/retroImage .
	retro --with setup.rx
