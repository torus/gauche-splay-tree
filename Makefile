anim: run
	magick 0*.png -extent '683x596' -set delay 80 anim.gif

run:
	gosh -I. test-rope.scm
