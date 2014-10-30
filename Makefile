.PHONY: all install

all:

update:
	cp ~/.emacs emacs.el
	cp ~/.emacs.d/init.el emacs.d/init.el
	cp ~/.emacs.d/customize.el emacs.d/customize.el
	cp ~/.emacs.d/local.el emacs.d/local.el

install: all
	mkdir -p ~/.emacs.d
	cp emacs.el ~/.emacs
	cp emacs.d/init.el ~/.emacs.d/
	cp emacs.d/customize.el ~/.emacs.d/
	cp emacs.d/local.el ~/.emacs.d/
	emacs -l install.el
