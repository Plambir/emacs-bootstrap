.PHONY: all install

all:

update:
	cp ~/.emacs emacs.el
	cp ~/.emacs.d/init.el emacs.d/init.el
	cp ~/.emacs.d/customize.el emacs.d/customize.el
	cp ~/.emacs.d/ext.el emacs.d/ext.el
	cp -R ~/.emacs.d/snippets emacs.d/

install: all
	mkdir -p ~/.emacs.d
	cp emacs.el ~/.emacs
	cp emacs.d/init.el ~/.emacs.d/
	cp emacs.d/customize.el ~/.emacs.d/
	cp emacs.d/ext.el ~/.emacs.d/
	cp -R emacs.d/snippets ~/.emacs.d/
	rm -rf ~/.emacs.d/elpa/apm-*
	touch ~/.emacs.d/local.el
	emacs -l install.el
