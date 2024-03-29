.PHONY: all install

all:

update:
	cp ~/.emacs.d/init.el emacs.d/init.el
	cp ~/.emacs.d/config.el emacs.d/config.el
	cp ~/.emacs.d/bitgames.el emacs.d/bitgames.el
	cp -R ~/.emacs.d/snippets emacs.d/
	cp -R ~/.emacs.d/templates emacs.d

install: all
	mkdir -p ~/.emacs.d
	cp emacs.d/init.el ~/.emacs.d/
	cp emacs.d/config.el ~/.emacs.d/
	cp emacs.d/bitgames.el ~/.emacs.d/
	cp -R emacs.d/snippets ~/.emacs.d/
	cp -R emacs.d/templates ~/.emacs.d/
	touch ~/.emacs.d/customize.el
	touch ~/.emacs.d/local.el
	mkdir -p ~/.emacs.d/local_snippets
	emacs -l install.el
	cp emacs.d/init.el ~/.emacs.d/
	rm -f ~/.emacs.d/init.el~

force_install:
	make rm_old_packages
	make install

rm_old_packages:
	find ~/.emacs.d/elpa/* -iname "*" -exec echo {} \; | grep -v 'archives' | xargs rm -rf
	rm -rf ~/.emacs.d/irony
