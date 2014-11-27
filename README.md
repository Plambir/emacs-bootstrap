emacs-bootstrap
===============

Quick setup GNU Emacs

#Install

Use `make install`.

#Use server

Run `emacsclient -a ""` with `-c` for GUI mode and `-t` for TTY mode.
For set `EDITOR` use string `export EDITOR=emacsclient\ -a\ \"\"\ -t` in your profile.

#Setup company-clang

    (setq company-clang-arguments '("-DGAME_TARGET_PLATFORM=ANDROID"
                                    "-DGAME_IS_DEV"
                                    "-I/path/to/project/include/"
                                    "-I/path/to/android-ndk/platforms/android-9/arch-x86/usr/include/"))
