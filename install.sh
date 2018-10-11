#!/bin/bash

this_dir=$(dirname $(realpath $0))

ln -sf $this_dir/tmux/tmux.conf ~/.tmux.conf
ln -sf $this_dir/emacs/init.el ~/.emacs.d/init.el
ln -sf $this_dir/emacs/lisp ~/.emacs.d/lisp
ln -sf $this_dir/emacs/elpa ~/.emacs.d/elpa
