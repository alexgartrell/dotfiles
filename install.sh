#!/bin/bash

this_dir=$(cd "$(dirname "$0")" && pwd -P)

ln -sf $this_dir/tmux/tmux.conf ~/.tmux.conf
ln -sf $this_dir/emacs/init.el ~/.emacs.d/init.el
ln -sf $this_dir/emacs/lisp ~/.emacs.d/lisp
ln -sf $this_dir/emacs/elpa ~/.emacs.d/elpa
