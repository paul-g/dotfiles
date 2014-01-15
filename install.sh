#!/bin/bash

dirs=(~/.emacs.el ~/.emacs.d ~/.yasnippet-snippets)

# Check for existing files/simlinks
for dir in "${dirs[@]}"; do
    path=$(readlink -f ${dir})
    if [ -a ${path} ]; then
        echo "${path} found! Please remove this before continuing..."
        exit 1
    fi
done

# Create symlinks
ln -s $(readlink -f emacs/emacs.el) ~/.emacs.el
ln -s $(readlink -f emacs/yasnippet-snippets) ~/.yasnippet-snippets
