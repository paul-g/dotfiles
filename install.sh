#!/bin/bash


# Check for existing files/simlinks
echo "Installing Emacs 24 setup..."
dirs=(~/.emacs.el ~/.emacs.d ~/.yasnippet-snippets)
installEmacsSetup=1
for dir in "${dirs[@]}"; do
    path=$(readlink -f ${dir})
    if [ -a ${path} ]; then
        echo -e "\t${path} found! Skipping Emacs setup installation."
        echo -e "\tPlease remove ${path} and re-run the script to update your setup."
        installEmacsSetup=0
        break
    fi
done

if [ $installEmacsSetup -eq 1 ]; then
    echo "Creating symlinks."
    ln -s $(readlink -f emacs/emacs.el) ~/.emacs.el
    ln -s $(readlink -f emacs/yasnippet-snippets) ~/.yasnippet-snippets
fi


echo "Installing some useful packages..."
packages=(maven python)

for p in "${packages[@]}"; do
    installed=$(dpkg -s "$p" 2> /dev/null)
    if [ $? -eq 1 ]; then
        echo -e "\t'$p' package not found! Installing..."
        apt-get install $p -y
    else
        echo -e "\t[OK] '$p' package found"
    fi
done
