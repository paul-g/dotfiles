#!/bin/bash

function fail() {
    echo -e "\t[FAIL] $1"
}

# Installing some useful packages
echo "Installing some useful packages..."
packages=(alien ack-grep clang fakeroot htop maven python python-pip
          xmonad libghc-xmonad-contrib-dev libghc-xmonad-dev suckless-tools trayer
          valgrind terminator chromium-browser xscreensaver)

for p in "${packages[@]}"; do
    installed=$(dpkg -s "$p" 2> /dev/null)
    if [ $? -eq 1 ]; then
        echo -e "\t'$p' package not found! Installing..."
        apt-get install $p -y
    else
        echo -e "\t[OK] '$p' package found"
    fi
done

# Installing some python packages
packages=(pylint scipy elpy rope)
for p in "${packages[@]}"; do
    pip install "$p"
done

# Check for existing files/simlinks
echo "Installing Emacs 24 setup..."
dirs=(~/.emacs.el ~/.emacs.d ~/.yasnippet-snippets)
installEmacsSetup=1
for dir in "${dirs[@]}"; do
    path=$(readlink -f ${dir})
    if [ -a ${path} ]; then
        fail "${path} found! Skipping Emacs setup installation."
        fail "Please remove ${path} and re-run the script to update your setup."
        installEmacsSetup=0
        break
    fi
done

if [ $installEmacsSetup -eq 1 ]; then
    echo "Creating symlinks."
    ln -s $(readlink -f emacs/emacs.el) ~/.emacs.el
    ln -s $(readlink -f emacs/yasnippet-snippets) ~/.yasnippet-snippets
fi

echo "Installing shell include..."
installBash=1
if [ -a $(readlink -f ~/.bash_include) ]; then
    fail "~/.bash_include found! Skipping installation."
    fail "Please remove ~/.bash_include to update your setup."
    installBash=0
fi

if [ $installBash -eq 1 ]; then
    ln -s $(readlink -f config/bash_include) ~/.bash_include
fi

# Install xmonad
echo "Installing xmonad..."
installXmonad=1
if [ -a $(readlink -f ~/.xmonad) ]; then
    fail "~/.xmonad found! Skipping installation"
    fail "Please remove ~/.xmonad to update your setup."
    installXmonad=0
fi

if [ $installXmonad -eq 1 ]; then
    ln -s $(readlink -f xmonad) ~/.xmonad
    ln -s $(readlink -f xmonad/bin/xmonad.start) /usr/bin/xmonad.start
fi
