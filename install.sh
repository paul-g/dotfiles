#!/bin/bash

function fail() {
    echo -e "    [FAIL] $1"
}

function skip() {
    echo -e "    [SKIP] $1"
}

function ok() {
    echo -e "    [OK]  $1"
}

function skipFoundFile() {
    skip "$1 found! Skipping installation"
    skip "    Please remove $1 to update your setup."
}

# Installing some useful packages
echo "Installing some useful packages..."
packages=(alien ack-grep clang exuberant-ctags emacs24
          fakeroot gnuplot htop maven python python-pip
	  meld colordiff inkscape zsh
          pandoc subversion silversearcher-ag-el silversearcher-ag
          xmonad xmobar libghc-xmonad-contrib-dev
          texlive-full libghc-xmonad-dev suckless-tools trayer
          valgrind terminator chromium-browser xscreensaver
          python-numpy python-scipy python-matplotlib ipython
          ipython-notebook python-pandas python-sympy python-nose)

for p in "${packages[@]}"; do
    installed=$(dpkg -s "$p" 2> /dev/null)
    if [ $? -eq 1 ]; then
        echo -e "\t'$p' package not found! Installing..."
        apt-get install $p -y
    else
        ok "$p package found"
    fi
done

# Installing some python packages
echo "\nInstalling python packages..."
packages=(pylint scipy elpy rope jedi epc)
for p in "${packages[@]}"; do
    pip install "$p"
done

# Check for existing files/simlinks
echo -e "\nInstalling Emacs 24 setup..."
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
    ln -s $(readlink -f lib/prelude) ~/.emacs.d
fi


# Install xmonad
echo -e "\nInstalling xmonad..."
if [ -a $(readlink -f ~/.xmonad) ]; then
    skipFoundFile "~/.xmonad"
else
    ln -s $(readlink -f xmonad) ~/.xmonad
    ln -s $(readlink -f xmonad/bin/xmonad.start) /usr/bin/xmonad.start
fi


# Install Git settings
echo -e "\nInstalling git config..."
if [ -a $(readlink -f ~/.gitconfig) ]; then
    skipFoundFile "~/.gitconfig"
else
    ln -s $(readlink -f config/gitconfig) ~/.gitconfig
fi

echo -e "\nInstalling tmux config..."
if [ -a $(readlink -f ~/.tmux.conf) ]; then
    skipFoundFile "~/.tmux.conf"
else
    ln -s $(readlink -f config/tmux.conf) ~/.tmux.conf
fi

echo -e "\nInstalling zsh..."
sh lib/oh-my-zsh/tools/install.sh
