#!/bin/bash

# Prompt Colours
RED='\033[0;31m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
ORANGE='\033[0;33m'
NC='\033[0m' # No Color

function fail() {
    printf "${RED}    [FAIL]${NC} $1\n"
}

function skip() {
    printf "${ORANGE}    [SKIP]${NC} $1\n"
}

function ok() {
    printf "${GREEN}    [OK]${NC}  $1\n"
}

function skipFoundFile() {
    skip "$1 found! Use '-f' to overwrite"
}

function install() {
  file=$1
  flag=$2
  dotfile=~/.${file}
  configfile=$(readlink -f "config/${file}")

  printf "${CYAN}Installing ${dotfile}${NC}\n"

  if [ "${flag}" == "1" ]; then
    rm -f "${dotfile}"
  fi

  if [ -a $(readlink -f ${dotfile}) ]; then
    skipFoundFile "${dotfile}"
    return
  fi

  ln -s ${configfile} ${dotfile}
  path=$(file ${dotfile})

  ok "${path}"
}

# A POSIX variable
OPTIND=1         # Reset in case getopts has been used previously in the shell.

install_packages=0
force_install=0

while getopts "hfp" opt; do
    case "$opt" in
    p)  install_packages=1
        ;;
    f)  force_install=1
        ;;
    esac
done

if [ "${install_packages}" == "1" ]
then
  # Installing some useful packages
  echo "Installing some useful packages..."
  packages=(ack-grep clang exuberant-ctags emacs24
            fakeroot gnuplot htop maven python python-pip
            meld colordiff pandoc i3
	    valgrind terminator chromium-browser xscreensaver)

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
fi

#install "vimrc" "${force_install}"
#install "ideavimrc" "${force_install}"
install "tmux.conf" "${force_install}"
install "gitconfig" "${force_install}"
install "dotfiles" "${force_install}"
#install "inputrc" "${force_install}"

#install "spacemacs" "${force_install}"
#install "i3status.conf" "${force_install}"

#mkdir "$HOME/.i3" -p && ln -s $(readlink -f config/i3config) ~/.i3/config
#ln -s $(readlink -f config/emacs.d/) ~/.emacs.d
