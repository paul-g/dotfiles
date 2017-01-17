# About

My config files for vim, bash, git, tmux, i3 etc.

To install:

````
git clone https://github.com/paul-g/dotfiles.git && cd dotfiles && git submodule update --init
````

Run `install.sh` to:

1. Create dotfile simlinks (e.g. `~/.vimrc -> config/vimrc`)
2. Install useful packages if not present

Additional system tweaks for Ubuntu 16:

* To disable Ctrl-space ibus binding, run `dconf-editor` desktop -> ibus -> general -> hotkeys
* Disable touchpad from BIOS
