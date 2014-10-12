# About

1. Emacs (emacs/)
2. Useful scripts (bin/)
3. Xmonad (xmonad/) -- shorter version of
   [Vic Fryzel's](https://github.com/vicfryzel/xmonad-config) ir-black
   config

## Installation

````
clone <repo>
cd <path_to_checkout>
git submodule init && git submodule update
bash install.sh
````

`install.sh`:

1. Creates appropriate simlinks (e.g. `~/.emacs.el -> <path/to/checkout/emacs/emacs.el`)
2. Installs a bunch of useful packages (when not present)

## Additional Tweaks (Ubuntu 14)

* To disable Ctrl-space ibus binding, run `dconf-editor` desktop -> ibus -> general -> hotkeys
* To improve power saving, run `powertop` and set everything to good
* To change keyboard layout use `dkpg-reconfigure keyboard-configuration`
