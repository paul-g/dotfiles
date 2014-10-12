# About

My configs and customisations for:

1. Emacs - [my fork](https://github.com/paul-g/prelude) of [prelude](https://github.com/tmalsburg/tango-plus-theme) with:
   1. python (with [elpy](https://github.com/jorgenschaefer/elpy))
   2. latex and markdown editing
   3. reference management (with [helm-bibtex](https://github.com/tmalsburg/helm-bibtex))
   4. slightly tweaked [tango+ theme](https://github.com/tmalsburg/tango-plus-theme)
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
