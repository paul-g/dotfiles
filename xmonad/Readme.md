# Setting up an Xmonad Session for Gnome

1. Create a /usr/share/xsessions/xmonad.desktop:

```
[Desktop Entry]
Encoding=UTF-8
Name=XMonad
Comment=Lightweight tiling window manager
Exec=xmonad.start
Icon=xmonad.png
Type=XSession
```

2. Create a /usr/local/bin/xmonad.start (or symlink bin/xmonad.start).
