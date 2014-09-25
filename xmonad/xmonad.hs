import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig

-- xprop | grep WM_CLASS
myManageHook = composeAll
    [ className =? "Emacs"            --> doShift "3",
      className =? "Terminator"       --> doShift "1"
    , resource  =? "desktop_window"   --> doIgnore
    , className =? "Gimp"             --> doFloat
    , className =? "VirtualBox"       --> doShift "4:vm"]

--- Layouts ---
myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)

--- UI ---
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

--- Config ---
xmonadConfig = defaultConfig {
    terminal           = "/usr/bin/terminator",
    focusFollowsMouse  = True,
    borderWidth        = 1,
    modMask            = mod4Mask,
    normalBorderColor  = "#7c7c7c",
    focusedBorderColor = "#ffb6b0",
    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook
} `additionalKeysP`
  [("M-S-l", spawn "xscreensaver-command --lock"),
   ("M-p",  spawn "dmenu_run -nb 'black' -nf 'white' -fn '-*-*-*-*-*-*-15-*-*-*-*-*-*-*' ")]

--- Start xmonad ---
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
  xmonad $ xmonadConfig {
      logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor "#FFB6B0" ""
          , ppCurrent = xmobarColor "#CEFFAC" ""}
      , manageHook = manageDocks <+> myManageHook
      , startupHook = setWMName "LG3D"
  }
