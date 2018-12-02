import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import System.IO

main = do
    xmproc <- spawnPipe myXmobar
    -- need to launch with docks for xmobar to be persistent
    xmonad $ docks def
      { terminal    = "alacritty"
      , modMask     = mod4Mask
      , workspaces  = myWorkspaces
      , borderWidth = 1
      , focusedBorderColor = blue
      , focusFollowsMouse  = False
      , manageHook = manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook def
      , layoutHook = smartBorders . avoidStruts  $  layoutHook def
      , logHook = dynamicLogWithPP $ xmobarPP
        { ppOutput = hPutStrLn xmproc,
          ppTitle = xmobarColor "green" "" . shorten 100
        }
      , handleEventHook = fullscreenEventHook
      }

myXmobar = "/home/iwaka/.local/bin/xmobar /home/iwaka/.xmobarrc"
myWorkspaces = ["1:term","2:web","3:code"] ++ map show [4..9]

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"
