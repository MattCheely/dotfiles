import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import System.IO

myKeys = [ ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2+")
         , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2-")
         , ("<XF86AudioMute>",        spawn "amixer set Master toggle")
         ]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig {
        terminal = "gnome-terminal",
        borderWidth = 3,
        normalBorderColor = "#2b2b2b",
        focusedBorderColor = "#6C6C9C",
        modMask = mod4Mask,  --Rebind Mod to the windows key

        manageHook = manageDocks <+> manageHook defaultConfig,
        layoutHook = avoidStruts  $  layoutHook defaultConfig,
        logHook = dynamicLogWithPP xmobarPP {
            ppOutput = hPutStrLn xmproc,
            ppCurrent = xmobarColor "#6c6c9c" "",
            ppTitle = xmobarColor "#6c6c9c" "" . shorten 50
        }
    }
