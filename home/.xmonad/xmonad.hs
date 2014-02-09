import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

{-
myKeys = \c -> mkKeymap c $ [
        ("<XF86AudioRaiseVolume>", spawn "aumix2pipe +10"),
        ("<XF86AudioLowerVolume>", spawn "aumix2pipe +10"),
        ("<XF86AudioMute>", spawn "aumix2pipe +10")
]
-}

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/matt/.xmobarrc"
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
