import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ScreenCorners
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ gnomeConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $ layoutHook defaultConfig
    , handleEventHook = docksEventHook <+> handleEventHook defaultConfig
    , startupHook = docksStartupHook <+> startupHook defaultConfig
    , logHook =
        dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor "green" "" . shorten 50
          }
    , modMask = mod4Mask
    } `additionalKeys`
    [
      -- Start screen saver
      ((mod4Mask .|. shiftMask, xK_z),
      spawn "xscreensaver-command -lock; xset dpms force off")

      -- Bring up dmenu
    , ((mod4Mask, xK_p),
      spawn "dmenu_run")

      -- Mute volume.
    , ((0, xF86XK_AudioMute),
      spawn "amixer -D pulse set Master 1+ toggle")

    -- Decrease volume.
    , ((0, xF86XK_AudioLowerVolume),
      spawn "amixer -D pulse set Master 10%-")

    -- Increase volume.
    , ((0, xF86XK_AudioRaiseVolume),
      spawn "amixer -D pulse set Master unmute && amixer -D pulse set Master 10%+")
    ]
