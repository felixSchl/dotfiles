module Main where

import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Actions.WindowBringer (bringMenu, gotoMenu)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import qualified XMonad.Util.Brightness as Brightness
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.CycleWindows

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ desktopConfig
    { manageHook = manageDocks <+> manageHook def
    , layoutHook = avoidStruts  $ layoutHook def
    , terminal = "gnome-terminal"
    , handleEventHook = docksEventHook <+> handleEventHook def
    , startupHook = docksStartupHook <+> startupHook def
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

      -- Lock to greeter
    , ((mod4Mask .|. shiftMask, xK_l),
      spawn "dm-tool switch-to-greeter")

    , ((mod4Mask, xK_s), cycleRecentWindows [xK_Super_L] xK_s xK_w)
    , ((mod4Mask, xK_z), rotOpposite)
    , ((mod4Mask, xK_i), rotUnfocusedUp)
    , ((mod4Mask, xK_u), rotUnfocusedDown)
    , ((mod4Mask .|. controlMask, xK_i), rotFocusedUp)
    , ((mod4Mask .|. controlMask, xK_u), rotFocusedDown)

      -- Bring up dmenu to go to window
    , ((mod4Mask, xK_g), gotoMenu)

      -- Bring up dmenu to bring window here
    , ((mod4Mask, xK_b), bringMenu)

      -- Bring up dmenu to start apps
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

    -- Increase/decrease brightness
    -- Note: Requires small manual set up on new systems (see docs)
    , ((0, xF86XK_MonBrightnessUp), () <$ (liftIO $ Brightness.change (+50)))
    , ((0, xF86XK_MonBrightnessDown), () <$ do
          liftIO $ Brightness.change $ \current ->
            if current > 100
              then current - 50
              else current
      )
    ]
