module Main where

import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Actions.WindowBringer (bringMenu, gotoMenu)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Brightness as Brightness
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.CycleWindows

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $
    let
      baseConf =
        desktopConfig
          { manageHook = manageDocks <+> manageHook def
          , layoutHook =
              avoidStruts $
                smartBorders $
                  Tall 1 (3 / 100) (1 / 2) |||
                  noBorders Full
          , terminal = "xterm" -- gnome-terminal"
          , handleEventHook = docksEventHook <+> handleEventHook def
          , borderWidth = 2
          , focusFollowsMouse = False
          , startupHook = docksStartupHook <+> startupHook def
          , logHook =
              dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                }
          , modMask = mod4Mask
          }
    in
      baseConf `additionalKeys` myAdditionalKeys baseConf

  where
  myAdditionalKeys conf =
    [
      -- Start a fresh emacsclient
      ((mod4Mask .|. shiftMask, xK_e),
      spawn "emacsclient --c")

      -- Start a fresh firefox
    , ((mod4Mask .|. shiftMask, xK_f),
      spawn "chromium-browser")

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
    -- , ((0, xF86XK_AudioLowerVolume),
    , ((0, xK_F11),
      spawn "amixer -D pulse set Master 5%-")

    -- Increase volume.
    -- , ((0, xF86XK_AudioRaiseVolume),
    , ((0, xK_F12),
      spawn "amixer -D pulse set Master unmute && amixer -D pulse set Master 5%+")

    -- Increase/decrease brightness
    -- Note: Requires small manual set up on new systems (see docs)
    , ((0, xF86XK_MonBrightnessUp), () <$ (liftIO $ Brightness.change (+50)))
    , ((0, xF86XK_MonBrightnessDown), () <$ do
          liftIO $ Brightness.change $ \current ->
            if current > 100
              then current - 50
              else current
      )
    ] ++ 
    -- mod-[1..9] %! Switch to workspace N (non-greedy)
    [((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
