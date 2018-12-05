{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt
import XMonad.Prompt.Pass
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.WindowBringer (bringMenu, gotoMenu)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Prompt.Window (windowPrompt)
import qualified XMonad.Prompt.Window as XPW
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Brightness as Brightness
import qualified XMonad.Actions.CycleWS
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.CycleWindows
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Util.WorkspaceCompare

xpconfig :: XPConfig
xpconfig =
  def
    { font = "xft:Deja Vu Sans Mono-10"
    , height = 48
    , searchPredicate = fuzzyMatch
    }

main :: IO ()
main = do
  xmprocs <- do
    numScreens :: Integer <- countScreens
    let
      xmobarWorkspaces :: [WorkspaceId]
      xmobarWorkspaces =
        map show [0..numScreens-1]
    mapM
      (\wId -> do
        process <- spawnPipe $ "xmobar -x " ++ wId
        pure (wId, process)) xmobarWorkspaces
  xmonad $
    let
      baseConf =
        desktopConfig
          { manageHook = manageDocks <+> manageHook def
          , layoutHook =
              avoidStruts $
                smartBorders $
                  Full |||
                    Tall 1 (3 / 100) (1 / 2) |||
                      Mirror (Tall 1 (3 / 100) (1 / 2))
          , terminal = "xterm"
          , handleEventHook = docksEventHook <+> handleEventHook def
          , borderWidth = 2
          , focusFollowsMouse = False
          , startupHook = docksStartupHook <+> startupHook def
          , logHook =
              mapM_ (\(_, xmproc) ->
                dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppSort = getSortByXineramaPhysicalRule def
                  }
                ) xmprocs
          , modMask = mod4Mask
          }
    in
      withUrgencyHookC
        BorderUrgencyHook { urgencyBorderColor = "#ffa500" }
        urgencyConfig { suppressWhen = Never } $
          baseConf `additionalKeys` myAdditionalKeys baseConf

  where
  myAdditionalKeys conf =
    [
      -- Start a fresh emacsclient
      ((modMask conf .|. shiftMask, xK_e),
      spawn "emacsclient --c")

    , ((modMask conf .|. shiftMask, xK_z),
      spawn "xscreensaver-command -lock")

      -- Start a fresh firefox
    , ((modMask conf .|. shiftMask, xK_f),
      spawn "firefox")

      -- Lock to greeter
    , ((modMask conf .|. shiftMask, xK_l),
      spawn "dm-tool switch-to-greeter")

      -- Urgents
    , ((modMask conf, xK_BackSpace), focusUrgent)
    , ((modMask conf .|. shiftMask, xK_BackSpace), clearUrgents)

    , ((modMask conf, xK_s), cycleRecentWindows [xK_Super_L] xK_s xK_w)
    , ((modMask conf, xK_z), rotOpposite)
    , ((modMask conf, xK_i), rotUnfocusedUp)
    , ((modMask conf, xK_u), rotUnfocusedDown)
    , ((modMask conf .|. controlMask, xK_i), rotFocusedUp)
    , ((modMask conf .|. controlMask, xK_u), rotFocusedDown)

      -- Bring up dmenu to go to window
    , ((modMask conf, xK_g), gotoMenu)

      -- Bring up dmenu to bring window here
    , ((modMask conf, xK_b), bringMenu)

      -- Bring up dmenu to start apps
    , ((modMask conf, xK_p),
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
    , ((modMask conf, xK_grave), XMonad.Actions.CycleWS.toggleWS)

    -- 'pass' integration
    , ((modMask conf .|. shiftMask, xK_p), passPrompt xpconfig)
    , ((modMask conf .|. shiftMask, xK_g), windowPrompt xpconfig XPW.Goto XPW.wsWindows)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N (non-greedy)
    [((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <-
            [ (W.view, 0)
            , (W.shift, shiftMask)
            , (W.greedyView, controlMask)
            ]]
