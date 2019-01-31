{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Char (isSpace)
import Data.Traversable (for)
import Control.Monad (void)
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt
import XMonad.Prompt.Pass
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.TagWindows (tagPrompt, delTag, addTag, focusUpTaggedGlobal)
import XMonad.Actions.SpawnOn (spawnHere)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Prompt.Input
import XMonad.Prompt.Window (windowPrompt)
import qualified XMonad.Prompt.Window as XPW
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Brightness as Brightness
import qualified XMonad.Actions.CycleWS
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.CycleWindows
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.CycleWS
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CopyWindow
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.ToggleLayouts
import XMonad.Actions.Navigation2D

xpconfig :: XPConfig
xpconfig =
  def
    { font = "xft:Deja Vu Sans Mono-10"
    , height = 48
    , searchPredicate = fuzzyMatch
    , alwaysHighlight = True
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
        ewmh def
          { manageHook =
              let
                myTitleFloats = []
                myClassFloats = ["Pinentry"]
              in
                composeAll $
                  (concat
                    [ [ title =? t --> doFloat | t <- myTitleFloats]
                    , [ className =? c --> doFloat | c <- myClassFloats ]
                    ]) ++ [ manageDocks
                          , manageHook def
                          , isFullscreen --> doFullFloat
                          ]
          , layoutHook =
              avoidStruts $
                smartBorders $
                  toggleLayouts Full emptyBSP
          , terminal = "xterm"
          , handleEventHook =
              docksEventHook <+>
              handleEventHook def
          , borderWidth = 3
          , focusFollowsMouse = False
          , startupHook = docksStartupHook <+> startupHook def
          , logHook =
              mapM_ (\(_, xmproc) ->
                workspaceNamesPP xmobarPP >>= \pp ->
                  dynamicLogWithPP pp
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
    [ ((modMask conf .|. shiftMask, xK_z),
      spawn "xscreensaver-command -lock")

      -- Start a emacsclient
    , ((modMask conf .|. shiftMask, xK_e), spawn "emacsclient --c")
    , ((modMask conf .|. shiftMask .|. controlMask, xK_e),
      runOrRaise "emacsclient --c" (className =? "Emacs"))

      -- Start firefox
    , ((modMask conf .|. shiftMask, xK_f), spawn "firefox")
    , ((modMask conf, xK_f),
      runOrRaise "firefox" (className =? "Firefox" <||> className =? "Firefox-bin"))

      -- Lock to greeter
    , ((modMask conf .|. shiftMask, xK_l),
      spawn "dm-tool switch-to-greeter")

      -- Urgents
    , ((modMask conf, xK_BackSpace), focusUrgent)
    , ((modMask conf .|. shiftMask, xK_BackSpace), clearUrgents)

    , ((modMask conf, xK_space), sendMessage ToggleLayout)

    -- Control EBSP
    , ((modMask conf .|. shiftMask,                 xK_l), sendMessage $ ExpandTowards R)
    , ((modMask conf .|. shiftMask,                 xK_h), sendMessage $ ExpandTowards L)
    , ((modMask conf .|. shiftMask,                 xK_j), sendMessage $ ExpandTowards D)
    , ((modMask conf .|. shiftMask,                 xK_k), sendMessage $ ExpandTowards U)
    , ((modMask conf .|. shiftMask .|. controlMask, xK_l), sendMessage $ ShrinkFrom R)
    , ((modMask conf .|. shiftMask .|. controlMask, xK_h), sendMessage $ ShrinkFrom L)
    , ((modMask conf .|. shiftMask .|. controlMask, xK_j), sendMessage $ ShrinkFrom D)
    , ((modMask conf .|. shiftMask .|. controlMask, xK_k), sendMessage $ ShrinkFrom U)
    , ((modMask conf,                               xK_r), sendMessage Rotate)
    , ((modMask conf,                               xK_s), sendMessage Swap)
    , ((modMask conf,                               xK_n), sendMessage FocusParent)
    , ((modMask conf .|. controlMask,               xK_n), sendMessage SelectNode)
    , ((modMask conf .|. shiftMask,                 xK_n), sendMessage MoveNode)
    , ((modMask conf,                               xK_b), sendMessage Balance)
    , ((modMask conf .|. shiftMask,                 xK_b), sendMessage Equalize)

   -- Directional navigation of screens
   , ((modMask conf, xK_l), windowGo R False)
   , ((modMask conf, xK_h), windowGo L False)
   , ((modMask conf, xK_k), windowGo U False)
   , ((modMask conf, xK_j), windowGo D False)

      -- Bring up dmenu to start apps
    , ((modMask conf, xK_p),
      spawn "dmenu_run")

      -- Mute volume.
    , ((0, xF86XK_AudioMute),
      spawn "amixer -D pulse set Master 1+ toggle")

    -- Decrease volume.
    , ((0, xF86XK_AudioLowerVolume),
      spawn "amixer -D pulse set Master 5%-")
    , ((0, xK_F11), -- XXX office keyboard
      spawn "amixer -D pulse set Master 5%-")

    -- Increase volume.
    , ((0, xF86XK_AudioRaiseVolume),
      spawn "amixer -D pulse set Master unmute && amixer -D pulse set Master 5%+")
    , ((0, xK_F12), -- XXX office keyboard
      spawn "amixer -D pulse set Master unmute && amixer -D pulse set Master 5%+")

    -- Toggle struts (hide xmobar)
    , ((mod4Mask .|. shiftMask, xK_F12), sendMessage ToggleStruts)

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

    -- window prompt
    , ((modMask conf, xK_o), windowPrompt xpconfig XPW.Goto XPW.allWindows)

    -- tagging
    , ((modMask conf, xK_a),
       tagPrompt xpconfig focusUpTaggedGlobal)
    , ((modMask conf .|. shiftMask, xK_a),
       tagPrompt xpconfig (withFocused . addTag))
    , ((modMask conf .|. shiftMask .|. controlMask, xK_a),
       tagPrompt xpconfig (withFocused . delTag))

    , ((modMask conf .|. controlMask, xK_h), moveTo Prev NonEmptyWS)
    , ((modMask conf .|. controlMask, xK_l), moveTo Next NonEmptyWS)

    , ((modMask conf .|. shiftMask, xK_r), renameWorkspace xpconfig)
    , ((modMask conf, xK_i), workspaceNamePrompt xpconfig (windows . W.view))
    -- , ((modMask conf, xK_comma), workspaceNamePrompt xpconfig (windows . W.view))

    -- XMonad.Actions.CopyWindow
    , ((modMask conf .|. shiftMask, xK_c), kill1)
    ]

    ++
    -- Switch to workspace N
    [((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <-
            [ (W.view, 0)
            , (W.shift, shiftMask)
            , (W.greedyView, controlMask)
            , (copy, shiftMask .|. controlMask)
            ]]
