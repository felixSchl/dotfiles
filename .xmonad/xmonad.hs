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
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.CycleWindows
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.CycleWS

xpconfig :: XPConfig
xpconfig =
  def
    { font = "xft:Deja Vu Sans Mono-10"
    , height = 32
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
        desktopConfig
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
                  Full |||
                    Tall 1 (3 / 100) (1 / 2) |||
                      Mirror (Tall 1 (3 / 100) (1 / 2))
          , terminal = "xterm"
          , handleEventHook =
              docksEventHook <+>
              handleEventHook def
          , borderWidth = 3
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

    , ((modMask conf, xK_s), cycleRecentWindows [xK_Super_L] xK_s xK_w)
    , ((modMask conf, xK_z), rotOpposite)
    , ((modMask conf, xK_i), rotUnfocusedUp)
    , ((modMask conf, xK_u), rotUnfocusedDown)
    , ((modMask conf .|. controlMask, xK_i), rotFocusedUp)
    , ((modMask conf .|. controlMask, xK_u), rotFocusedDown)

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

    , ((modMask conf .|. shiftMask, xK_o),
        let
          completions str =
            return $ filter ((searchPredicate xpconfig) str) $
              [ "dn3010/jasmy-call-service"
              , "dn3010/js-ipfs-api"
              , "dn3010/plug.js"
              , "dn3010/psc-package-sets"
              , "dn3010/purescript-aff-queue"
              , "dn3010/purescript-indexedDB"
              , "dn3010/purescript-ipfs-api"
              , "dn3010/purescript-ipfs-log"
              , "dn3010/purescript-lock"
              , "dn3010/purescript-logging"
              , "dn3010/purescript-node-sqlite3"
              , "dn3010/purescript-node-sqlite3-extras"
              , "dn3010/purescript-node-streams"
              , "dn3010/purescript-plug-client"
              , "dn3010/purescript-processor"
              , "dn3010/purescript-signal-protocol"
              , "dn3010/purescript-supervisor"
              , "dn3010/purescript-webrtc"
              , "dn3010/substrate-node-template"
              , "dn3010/sylo-hub"
              , "dn3010/sylo-mobile"
              , "dn3010/sylo-plug-node"
              , "dn3010/sylo-protocol"
              , "dn3010/sylo-protocol-factory-browser"
              , "dn3010/sylo-protocol-redux"
              , "dn3010/sylo-pure-calling"
              , "dn3010/sylo-pure-e2ee"
              , "dn3010/sylo-redux"
              ]
        in do
          inputPromptWithCompl xpconfig "Choose Url" completions
            ?+ \url -> do
              runOrRaise "firefox" (className =? "Firefox" <||> className =? "Firefox-bin")
              spawnHere $ "firefox " ++ "https://github.com/" ++ trim' url
      )
    ]

    ++
    -- Switch to workspace N
    [((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <-
            [ (W.view, 0)
            , (W.shift, shiftMask)
            , (W.greedyView, controlMask)
            ]]
trim' :: String -> String
trim' = f . f
   where f = reverse . dropWhile isSpace
