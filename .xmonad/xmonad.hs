{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception                  (SomeException, catch)
import           Control.Monad                      (void)
import qualified Data.Map                           as M
import qualified Data.Map.Strict                    as Map
import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWindows
import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.CycleWS
import           XMonad.Actions.Minimize
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.TagWindows          (addTag, delTag,
                                                     focusUpTaggedGlobal,
                                                     tagPrompt)
import           XMonad.Actions.WindowGo
import           XMonad.Actions.WorkspaceNames      (renameWorkspace,
                                                     workspaceNamePrompt,
                                                     workspaceNamesPP)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops          (ewmh)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.BinarySpacePartition
import qualified XMonad.Layout.BoringWindows        as BW
import           XMonad.Layout.Hidden
import           XMonad.Layout.IndependentScreens   (countScreens)
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders            (smartBorders)
import           XMonad.Layout.ToggleLayouts
import           XMonad.Prompt
import qualified XMonad.Prompt                      as XP
import           XMonad.Prompt.FuzzyMatch           (fuzzyMatch)
import           XMonad.Prompt.Input
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Window               (windowPrompt)
import qualified XMonad.Prompt.Window               as XPW
import qualified XMonad.StackSet                    as W
import qualified XMonad.Util.Brightness             as Brightness
import           XMonad.Util.EZConfig               (additionalKeys)
import           XMonad.Util.Run                    (runProcessWithInput,
                                                     safeSpawn, spawnPipe)
import           XMonad.Util.WorkspaceCompare

xpconfig :: XPConfig
xpconfig =
  def
    { font = "xft:Deja Vu Sans Mono-10"
    , height = 48
    , searchPredicate = fuzzyMatch
    , alwaysHighlight = True
    , promptKeymap =
          M.fromList
            [ ((controlMask, xK_y), do
                  clip <-
                    liftIO $
                      catch
                        (runProcessWithInput "xclip" ["-sel", "clip", "-o" ] "")
                        (\(_ :: SomeException) -> return "")
                  XP.insertString clip)
            ] `M.union` promptKeymap def
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
                myClassFullFloats = ["csgo_linux64", "hl2_linux", "portal2_linux"]
              in
                composeAll $
                  (concat
                    [ [ title =? t --> doFloat | t <- myTitleFloats]
                    , [ className =? c --> doFloat | c <- myClassFloats ]
                    , [ className =? c --> doFullFloat | c <- myClassFullFloats ]
                    ]) ++ [ manageDocks
                          , manageHook def
                          , isFullscreen --> doFullFloat
                          ]
          , layoutHook =
              BW.boringWindows $
                avoidStruts $
                  smartBorders $
                    toggleLayouts Full $
                      minimize $
                        emptyBSP
          , terminal = "xterm"
          , handleEventHook =
              docksEventHook <+>
              handleEventHook def
          , borderWidth = 3
          , focusFollowsMouse = True
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

      -- Start chromium
    , ((modMask conf .|. shiftMask, xK_f), spawn "chromium-browser")
    , ((modMask conf, xK_f),
      runOrRaise "chromium-browser" (className =? "chromium-browser"))

      -- Screenshots
    , ((modMask conf .|. shiftMask, xK_u),
      spawn "sh -c 'sleep 0.2; scrot -s /tmp/clip.png && xclip -selection c -t image/png < /tmp/clip.png'")

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

    -- boring windows
    , ((modMask conf, xK_backslash), BW.markBoring)
    , ((modMask conf .|. shiftMask, xK_backslash), BW.clearBoring)

    -- hide windows
    -- , ((modMask conf .|. controlMask .| shiftMask, xK_), withFocused hideWindow)

   -- Directional navigation of screens
   , ((modMask conf, xK_l), windowGo R False)
   , ((modMask conf, xK_h), windowGo L False)

   -- context-sensitive j/k.
   , ((modMask conf, xK_k), do
         ws <- W.workspace . W.current . windowset <$> get
         if description (W.layout ws) == "Full" || length (W.integrate' $ W.stack ws) <= 2
           then BW.focusUp
           -- then windows W.focusUp
           else windowGo U False)
   , ((modMask conf, xK_j), do
         ws <- W.workspace . W.current . windowset <$> get
         if description (W.layout ws) == "Full" || length (W.integrate' $ W.stack ws) <= 2
           then BW.focusDown
           -- then windows W.focusDown
           else windowGo D False)

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

    -- search haskell lts
    , ((modMask conf .|. shiftMask, xK_s),
        let
          completions str =
            return []
        in do
          inputPromptWithCompl (xpconfig { alwaysHighlight = False }) "Search stackage" completions
            ?+ \query -> do
              let url = "https://www.stackage.org/lts-12.26/hoogle?q=" ++ query
              raiseAndDo
                (safeSpawn "chromium-browser" [url])
                (className =? "chromium-browser")
                (\_ -> safeSpawn "chromium-browser" [url])
      )

    -- 'pass' integration
    , ((modMask conf .|. shiftMask, xK_p), passPrompt xpconfig)

    -- window prompt
    , ((modMask conf, xK_o), windowPrompt xpconfig XPW.Goto XPW.allWindows)

    -- Window tagging
    , ((modMask conf, xK_a),
       tagPrompt xpconfig focusUpTaggedGlobal)
    , ((modMask conf .|. shiftMask, xK_a),
       tagPrompt xpconfig (withFocused . addTag))
    , ((modMask conf .|. shiftMask .|. controlMask, xK_a),
       tagPrompt xpconfig (withFocused . delTag))

    -- Window cloning
    , ((modMask conf .|. shiftMask, xK_c), kill1)

    -- Workspace management
    , ((modMask conf .|. controlMask, xK_h), moveTo Prev NonEmptyWS)
    , ((modMask conf .|. controlMask, xK_l), moveTo Next NonEmptyWS)
    , ((modMask conf .|. shiftMask, xK_r), renameWorkspace xpconfig)
    , ((modMask conf, xK_i), workspaceNamePrompt xpconfig (windows . W.view))
    , ((modMask conf, xK_comma), workspaceNamePrompt xpconfig (windows . W.view))

    , ((modMask conf .|. shiftMask, xK_o),
        let
          mapping = Map.fromList $
            fmap (\x -> (x, "https://github.com/" ++ x))
              [ "dn3010/psc-package-sets"
              , "dn3010/purescript-aff-queue"
              , "dn3010/purescript-indexedDB"
              , "dn3010/purescript-libp2p"
              , "dn3010/purescript-ipfs-api"
              , "dn3010/purescript-ipfs-log"
              , "dn3010/purescript-lock"
              , "dn3010/purescript-logging"
              , "dn3010/purescript-node-sqlite3"
              , "dn3010/purescript-node-sqlite3-extras"
              , "dn3010/purescript-node-streams"
              , "dn3010/purescript-plug-client"
              , "dn3010/purescript-protobuffers"
              , "dn3010/purescript-oplog-sync"
              , "dn3010/purescript-processor"
              , "dn3010/purescript-signal-protocol"
              , "dn3010/purescript-supervisor"
              , "dn3010/purescript-webrtc"
              , "dn3010/substrate-node-template"
              , "dn3010/sylo-hub"
              , "dn3010/node-libp2p"
              , "dn3010/sylo-mobile"
              , "dn3010/sylo-plug-node"
              , "dn3010/sylo-protocol"
              , "dn3010/sylo-protocol-client"
              , "dn3010/sylo-protocol-factory-browser"
              , "dn3010/sylo-protocol-factory-rn"
              , "dn3010/sylo-protocol-redux"
              , "dn3010/sylo-pure-calling"
              , "dn3010/sylo-pure-e2ee"
              , "dn3010/sylo-redux"
              ] ++ [ ("dn3010/wiki", "https://github.com/dn3010/wiki/wiki") ]
          completions str =
            return $ filter ((searchPredicate xpconfig) str) $
              fmap fst $ Map.toList mapping
        in
          inputPromptWithCompl xpconfig "Choose Url" completions
            ?+ \key ->
              case Map.lookup key mapping of
                Nothing ->
                  pure ()
                Just url ->
                  raiseAndDo
                    (safeSpawn "chromium-browser" [url])
                    (className =? "chromium-browser")
                    (\_ -> safeSpawn "chromium-browser" [url])
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
            , (copy, shiftMask .|. controlMask)
            ]]
