{-# LANGUAGE DeriveDataTypeable #-}

import CompatibilityShims
import Superscripts (superScriptNum)

{-
    - Sub all unqualified usages of xK_{...}:
    :%s/\W\@<=\%(Key\.\)\@<!\%(xK\)\@=/Key./g
-}

import Numeric (showHex)
import XMonad -- (ExtensionClass(..), xmonad)
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Warp (warpToScreen)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Layout.WindowNavigation
import XMonad.Util.Run
import qualified XMonad.Util.ExtensibleState as XS
import Data.Monoid
import System.Exit

import qualified XMonad.Layout.LayoutCombinators as LC
import XMonad.Layout.Groups (group)
import XMonad.Layout.Groups.Examples (zoomRowG)
import XMonad.Layout.Groups.Wmii
import XMonad.Layout.MessageControl (ignore, unEscape)
import XMonad.Layout.Named (named)
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft))
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed (addTabs)
import XMonad.Operations (rescreen)

import System.Directory (getHomeDirectory)
import GHC.IO.Handle (hClose, hFlush)
import GHC.IO.Handle.Types (Handle)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Prompt
import XMonad.Prompt.Workspace (workspacePrompt)

import qualified Graphics.X11.Xlib.Atom as Atom
import qualified Graphics.X11.Xlib.Extras as Xtras
import qualified Graphics.X11.Types as Key
import Graphics.X11.ExtraTypes.XF86

-- for myDynamicLogWithPP
import Codec.Binary.UTF8.String (encodeString)
import Data.List (find, intersperse, sortBy)
import Data.Maybe (isJust, catMaybes)
import System.Posix (getProcessGroupIDOf)
import System.Posix.Signals (signalProcessGroup, sigTERM)
import System.Posix.Types (ProcessGroupID, ProcessID)
import XMonad.Util.NamedWindows
import XMonad.Util.WorkspaceCompare (getWsCompareByTag, WorkspaceSort)

import Control.Applicative (liftA)
import qualified Control.Exception as E
import Control.Monad (when)

import Data.Set ((\\), toList, fromList)

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "term"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod3Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = map show [1..10]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#285577"
myFocusedBorderColor = "#ff9900"
myUrgentColor = "orange"

myPrompt = def
    { bgColor = statusColorBG
    , fgColor = statusColorNormalFG
    , bgHLight = statusColorNormalFG
    , fgHLight = statusColorBG
    , borderColor = myNormalBorderColor
    , position = Top
    }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

-- probably-broken window movement commands.  Using two competing
-- metaphors (tabs and stacks), so we do both actions.
--
windowDown = do
    sendMessage $ Go D
    focusDown
windowUp = do
    sendMessage $ Go U
    focusUp

data WarpViewStyle = Warp
                   | Greedy
    deriving (Typeable, Show, Read, Eq, Enum, Bounded)

instance ExtensionClass WarpViewStyle where
    initialValue = Warp

warpViewCycle :: WarpViewStyle -> WarpViewStyle
warpViewCycle x
  | x == maxBound = minBound
  | otherwise = succ x

-- |
-- Change to the specified workspace. If the newly-selected workspace was
-- visible, but not primary, before the change, warp the mouse pointer to it.
warpView :: WorkspaceId -> X ()
warpView tag = do
    XState { windowset = old } <- get
    style <- XS.get :: X WarpViewStyle
    case style of
      Greedy -> windows $ W.greedyView tag
      Warp -> (windows $ W.view tag) >> warpIfVisible tag old

warpIfVisible :: String -> WindowSet -> X ()
warpIfVisible tag old = do
    let byTag = ((tag ==) . W.tag . W.workspace)
    case find byTag (W.visible old) of
      Just s -> warpToScreen (W.screen s) 0.4 0.5
      _ -> return ()

currentWindows :: XState -> [Window]
currentWindows = W.integrate' . W.stack . W.workspace . W.current . windowset

shiftAll :: WorkspaceId -> X ()
shiftAll tag = gets currentWindows >>= windows . sendAll
    where
        sendAll :: [Window] -> WindowSet -> WindowSet
        sendAll = flip $ foldl sendWindow
        sendWindow :: WindowSet -> Window -> WindowSet
        sendWindow = flip $ W.shiftWin tag

fixXinerama :: X ()
fixXinerama = do
  rescreen
  windows $ \wins@(W.StackSet { W.visible = vis, W.hidden = hid }) ->
    let present = fromList $ W.tag <$> W.workspaces wins
        wanted = fromList $ show <$> [1..10]
        missing = toList $ wanted \\ present
        base = W.workspace (W.current wins)
        withTag tag = base { W.tag = tag, W.stack = Nothing }
     in wins { W.hidden = W.hidden wins ++ (withTag <$> missing) }

raiseWindowByClass :: String -> X ()
raiseWindowByClass cls = withDisplay $ \d -> do
    XConf{ theRoot = r } <- ask
    (_, _, lowToHigh) <- io $ Xtras.queryTree d r
    case (reverse lowToHigh) of
      (top:r:rs) -> do
          tclass <- classOf top
          when (tclass /= Just cls) $ withDisplay $ \d -> do
              win <- findFirst (r:rs)
              maybe (pure ()) (io . raiseWindow d) win

      _ -> pure ()
    where
        findFirst :: [Window] -> X (Maybe Window)
        findFirst [] = pure Nothing
        findFirst (w:ws) = do
            c <- classOf w
            case c of
              Just x | x == cls -> return $ Just w
              _ -> findFirst ws

        classOf :: Window -> X (Maybe String)
        classOf win = do
            cs <- (classes win) `catchX` (pure [])
            case cs of
              (_:c:_) -> return $ Just c
              _ -> pure Nothing

        classes :: Window -> X [String]
        classes win = withDisplay $ \d -> io $ do
            p <- Xtras.getTextProperty d win Atom.wM_CLASS
            Xtras.wcTextPropertyToTextList d p
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm, xK_Return), spawn $ "in-cwd " ++ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_Return), spawn $ "in-cwd uxterm")
    , ((mod1Mask, xK_Return), spawn $ "in-cwd " ++ XMonad.terminal conf)
    , ((mod4Mask, xK_Return), spawn $ "LC_ALL=en_US.UTF-8 term -e /bin/bash -l")
    , ((mod4Mask, xK_t), spawn "tmux-choose")

    -- launch dmenu
    , ((mod1Mask, xK_space), spawn "dmenu_run")

    -- other launchers
    ---- process monitoring
    , ((mod4Mask, xK_h), spawn $ XMonad.terminal conf ++ " -e htop")
    , ((mod4Mask, xK_i), spawn $ XMonad.terminal conf ++ " -e sudo iotop")
    , ((mod4Mask, xK_p), spawn $ XMonad.terminal conf ++ " -e sudo powertop")

    ---- web browsers
    , ((modm, xK_c), spawn "chromium --new-window")
    , ((mod4Mask, xK_c), spawn "chromium --incognito")
    , ((mod4Mask, xK_s), spawn "scratch")

    ---- ssh
    , ((modm, xK_u), spawn "ssh-choose -r -1")
    , ((modm, xK_slash), spawn "ssh-choose -r")

    ---- screensaver/sleep
    , ((modm, xK_Scroll_Lock), spawn "xscreensaver-command -activate")
    , ((mod4Mask, xK_Scroll_Lock), spawn "xscreensaver-command -activate")
    , ((mod4Mask, xK_l), spawn "xscreensaver-command -activate")
    , ((0, xF86XK_ScreenSaver), spawn "xscreensaver-command -activate")
    , ((0, xF86XK_Sleep), spawn "slp")
    , ((mod4Mask .|. shiftMask, xK_l), spawn "slp")
    , ((mod4Mask .|. controlMask, xK_l), spawn "blank")

    ---- screenshot
    , ((0, xK_Print), spawn "screenshot")
    , ((mod4Mask, xK_Multi_key), spawn "screenshot")

    ---- monitor toggle
    , ((0, xF86XK_Display), spawn "mon --toggle")
    , ((shiftMask, xF86XK_Display), spawn "mon --cycle")

    ---- mpd
    , ((0, xF86XK_AudioPlay), spawn "music toggle")
    , ((0, xF86XK_AudioPause), spawn "music toggle") {- headset sends "Pause" -}
    , ((0, xF86XK_AudioStop), spawn "music stop")
    , ((0, xF86XK_AudioNext), spawn "music next")
    , ((0, xF86XK_AudioPrev), spawn "music prev")
    , ((0, xF86XK_AudioMute), spawn "toggle-pulse-mute")
    , ((mod4Mask, xF86XK_AudioRaiseVolume), spawn "sound-check")
    , let speakerTest = spawn "speaker-test -t wav -c2 -l1"
       in ((mod4Mask .|. shiftMask, xF86XK_AudioRaiseVolume), speakerTest)
    , ((0, xF86XK_AudioLowerVolume), spawn "pavucontrol")

    ---- brightness
    , ((modm, xK_F5), spawn "brightness = 1")
    , ((modm, xF86XK_MonBrightnessDown), spawn "brightness = 1")
    , ((modm .|. shiftMask, xK_F5), spawn "brightness down")
    , ((modm, xK_F6), spawn "brightness max")
    , ((modm, xF86XK_MonBrightnessUp), spawn "brightness max")
    , ((modm .|. shiftMask, xK_F6), spawn "brightness up")

    ---- pickers
    -- `run` a command
    , ((mod4Mask, xK_r), spawn "pick run")
    -- `vim` by file(s) being edited
    , ((mod4Mask, xK_v), spawn "pick kak")
    , ((mod4Mask .|. shiftMask, xK_v), spawn "pick vim")
    -- `zsh` by command or working directory
    , ((mod4Mask, xK_z), spawn "pick zsh")
    -- `mpc` by song
    , ((mod4Mask, xK_m), spawn "pick mpc")
    -- `window`
    , ((mod4Mask, xK_w), spawn "pick window")
    -- `wifi` ('shift+' because it's less common)
    , ((mod4Mask .|. shiftMask, xK_w), spawn "wifi-chooser")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c), kill)

    -- Rotate through the available layout algorithms
    , ((mod4Mask, xK_space), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((mod4Mask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm, xK_n), refresh)

    -- Swap the focused window and the master window
    , ((mod4Mask .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Increment the number of windows in the master area
    , ((modm .|. shiftMask, xK_comma), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm .|. shiftMask, xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm, xK_q), spawn "xmonad --recompile && xmonad --restart")
    , ((modm, xK_r), spawn "xmonad --recompile")
    ]
    ++

    -- Directional movement
    [ ((modm, xK_h), sendMessage $ Go L)
    , ((modm, xK_j), windowDown)
    , ((modm, xK_k), windowUp)
    , ((modm, xK_l), sendMessage $ Go R)
    , ((modm .|. shiftMask, xK_j), sendMessage $ Swap D)
    , ((modm .|. shiftMask, xK_j), swapDown)
    , ((modm .|. shiftMask, xK_k), sendMessage $ Swap U)
    , ((modm .|. shiftMask, xK_k), swapUp)
    ]
    ++

    -- wmii style layouts
    [ ((modm, xK_s), groupToTabbedLayout)
    , ((modm, xK_m), groupToFullLayout)
    , ((modm, xK_f), groupToNextLayout)
    , ((modm, xK_d), groupToVerticalLayout)
    , ((modm .|. shiftMask, xK_h), moveToGroupUp False)
    , ((modm .|. shiftMask, xK_l), moveToGroupDown False)
    , ((modm, xK_space), withFocused float)
    , ((modm .|. shiftMask, xK_space), withFocused $ windows . W.sink)
    ]
    ++

    -- named workspaces
    [ ((modm, xK_t), selectWorkspace myPrompt)
    , ((modm .|. shiftMask, xK_t), withWorkspace myPrompt (windows . W.shift))
    , ((modm, xK_p), moveTo Prev NonEmptyWS)
    , ((modm .|. shiftMask, xK_p), swapTo Prev)
    , ((modm, xK_n), moveTo Next NonEmptyWS)
    , ((modm .|. shiftMask, xK_n), swapTo Next)
    , ((modm .|. shiftMask, xK_minus), removeEmptyWorkspace)
    ]
    ++

    -- fix issue with Xinerama when (dis-/)connecting laptop dock
    [ ((mod4Mask, xK_q), fixXinerama) ]
    ++

    --
    -- mod-[1..9,0,i], Switch to workspace N
    -- mod-shift-[1..9,0,i], Move client to workspace N
    -- "0" maps to N=10
    -- "i" for "IM" maps to N=7 (force of habit)
    --
    let workspaces = show <$> [1 .. 9] ++ [10, 7]
        keys = [xK_1 .. xK_9] ++ [xK_0, xK_i]
    in [ ((modm .|. mask, key), action workspace)
       | (workspace, key) <- zip workspaces keys
       , (mask, action) <- [ (0, warpView)
                           , (shiftMask, windows . W.shift)
                           , (mod4Mask, windows . W.greedyView)
                           , (mod4Mask .|. shiftMask, shiftAll)
                           ]
       ]
    ++
    -- Toggle the state for warpView actions
    [ ((mod4Mask .|. shiftMask, xK_1), XS.modify warpViewCycle >> refresh) ]
    ++

    -- Ctrl+Win+[q,w,e,r,t,...] = warp to screen 0,1,2,...
    [ ((mod4Mask .|. controlMask, key), warpToScreen screen 0.5 0.5)
      | (key, screen) <- zip [xK_q, xK_w, xK_e, xK_r, xK_t, xK_y] [0..]]
    ++

    -- Grid Select
    [ ((mod4Mask, xK_g), goToSelected def) ]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
type WinFunc = Window -> X ()
modsMouseBindings :: ([KeyMask], Button, WinFunc) -> [((KeyMask, Button), WinFunc)]
modsMouseBindings (ms, b, f) = map (\m -> ((m, b), f)) $ ms

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    concat $ map modsMouseBindings $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ (mainmods, button1, (\w -> focus w >> mouseMoveWindow w))
    , (mainmods, button2, (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , (mainmods, button3, (\w -> focus w >> mouseResizeWindow w))

    -- alt + scroll = workspace up/down
    , (mainmods, button4, (\w -> windowUp))
    , (mainmods, button5, (\w -> windowDown))
    ]
        where
            mainmods = [modm, mod1Mask]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayoutHook = windowNavigation $
               FS.fullscreenFocus $
               myWmii ||| wmiiLike ||| Grid
  where
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

     wmiiLike = wmii shrinkText def

     -- myWmii is basically the same as stock wmii,
     -- but it defaults to tabs first
     myWmii = group innerLayout zoomRowG
        where column = named "Column" $ avoidStruts $ Tall 0 delta ratio
              tabs = named "Tabs" $ avoidStruts $ Simplest
              innerLayout = renamed [CutWordsLeft 3]
                            $ addTabs shrinkText def
                            $ ignore NextLayout
                            $ ignore (LC.JumpToLayout "") $ unEscape
                                $ tabs LC.||| column LC.||| Full

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = FS.fullscreenManageHook <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Pinentry" --> doFloat
    , className =? "Pavucontrol" --> doFloat
    , className =? "Vncviewer" --> doFloat
    , className =? "sun-awt-X11-XFramePeer" --> doFloat
    , className =? "java-lang-Thread" --> doFloat
    , className =? "xfreerdp" --> unfloat
    , title =? "QEMU" --> doFloat
    , title =? "Authy" --> doFloat
    ]
  where unfloat = ask >>= doF . W.sink

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = FS.fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLayoutDisplay :: String -> String
myLayoutDisplay "Tall" = "[]="
myLayoutDisplay "Full" = "[M]"
myLayoutDisplay "Tabs by ZoomRow" = "tabs"
myLayoutDisplay other = wrap "(layout:" ")" other

statusColorNormalFG = "white"
statusColorSubdued = "gray60"
statusColorBG = "#285577"

statusBarProc :: String -> String
statusBarProc xmonadDir = "dzen2 -dock -expand right -w 980 -x 0 -fn 'DejaVu Sans Mono'"
statusBarColor = dzenColor
statusNormalColor = statusBarColor statusColorNormalFG statusColorBG

escapeStatusCodes :: String -> String
escapeStatusCodes title = foldl (\acc c -> acc ++ case c of
 '{' -> "("
 '}' -> ")"
 a -> [a]) [] title

-- | Output a list of strings, ignoring empty ones and separating the
--   rest with the given separator.
sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = concat . intersperse sep . filter (not . null)

-- | Format the current status using the supplied pretty-printing format,
--   and write it to stdout.
myDynamicLogWithPP :: PP -> X ()
myDynamicLogWithPP pp = myDynamicLogString pp >>= io . ppOutput pp

-- | Format the workspace information, given a workspace sorting function,
--   a list of urgent windows, a pretty-printer format, and the current
--   WindowSet.
myPprWindowSet :: WorkspaceSort -> [Window] -> PP -> WindowSet -> String
myPprWindowSet sort' urgents pp s = sepBy (ppWsSep pp) . map fmt . sort' $
            map W.workspace (W.current s : W.visible s) ++ W.hidden s
   where this     = W.currentTag s
         visibles = map (W.tag . W.workspace) (W.visible s)

         fmt w = printer pp (W.tag w ++ superScriptNum (nws w))
          where printer | any (\x -> maybe False (== W.tag w) (W.findTag x s)) urgents  = ppUrgent
                        | W.tag w == this                                               = ppCurrent
                        | W.tag w `elem` visibles                                       = ppVisible
                        | isJust (W.stack w)                                            = ppHidden
                        | otherwise                                                     = ppHiddenNoWindows
                nws = length . W.integrate' . W.stack

-- | The same as 'dynamicLogWithPP', except it simply returns the status
--   as a formatted string without actually printing it to stdout, to
--   allow for further processing, or use in some application other than
--   a status bar.
myDynamicLogString :: PP -> X String
myDynamicLogString pp = do
  winset <- gets windowset
  urgents <- readUrgents
  sort' <- ppSort pp
  style <- XS.get :: X WarpViewStyle

  -- layout description
  let ld = description . W.layout . W.workspace . W.current $ winset

  -- workspace list
  let ws = pprWindowSet sort' urgents pp winset
  -- let ws = show ((ppSort pp) $ map W.tag $ W.hidden $ winset)
  -- let ws = show (W.allWindows winset)
  -- let ws = show $ map length $ map W.tag $ W.hidden $ winset
  -- let ws = show $ map (\x -> ((length . W.integrate' . W.stack) x, W.tag x) :: (Int,String)) $ sortBy getWsCompareByTag $ (W.workspaces winset)

  -- window title
  wt <- maybe (return "") (fmap show . getName) . W.peek $ winset

  -- run extra loggers, ignoring any that generate errors.
  extras <- mapM (flip catchX (return Nothing)) $ ppExtras pp

  -- Description of current `warpView` style
  let warpStyle = statusNormalColor $ show style

  return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
         [ ws
         , warpStyle
         , ppLayout pp ld
         , ppTitle  pp wt
         ]
         ++ catMaybes extras

withLogHandlePP :: PP -> Handle -> PP
withLogHandlePP pp proc = pp { ppOutput = hPutStrLn proc . escapeStatusCodes }

withLogHandlePPX :: X PP -> Handle -> X PP
withLogHandlePPX xpp proc = do
    pp <- xpp
    return (withLogHandlePP pp proc)

myLogPP = def
  { ppCurrent = statusBarColor myNormalBorderColor statusColorNormalFG
  , ppHidden = statusNormalColor
  , ppHiddenNoWindows = const ""
  , ppUrgent = statusBarColor myNormalBorderColor myUrgentColor
  , ppTitle = statusNormalColor . shorten 120
  , ppLayout = statusNormalColor . myLayoutDisplay
  , ppSep = statusNormalColor " │ "
  , ppWsSep = statusNormalColor " "
  }

dropcolon :: String -> String
dropcolon s = if takeWhile (/= ':') s == s
              then s
              else drop 1 $ dropWhile (/= ':') s

myLogHook :: Handle -> X ()
myLogHook statusproc = do
    raiseWindowByClass "Dunst"
    myDynamicLogWithPP (withLogHandlePP myLogPP statusproc)

wrapIn :: String -> String -> String
wrapIn q string = q ++ string ++ q

quote :: String -> String
quote = wrapIn "'"

arg :: String -> String -> [String]
arg flag value = [("-" ++ flag), quote value]

externalStatusCmd :: String
externalStatusCmd = let
    fg = statusColorNormalFG
    bg = statusColorBG
    cmd arg0 args = unwords $ [arg0] ++ args
    status = cmd "status" $ quote <$> [fg, bg]
    dzenOptions = ["-dock", "-ta", "r", "-w", "800", "-x", "972"]
    fontName = "DejaVu Sans Mono"
    flags = [("fn", fontName), ("fg", fg), ("bg", bg)]
    displayOptions = (unwords . uncurry arg) <$> flags
    dzen2 = cmd "dzen2" $ dzenOptions ++ displayOptions
        in status ++ " | " ++ dzen2

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = ewmhDesktopsStartup

------------------------------------------------------------------------
-- Shutdown hook

-- Perform an arbitrary action each time xmonad exits.
-- Normal shutdown occurs by not catching an ExitSuccess exception.
-- For the hook, first run the action, then rethrow the error.
myShutdownHook :: IO () -> E.SomeException -> IO ()
myShutdownHook pre e = pre >> E.throw e

-- |Kill the statusbar handlers by closing one's pipe and killing the other.
killStatusProcs :: Handle -> ProcessID -> IO ()
killStatusProcs h pid = do
    E.handle printException $ hFlush h
    E.handle printException $ hClose h
    pgid <- getPGID pid
    E.handle printException $ either E.throw (signalProcessGroup sigTERM) pgid
        where
            getPGID :: ProcessID -> IO (Either E.SomeException ProcessGroupID)
            getPGID = E.try . getProcessGroupIDOf
            printException :: E.SomeException -> IO ()
            printException = putStrLn . show

-- |Modifies an XConfig to install a handler for XMONAD_RESTART events.
withRestartHook :: IO () -> XConfig l -> XConfig l
withRestartHook handler conf@XConfig { handleEventHook = orig } = conf {
    handleEventHook = handleRestartEvent handler >> orig
}

-- |Run a hook when the restart message is received.
handleRestartEvent :: IO () -> Event -> X ()

-- Process ClientMessageEvent to check its type.
handleRestartEvent onrestart e@ClientMessageEvent {ev_message_type = msgT} = do
    restartAtom <- getAtom "XMONAD_RESTART"
    if (msgT == restartAtom)
       then io onrestart
       else return ()

-- Ignore everything else.
handleRestartEvent _ _ = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    homeDir <- getHomeDirectory
    statusproc <- spawnPipe $ statusBarProc (homeDir ++ "/.xmonad")
    barPid <- spawnPID externalStatusCmd
    let statusKiller = killStatusProcs statusproc barPid
    E.handle (myShutdownHook statusKiller) $ xmonad
           $ docks
           $ ewmh
           $ withRestartHook statusKiller
           $ withUrgencyHook NoUrgencyHook
           $ def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayoutHook,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook statusproc,
        startupHook        = myStartupHook
    }
