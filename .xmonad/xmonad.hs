--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import Superscripts

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.WorkspaceNames (getWorkspaceNames, renameWorkspace)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Layout.WindowNavigation
import XMonad.Util.Run
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

import System.Environment.FindBin
import GHC.IO.Handle.Types (Handle)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Prompt

import Graphics.X11.ExtraTypes.XF86

-- for myDynamicLogWithPP
import Codec.Binary.UTF8.String (encodeString)
import Data.List (intersperse, sortBy)
import Data.Maybe (isJust, catMaybes)
import XMonad.Util.NamedWindows
import XMonad.Util.WorkspaceCompare (getWsCompareByTag, WorkspaceSort)

import Control.Applicative (liftA)

import Data.Set (toList, fromList)

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

myPrompt = defaultXPConfig { bgColor = statusColorBG
                           , fgColor = statusColorNormalFG
                           , bgHLight = statusColorNormalFG
                           , fgHLight = statusColorBG
                           , borderColor = myNormalBorderColor
                           , position = Top
                           }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm, xK_Return), spawn $ "in-cwd " ++ XMonad.terminal conf)
    , ((mod1Mask, xK_Return), spawn $ "in-cwd " ++ XMonad.terminal conf)
    , ((mod4Mask, xK_Return), spawn $ "LC_ALL=en_US.UTF-8 in-cwd urxvt")

    -- root term
    , ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf ++ " -e sudo su - -s /bin/zsh -l")

    -- launch dmenu
    , ((mod4Mask,           xK_r     ), spawn "dmenu_run")
    , ((modm,               xK_space ), spawn "dmenu_run")
    , ((mod1Mask,           xK_space ), spawn "dmenu_run")

    -- other launchers
    ---- process monitoring
    , ((mod4Mask,           xK_h     ), spawn $ XMonad.terminal conf ++ " -e htop")
    , ((mod4Mask,           xK_i     ), spawn $ XMonad.terminal conf ++ " -e sudo iotop")

    ---- web browsers
    , ((modm,               xK_c     ), spawn "chromium")
    , ((mod4Mask,           xK_c     ), spawn "chromium --incognito")

    ---- ssh
    , ((modm,               xK_u     ), spawn "ssh-choose -r -1")
    , ((modm,               xK_slash ), spawn "ssh-choose -r")

    ---- screensaver/sleep
    , ((modm,               xK_Scroll_Lock), spawn "xscreensaver-command -activate")
    , ((mod4Mask,           xK_Scroll_Lock), spawn "xscreensaver-command -activate")
    , ((mod4Mask,           xK_l     ), spawn "xscreensaver-command -activate")
    , ((0,                  xF86XK_ScreenSaver), spawn "xscreensaver-command -activate")
    , ((0,                  xF86XK_Sleep), spawn "slp")

    ---- screenshot
    , ((0,                  xK_Print ), spawn "screenshot")

    ---- xmms2
    , ((0,                  xF86XK_AudioPlay), spawn "nyxmms2 play")
    , ((shiftMask,          xF86XK_AudioPlay), spawn "nyxmms2 pause")
    , ((0,                  xF86XK_AudioStop), spawn "nyxmms2 stop")
    , ((shiftMask,          xF86XK_AudioStop), spawn "nyxmms2 quit")
    , ((0,                  xF86XK_AudioNext), spawn "nyxmms2 next")
    , ((0,                  xF86XK_AudioPrev), spawn "nyxmms2 prev")

    ---- brightness
    , ((modm, xK_F5), spawn "brightness down")
    , ((modm .|. shiftMask, xK_F5), spawn "brightness min")
    , ((modm, xK_F6), spawn "brightness up")
    , ((modm .|. shiftMask, xK_F6), spawn "brightness max")

    ---- choose wifi
    , ((mod4Mask, xK_w), spawn "wifi-chooser \"<cli>\"")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((mod4Mask,           xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((mod4Mask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Swap the focused window and the master window
    , ((mod4Mask .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Push window back into tiling
    , ((mod4Mask,           xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm .|. shiftMask, xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm .|. shiftMask, xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile && xmonad --restart")
    ]
    ++

    -- Directional movement
    [ ((modm, xK_h), sendMessage $ Go L)
    , ((modm, xK_j), sendMessage $ Go D) -- probably broken to do this...
    , ((modm, xK_j), focusDown)          -- ...and this
    , ((modm, xK_k), sendMessage $ Go U) -- probably broken to do this...
    , ((modm, xK_k), focusUp)            -- ...and this
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
    , ((modm, xK_space), toggleFocusFloat)
    , ((modm .|. shiftMask, xK_space), withFocused $ windows . W.sink)
    ]
    ++

    -- named workspaces
    [ ((modm, xK_t), renameWorkspace myPrompt)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
        {- , (m == 0 || (not $ k `elem` [xK_9, xK_0]) ) -} ] -- exclude M-S-(, M-S-)
    ++

    -- Grid Select
    [ ((mod4Mask, xK_g), goToSelected defaultGSConfig) ]
    ++

    -- add alt-i = workspace 7 (= "IM")
    [ ((modm, xK_i), windows $ W.greedyView "7")
    , ((modm .|. shiftMask, xK_i), windows $ W.shift "7")
    ]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    concat $ map (\modm ->
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w))
    ]) $ toList $ fromList [modm, mod1Mask]

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
myLayout = windowNavigation (FS.fullscreenFocus $ myWmii ||| wmiiLike)
  where
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

     wmiiLike = avoidStruts $ wmii shrinkText defaultTheme

     -- myWmii is basically the same as stock wmii,
     -- but it defaults to tabs first
     myWmii = avoidStruts $ group innerLayout zoomRowG
        where column = named "Column" $ Tall 0 delta ratio
              tabs = named "Tabs" $ Simplest
              innerLayout = renamed [CutWordsLeft 3]
                            $ addTabs shrinkText defaultTheme
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
myManageHook = FS.fullscreenManageHook <+> manageDocks <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Pinentry" --> doFloat
    , className =? "Pavucontrol" --> doFloat
    , className =? "sun-awt-X11-XFramePeer" --> doFloat
    , className =? "java-lang-Thread" --> doFloat
    , className =? "xfreerdp" --> unfloat
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
myDynamicLogWithPP pp = myDynamicLogString pp >>= XMonad.io . ppOutput pp

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

  -- layout description
  let ld = description . W.layout . W.workspace . W.current $ winset

  -- workspace list
  let ws = myPprWindowSet sort' urgents pp winset
  -- let ws = show ((ppSort pp) $ map W.tag $ W.hidden $ winset)
  -- let ws = show (W.allWindows winset)
  -- let ws = show $ map length $ map W.tag $ W.hidden $ winset
  -- let ws = show $ map (\x -> ((length . W.integrate' . W.stack) x, W.tag x) :: (Int,String)) $ sortBy getWsCompareByTag $ (W.workspaces winset)

  -- window title
  wt <- maybe (return "") (fmap show . getName) . W.peek $ winset

  -- run extra loggers, ignoring any that generate errors.
  extras <- mapM (flip catchX (return Nothing)) $ ppExtras pp

  return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
         [ ws
         , ppLayout pp ld
         , ppTitle  pp wt
         ]
         ++ catMaybes extras

statusLogHook :: Handle -> X ()
statusLogHook statusproc = myDynamicLogWithPP defaultPP
  { ppOutput = hPutStrLn statusproc . escapeStatusCodes
  , ppCurrent = statusBarColor myNormalBorderColor statusColorNormalFG
  , ppHidden = statusNormalColor
  , ppHiddenNoWindows = statusBarColor statusColorSubdued statusColorBG
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

myWorkspaceNamesPP :: PP -> X PP
myWorkspaceNamesPP pp = do
   {-names <- liftM (>>> dropcolon) getWorkspaceNames-}
   names <- liftA ((.) dropcolon) getWorkspaceNames
   return $ pp { ppCurrent = ppCurrent pp . names
               , ppVisible = ppVisible pp . names
               , ppHidden = ppHidden pp . names
               , ppHiddenNoWindows = ppHiddenNoWindows pp . names
               , ppUrgent = ppUrgent pp . names
               }

myLogHook :: Handle -> X ()
myLogHook statusproc = myWorkspaceNamesPP defaultPP {
                       ppOutput = hPutStrLn statusproc
                       } >>= dynamicLogWithPP

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = ewmhDesktopsStartup

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmonadDir <- getProgPath
    statusproc <- spawnPipe $ statusBarProc xmonadDir
    barPid <- spawnPID $ "while sleep 1 ; do status '" ++ statusColorNormalFG ++ "' '" ++ statusColorBG ++ "' ; done"
                   ++ " | dzen2 -ta r -w 900 -x -964 -fn 'DejaVu Sans Mono'"
                   ++ " -fg '" ++ statusColorNormalFG ++ "' -bg '" ++ statusColorBG ++ "'"
    xmonad $ ewmh
           $ withUrgencyHook NoUrgencyHook
           $ defaultConfig {
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
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook statusproc,
        startupHook        = myStartupHook
    }
