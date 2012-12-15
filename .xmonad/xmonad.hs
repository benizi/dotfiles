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
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Util.Run
import Data.Monoid
import System.Exit

import System.Environment.FindBin

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Graphics.X11.ExtraTypes.XF86

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
myModMask       = mod1Mask

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

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ "in-cwd " ++ XMonad.terminal conf)
    , ((mod4Mask, xK_Return), spawn $ "in-cwd urxvt-256color")

    -- root term
    , ((modm .|. shiftMask, xK_Return), spawn "st -e sudo su - -s /bin/zsh -l")

    -- launch dmenu
    , ((mod4Mask,           xK_r     ), spawn "dmenu_run")
    , ((modm,               xK_space ), spawn "dmenu_run")

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

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((mod4Mask .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((mod4Mask,           xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm .|. shiftMask, xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm .|. shiftMask, xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile && xmonad --restart")
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

    -- add alt-i = workspace 7 (= "IM")
    [ ((modm, xK_i), windows $ W.greedyView "7")
    , ((modm .|. shiftMask, xK_i), windows $ W.shift "7")
    ]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

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
myLayout = FS.fullscreenFocus $ avoidStruts $ Full ||| tiled ||| GridRatio (8/2)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

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
    , className =? "Huludesktop" --> doFloat
    , className =? "Pavucontrol" --> doFloat
    , className =? "sun-awt-X11-XFramePeer" --> doFloat
    , className =? "java-lang-Thread" --> doFloat
    ]

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
myLogHook = takeTopFocus

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = ewmhDesktopsStartup

myLayoutDisplay :: String -> String
myLayoutDisplay "Tall" = "[]="
myLayoutDisplay "Full" = "[M]"
myLayoutDisplay other = wrap "(layout:" ")" other

myActiveMarker = wrap "" (superScriptNum 0)

statusBarProc :: String -> String
statusBarProc xmonadDir = "xmobar " ++ xmonadDir ++ "/xmobarrc"
statusBarColor = xmobarColor

statusBarTitle :: String -> String
statusBarTitle title = foldl (\acc c -> acc ++ case c of
 '{' -> "("
 '}' -> ")"
 a -> [a]) [] title

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmonadDir <- getProgPath
    statusproc <- spawnPipe $ statusBarProc xmonadDir
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
        logHook            = dynamicLogWithPP defaultPP
                                 { ppOutput = hPutStrLn statusproc . statusBarTitle
                                 , ppCurrent = statusBarColor myNormalBorderColor "white" . myActiveMarker
                                 , ppHidden = statusBarColor "white" "" . myActiveMarker
                                 , ppHiddenNoWindows = statusBarColor "gray60" ""
                                 , ppUrgent = statusBarColor myNormalBorderColor myUrgentColor
                                 , ppTitle = statusBarColor "white" "" . shorten 120
                                 , ppLayout = myLayoutDisplay
                                 , ppSep = " │ "
                                 , ppWsSep = " "
                                 } <+> myLogHook,
        startupHook        = myStartupHook
    }
