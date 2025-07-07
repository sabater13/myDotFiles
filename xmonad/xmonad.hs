import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
-- NOTE: Importing XMonad.Util.Ungrab is only necessary for versions
-- < 0.18.0! For 0.18.0 and up, this is already included in the
-- XMonad import and will generate a warning instead!
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (spawnPipe)

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
--import XMonad.Layout.NoBorders
-- Remove border from fullscreened applications
import XMonad.Layout.NoBorders    (Ambiguity(OnlyScreenFloat), lessBorders)
-- Dynamically apply transformers (such as FULL, MIRROR, NOBORDERS) to
-- base layouts.
import XMonad.Layout.MultiToggle (Toggle(Toggle), single, mkToggle)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(MIRROR, NBFULL))
import XMonad.Layout.ThreeColumns (ThreeCol(ThreeCol))
import XMonad.Layout.Grid         (Grid(Grid))
import XMonad.Layout.TwoPane      (TwoPane(TwoPane))

--import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.WorkspaceNames

import XMonad.Prompt
import XMonad.Prompt.Workspace

import XMonad.StackSet

import XMonad.Config.Desktop (desktopLayoutModifiers)

import Graphics.X11.ExtraTypes.XF86

import System.Process (readProcess)


main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig


myConfig = def
    { modMask    = mod4Mask      -- modMask=Alt, mod2Mask=?, mod3Mask=?, mod4Mask=Windows  
    , layoutHook = myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    , startupHook = myStartupHook
    , focusedBorderColor = myFocusedBorderColour
    , XMonad.workspaces = words "music mail"
    }
  `additionalKeysP`
    [ ("M-S-l", spawn "xscreensaver-command -lock")
    , ("M-C-b", spawn "brave-browser")
    --, ("M-f"  , spawn "firefox")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+")
    , ("<XF86AudioMute>", spawn "amixer set Master 100%-")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 5%-")
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl s +5%")
    , ("<XK_3270_PrintScreen>", spawn "scrot -s")
    , ("M-n", makeWorkspace)  -- Create/switch to named Workspace
    , ("M-<Delete>", removeEmptyWorkspace) -- remove empty workspace
    , ("M-C-e", spawn "nautilus")
    , ("M-C-n", withWorkspace myXPConfig sendX) -- send to named workspace
    , ("M-f", sendMessage $ XMonad.Layout.MultiToggle.Toggle NBFULL)
    , ("M-C-k", spawn ".local/kitty.app/bin/kitty")
    ]

makeWorkspace :: X ()
makeWorkspace = selectWorkspace myXPConfig { autoComplete = Nothing }
sendX = windows . shift

myXPConfig :: XPConfig
myXPConfig = def
    { bgColor = "orange"
    , fgColor = "blue"
    , font = "xft:UnknownPicksDefault:pixelsize=20:weight=bold"
    , height = 28
    , promptBorderWidth   = 1
    , position            = Top
    , historySize         = 100
    , autoComplete        = Just 100000  -- microseconds
    }

-- From Max's file
-- When using the fullscreen mode of just about any application (XBMC
-- being a notable exception) the fullscreened window got a
-- border. This gets rid of it.
myLayout = lessBorders OnlyScreenFloat $ myLayout'

myLayout' = desktopLayoutModifiers
  . mkToggle (single NBFULL) -- toggle fullscreen the active window
  $ (TwoPane (3/100) (1/2)) ||| three ||| tiled ||| Grid
  where

    three = ThreeCol nmaster delta ratio

    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percentage of screen to increment by when resizing panes
    delta   = 3/100

-- Original layout
--myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  --where
    --threeCol = ThreeColMid nmaster delta ratio
    --tiled    = Tall nmaster delta ratio
    --nmaster  = 1      -- Default number of windows in the master pane
    --ratio    = 1/2    -- Default proportion of screen occupied by master pane
    --delta    = 3/100  -- Percent of screen to increment by when resizing panes

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "nautilus" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "Firefox" <&&> resource =? "Toolkit" --> doFloat
    , isDialog            --> doFloat
    , isFullscreen --> doFullFloat
    ]

myFocusedBorderColour = "#1D8348"

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = yellow " • "
    , ppTitleSanitize   = xmobarStrip
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . green . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . darkgreen    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 20

    green, lowWhite, darkgreen, red, white, yellow :: String -> String
    darkgreen = xmobarColor "#2AAE08" ""
    green     = xmobarColor "#48FB2D" ""
    white     = xmobarColor "#f8f8f2" ""
    yellow    = xmobarColor "#f1fa8c" ""
    red       = xmobarColor "#ff5555" ""
    lowWhite  = xmobarColor "#bbbbbb" ""

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "feh --bg-fill --no-fehbg ~/.wallpapers/Bristol-F2b_Fighter.jpg"
  --liftIO spawnXmobarOnAllScreens  -- Spawn xmobar on all screens
  --spawnOnce "xmobar ~/.xmobarrc"

-- Function to spawn xmobar on all connected screens
spawnXmobarOnAllScreens :: IO ()
spawnXmobarOnAllScreens = do
  -- Detect the number of connected screens using `xrandr`
  result <- readProcess "xrandr" ["--listmonitors"] ""
  let screenCount = length (lines result) - 1  -- First line is just the "Monitors" header
  -- Spawn xmobar on each screen
  mapM_ (\i -> spawnPipe $ "xmobar -x " ++ show i) [0..screenCount-1]