-- Copyright (C) 2020-2025 Aditya Shakya <adi1090x@gmail.com>
--
-- Xmonad config for Archcraft 

-- ## Modules ## -------------------------------------------------------------------
import XMonad
import XMonad.Util.SpawnOnce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps

import Graphics.X11.ExtraTypes.XF86
import System.Exit
import Control.Monad
import Data.Monoid
import Data.Maybe

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.NoBorders

-- ## Startup hook ## ---------------------------------------------------------------
myStartupHook = do
    spawnOnce "bash ~/.config/xmonad/scripts/xmonad_autostart"

-- ## Settings ## -------------------------------------------------------------------

-- focus follows the mouse pointer
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- clicking on a window to focus
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- ** DO NOT CHANGE TEXT FORMAT ** --
-- ** Required for themes to work correctly ** --
--
-- Width of the window border in pixels
myBorderWidth = 1
-- Border colors for focused & unfocused windows
myFocusedBorderColor = "#b8543e"
myNormalBorderColor = "#141719"
--
-- ** ************************* ** --

-- myModMask : modkey you want to use
-- mod1Mask  : left alt Key
-- mod4Mask  : Windows or Super Key
myModMask = mod4Mask

-- Workspaces (ewmh)
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

-- ## Applications ## ---------------------------------------------------------------

-- Terminal
myTerminal :: String
myTerminal = "~/.config/xmonad/scripts/xmonad_term"

myTermKitty :: String
myTermKitty = "~/.config/xmonad/scripts/xmonad_kitty"

-- Apps
myFileManager    = "thunar"
myTextEditor     = "geany"
myWebBrowser     = "firefox"

-- CLI Apps
myCLIFileManager = myTerminal ++ " -e ranger"
myCLITextEditor  = myTerminal ++ " -e vim"
myCLIMonitor     = myTerminal ++ " -e htop"
myCLIMusic       = "~/.config/xmonad/scripts/xmonad_music"

-- Rofi Menus
myRofiPath :: String
myRofiPath = "~/.config/xmonad/scripts/"
myRofi_NETWORK   = myRofiPath ++ "network_menu"
myRofi_ROOT      = myRofiPath ++ "rofi_asroot"
myRofi_BLUETOOTH = myRofiPath ++ "rofi_bluetooth"
myRofi_LAUNCHER  = myRofiPath ++ "rofi_launcher"
myRofi_MUSIC     = myRofiPath ++ "rofi_music"
myRofi_POWER     = myRofiPath ++ "rofi_powermenu"
myRofi_RUNNER    = myRofiPath ++ "rofi_runner"
myRofi_SHOTS     = myRofiPath ++ "rofi_screenshot"
myRofi_WINDOW    = myRofiPath ++ "rofi_windows"

-- Functions Keys
myVolume         = "~/.config/xmonad/scripts/xmonad_volume"
myBrightness     = "~/.config/xmonad/scripts/xmonad_brightness"
myScreenshot     = "~/.config/xmonad/scripts/xmonad_screenshot"

-- Misc
myColorPicker    = "~/.config/xmonad/scripts/xmonad_colorpicker"
myLockScreen     = "betterlockscreen --lock"

-- ## Key Bindings ## -------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = super}) = M.fromList $

    -- Launch terminal : alacritty
    [ ((super,                      xK_Return),  spawn $ XMonad.terminal conf)
    , ((super .|. shiftMask,        xK_Return),  spawn $ myTerminal ++ " --float")
    , ((super .|. mod1Mask,         xK_Return),  spawn $ myTerminal ++ " --full")

    -- Launch terminal : kitty
    , ((controlMask .|. mod1Mask,        xK_t),  spawn myTermKitty)

    -- Launch applications
    , ((super .|. shiftMask,             xK_f),  spawn myFileManager)
    , ((super .|. shiftMask,             xK_e),  spawn myTextEditor)
    , ((super .|. shiftMask,             xK_w),  spawn myWebBrowser)

    -- Launch CLI applications
    , ((controlMask .|. mod1Mask,        xK_r),  spawn myCLIFileManager)
    , ((controlMask .|. mod1Mask,        xK_v),  spawn myCLITextEditor)
    , ((controlMask .|. mod1Mask,        xK_h),  spawn myCLIMonitor)
    , ((controlMask .|. mod1Mask,        xK_m),  spawn myCLIMusic)

    -- Launch rofi menus
    , ((mod1Mask,                       xK_F1),  spawn myRofi_LAUNCHER)
    , ((mod1Mask,                       xK_F2),  spawn myRofi_RUNNER)
    , ((super,                           xK_n),  spawn myRofi_NETWORK)
    , ((super,                           xK_r),  spawn myRofi_ROOT)
    , ((super,                           xK_b),  spawn myRofi_BLUETOOTH)
    , ((super,                           xK_m),  spawn myRofi_MUSIC)
    , ((super,                           xK_x),  spawn myRofi_POWER)
    , ((super,                           xK_s),  spawn myRofi_SHOTS)
    , ((super,                           xK_w),  spawn myRofi_WINDOW)

    -- Audio keys
    , ((0,                   xF86XK_AudioPlay),  spawn "mpc toggle")
    , ((0,                   xF86XK_AudioPrev),  spawn "mpc prev")
    , ((0,                   xF86XK_AudioNext),  spawn "mpc next")
    , ((0,                   xF86XK_AudioStop),  spawn "mpc stop")
    , ((0,            xF86XK_AudioRaiseVolume),  spawn $ myVolume ++ " --inc")
    , ((0,            xF86XK_AudioLowerVolume),  spawn $ myVolume ++ " --dec")
    , ((0,                   xF86XK_AudioMute),  spawn $ myVolume ++ " --toggle")
    , ((0,                xF86XK_AudioMicMute),  spawn $ myVolume ++ " --toggle-mic")

    -- Brightness keys
    , ((0,             xF86XK_MonBrightnessUp),  spawn $ myBrightness ++ " --inc")
    , ((0,           xF86XK_MonBrightnessDown),  spawn $ myBrightness ++ " --dec") 

    -- Screenshot keys
    , ((0,                           xK_Print),  spawn $ myScreenshot ++ " --now")
    , ((mod1Mask,                    xK_Print),  spawn $ myScreenshot ++ " --in5")
    , ((shiftMask,                   xK_Print),  spawn $ myScreenshot ++ " --in10")
    , ((controlMask,                 xK_Print),  spawn $ myScreenshot ++ " --win")
    , ((super,                       xK_Print),  spawn $ myScreenshot ++ " --area")

    -- Misc
    , ((super,                           xK_p),  spawn myColorPicker)

    -- Lockscreen
    , ((mod1Mask .|. controlMask,        xK_l),  spawn myLockScreen)

    -- Close focused window
    , ((super,                           xK_c),  kill)
    , ((super,                      xK_Escape),  spawn "xkill")
    
    -- Window Manager Specific -----------------------------------------

    -- Change gaps on the fly
    , ((super .|. controlMask,           xK_g),  sendMessage $ ToggleGaps)                                -- toggle all gaps
    , ((super .|. shiftMask,             xK_g),  sendMessage $ setGaps [(L,50), (R,50), (U,80), (D,50)])  -- reset the GapSpec
    
    , ((super .|. controlMask,           xK_t),  sendMessage $ IncGap 10 L)  -- increment the left-hand gap
    , ((super .|. shiftMask,             xK_t),  sendMessage $ DecGap 10 L)  -- decrement the left-hand gap
    
    , ((super .|. controlMask,           xK_y),  sendMessage $ IncGap 10 U)  -- increment the top gap
    , ((super .|. shiftMask,             xK_y),  sendMessage $ DecGap 10 U)  -- decrement the top gap

    , ((super .|. controlMask,           xK_u),  sendMessage $ IncGap 10 D)  -- increment the bottom gap
    , ((super .|. shiftMask,             xK_u),  sendMessage $ DecGap 10 D)  -- decrement the bottom gap

    , ((super .|. controlMask,           xK_i),  sendMessage $ IncGap 10 R)  -- increment the right-hand gap
    , ((super .|. shiftMask,             xK_i),  sendMessage $ DecGap 10 R)  -- decrement the right-hand gap

    -- Rotate through the available layout algorithms
    , ((super .|. controlMask,       xK_space),  sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((super .|. shiftMask,         xK_space),  setLayout $ XMonad.layoutHook conf)

    -- Toggle fullscreen
    , ((super,                           xK_f),  toggleFull >> spawn "polybar-msg cmd toggle")

    -- Toggle between floating and tiling
    , ((super,                       xK_space),  withFocused toggleFloat)

    -- Resize viewed windows to the correct size
    , ((super .|. shiftMask,             xK_r),  refresh)

    -- Move focus to the master window
    , ((super .|. mod1Mask,              xK_m),  windows W.focusMaster)

    -- Swap the focused window and the master window
    , ((super .|. shiftMask,             xK_m),  windows W.swapMaster)

    -- Move focus to the next window
    , ((super,                         xK_Tab),  windows W.focusDown)

    -- Move focus to the next window
    , ((super,                           xK_j),  windows W.focusDown)
    , ((super,                        xK_Left),  windows W.focusDown)

    -- Move focus to the previous window
    , ((super,                           xK_k),  windows W.focusUp)
    , ((super,                       xK_Right),  windows W.focusUp)

    -- Swap the focused window with the next window
    , ((super .|. shiftMask,             xK_j),  windows W.swapDown)
    , ((super .|. shiftMask,          xK_Left),  windows W.swapDown)

    -- Swap the focused window with the previous window
    , ((super .|. shiftMask,             xK_k),  windows W.swapUp)
    , ((super .|. shiftMask,         xK_Right),  windows W.swapUp)

    -- Shrink the master area
    , ((super,                           xK_h),  sendMessage Shrink)
    , ((super .|. controlMask,        xK_Left),  sendMessage Shrink)

    -- Expand the master area
    , ((super,                           xK_l),  sendMessage Expand)
    , ((super .|. controlMask,       xK_Right),  sendMessage Expand)

    -- Increment the number of windows in the master area
    , ((super,                       xK_comma),  sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((super,                      xK_period),  sendMessage (IncMasterN (-1)))

    -- Restart xmonad
    , ((super,                           xK_q),  spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((super .|. shiftMask,         xK_slash),  spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    ]
    ++

    -- Workspace Specific ---------------------------------------------------------------

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. super, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{y,u,i}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{y,u,i}, Move client to screen 1, 2, or 3
    [((m .|. super, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_y, xK_u, xK_i] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- ## Mouse Bindings ## ------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = super}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((super, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((super, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((super, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

-- ## Layouts ## -------------------------------------------------------------------------
myLayout = avoidStruts(tiled ||| Mirror tiled ||| Full)
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta ratio
        
        -- The default number of windows in the master pane
        nmaster = 1
        
        -- Default proportion of screen occupied by master pane
        ratio   = 1/2
        
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100

-- ## Window rules ## --------------------------------------------------------------------
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doCenterFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    ]
    where
        myCFloats = ["alacritty-float", "kitty-float", "Music", "MPlayer", "mpv",
                    "Gimp", "feh", "Viewnior", "Gpicview",
                    "Kvantum Manager", "qt5ct", "VirtualBox Manager", "qemu", "Qemu-system-x86_64",
                    "Lxappearance", "Nitrogen", "Arandr", "Pavucontrol", "Xfce4-power-manager-settings", "Nm-connection-editor"]
        myTFloats = ["Downloads", "Save As...", "About : Aditya Shakya", "Getting Started"]
        myRFloats = []
        myIgnores = ["desktop_window"]

-- ## Misc ## -----------------------------------------------------------------------------
addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

-- toggle b/w tiling and floating (for keybinding)
toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else (W.float w (W.RationalRect (1/4) (1/10) (1/2) (4/5)) s))

-- toggle fullscreen (for keybinding)
toggleFull = withFocused (\windowId -> do
{
   floats <- gets (W.floating . windowset);
   if windowId `M.member` floats
   then do
       withFocused $ toggleBorder
       withFocused $ windows . W.sink
   else do
       withFocused $ toggleBorder
       withFocused $  windows . (flip W.float $ W.RationalRect 0 0 1 1)})

-- ## Event handling ## -------------------------------------------------------------------
myEventHook = mempty

-- ## Logging ## --------------------------------------------------------------------------
myLogHook   = return ()

-- ## Main Function ## --------------------------------------------------------------------

-- Run xmonad with all the configs we set up.
main = xmonad $ fullscreenSupport $ docks $ ewmh defaults

defaults = def {
      -- configs
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        manageHook         = myManageHook,
        layoutHook         = gaps [(L,0), (R,0), (U,0), (D,0)] $ spacingRaw False (Border 10 0 10 0) True (Border 0 10 0 10) True $ myLayout,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook >> addEWMHFullscreen
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'super'. Default keybindings:",
    "",
    "-- Launching applications",
    "super + Enter            Launch terminal (alacritty)",
    "super + shift + Enter    Launch terminal in floating mode",
    "super + alt + Enter      Launch terminal in fullscreen mode",
    "super + shift + f        Launch file manager (thunar)",
    "super + shift + e        Launch text editor (geany)",
    "super + shift + w        Launch web browser (firefox)",
    "ctrl + alt + r           Launch ranger in terminal",
    "ctrl + alt + v           Launch vim in terminal",
    "ctrl + alt + h           Launch htop in terminal",
    "ctrl + alt + m           Launch ncmpcpp with album art in terminal",
    "",
    "-- Launching rofi applets",
    "alt + F1                 Open rofi app launcher",
    "alt + F2                 Open rofi command runner",
    "super                    Open rofi app launcher",
    "super + n                Open network menu",
    "super + r                Open as_root applet",
    "super + b                Open bluetooth applet",
    "super + m                Open music (mpd) applet",
    "super + x                Open powermenu applet",
    "super + s                Open screenshot applet",
    "super + w                Open window applet",
    "",
    "-- Taking screenshots",
    "print                    Take screenshot",
    "alt + print              Take screenshot in 5s",
    "shift + print            Take screenshot in 10s",
    "ctrl + print             Take screenshot of focused window",
    "super + print            Take screenshot of selected area",
    "",
    "-- Misc",
    "super + p                Run colorpicker",
    "ctrl + alt + l           Trigger lockscreen",
    "super + c                Close focused window",
    "super + Escape           Run xkill",
    "",
    "-- Layout",
    "super + f                Toggle fullscreen",
    "super + space            Toggle between floating and tiling",
    "super + ctrl + space     Rotate through the available layout algorithms",
    "super + shift + space    Reset the layouts on the current workSpace to default",
    "super + shift + r        Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "super + Tab              Move focus to the next window",
    "super + j/left           Move focus to the next window",
    "super + k/right          Move focus to the previous window",
    "super + shift + m        Swap the focused window and the master window ",
    "super + alt + m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "super + Shift + j/left   Swap the focused window with the next window",
    "super + Shift + k/right  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "super + h                Shrink the master area",
    "super + ctrl + left      Shrink the master area",
    "super + l                Expand the master area",
    "super + ctrl + right     Expand the master area",
    "",
    "-- increase or decrease number of windows in the master area",
    "super + comma            Increment the number of windows in the master area",
    "super + period           Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "super + q                Restart xmonad",
    "super + shift + /        Open this help window",
    "",
    "-- Workspaces & screens",
    "super + [1..9]           Switch to workSpace N",
    "super + Shift + [1..9]   Move client to workspace N",
    "super + {y,u,i}          Switch to physical/Xinerama screens 1, 2, or 3",
    "super + Shift + {y,u,i}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "super + button1          Set the window to floating mode and move by dragging",
    "super + button2          Raise the window to the top of the stack",
    "super + button3          Set the window to floating mode and resize by dragging"]
