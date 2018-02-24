{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import Data.Bits
import Data.Default
import Graphics.X11
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run (spawnPipe)
import qualified Data.Map as M
import qualified XMonad.StackSet as W

myFgColor = "#6D9EAB"
myBgColor = "#202020"
myHighlightedFgColor = "#FFFFFF"
myHighlightedBgColor = myFgColor

---------------------
--Workspaces Colors--
---------------------
myCurrentWsFgColor = myHighlightedFgColor
myCurrentWsBgColor = "#DE4314"
myVisibleWsFgColor = "#DE4314"
myVisibleWsBgColor = "#303030"
myHiddenWsFgColor = "#909090"
myHiddenWsBgColor = "#A62806"
myHiddenEmptyWsFgColor = myHiddenWsFgColor
myHiddenEmptyWsBgColor = myBgColor
myUrgentWsFgColor = myBgColor
myUrgentWsBgColor = "#F0F0F0"
myTitleFgColor = myFgColor
-----------------
--Border Colors--
-----------------
myActiveBorderColor = myFgColor
myInactiveBorderColor = myBgColor
myBorderWidth = 2

myWorkspaces :: [String]
myWorkspaces = ["  1  ", "  2  ", "  3  ", "  4  ", "  5  ", "  6  ", "  7  ", "  8  ", "  9  ", "  0  ", "  +  "]

myLayout = avoidStruts $ smartBorders layouts
  where
    layouts         = tiled ||| full ||| threeCol ||| grid
    tiled           = renamed [Replace "Tall"] $ toggleReflect (Tall 1 0.03 0.5)
    threeCol        = renamed [Replace "Three"] $ toggleReflect (ThreeCol 1 (0.8/100) (1/3))
    grid            = Grid
    full            = Full
    toggleReflect l = toggleLayouts (reflectHoriz l) l

main :: IO ()
main = do p <- spawnPipe "xmobar"
          xmonad (conf p) {startupHook = setWMName "LG3D" }
  where
    conf h = ewmh $ withUrgencyHook NoUrgencyHook def
         { terminal = "xterm"
         , logHook = dynamicLogWithPP $ xmobarPP { ppOutput          = hPutStrLn h
                                                 , ppTitle           = xmobarColor myTitleFgColor "" . shorten 150
                                                 , ppCurrent         = xmobarColor myCurrentWsFgColor myCurrentWsBgColor
                                                 , ppVisible         = xmobarColor myVisibleWsFgColor myVisibleWsBgColor
                                                 , ppHidden          = xmobarColor myHiddenWsFgColor myHiddenWsBgColor
                                                 , ppHiddenNoWindows = xmobarColor myHiddenEmptyWsFgColor myHiddenEmptyWsBgColor
                                                 , ppUrgent          = xmobarColor myUrgentWsFgColor myUrgentWsBgColor . xmobarStrip
                                                 , ppSep             = xmobarColor myHiddenWsFgColor "" " :: "
           }
         , normalBorderColor  = "#334455"
         , focusedBorderColor = "#ff9900"
         , modMask = mod4Mask
         , workspaces = myWorkspaces
         , handleEventHook = docksEventHook
         , layoutHook = myLayout
         , manageHook = composeAll [ isDialog --> doCenterFloat
                                   , className =? "stalonetray" --> doIgnore
                                   , className =? "trayer"  --> doIgnore
                                   , className =? "Gimp"      --> doFloat
                                   , className =? "sun-applet-Main" --> doFloat
                                   , className =? "sun.applet.Main" --> doFloat
                                   , className =? "sun-plugin-navig-motif-Plugin" --> doFloat
                                   , className =? "Spotify" --> doShift "+" <+> doFloat
                                   ]
         , keys = \c -> mykeys c `M.union` keys def c
         }
    mykeys (XConfig {XMonad.modMask = modm}) = M.fromList $
                                               [ ((controlMask .|. modm, xK_Right), nextWS) -- C-=>
                                               , ((controlMask .|. modm, xK_Left),  prevWS) -- -=-
                                               , ((modm, xK_g ),   withFocused toggleBorder) -- mod-g
                                               , ((modm .|. controlMask, xK_space), sendMessage ToggleLayout) -- mod-space
                                               ] ++
                                               [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
                                               | (key, sc) <- zip [xK_w, xK_e, xK_r, xK_s] [0, 1, 2]
                                               , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]] ++
                                               [ ((modm, k), windows $ W.greedyView i)
                                               | (i, k) <- zip myWorkspaces workspaceKeys
                                               ]
                                               ++
                                               [ ((modm .|. shiftMask, k), windows $ W.shift i)
                                               | (i, k) <- zip myWorkspaces workspaceKeys
                                               ]
      where workspaceKeys = [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_plus]
