{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import RIO
import RIO.List

import Data.Monoid

import System.Directory
import System.FilePath ((</>))
import System.IO (appendFile)
import System.Posix.Files

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WallpaperSetter
import XMonad.Layout.LayoutModifier
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Util.Cursor
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W

data GlobalConfig = GlobalConfig
  { wsLogFile :: FilePath
  } deriving Typeable

instance ExtensionClass GlobalConfig where
  initialValue = GlobalConfig
    { wsLogFile = mempty
    }

main :: IO ()
main = xmonad . ewmh . docks $ myConfig

myConfig :: XConfig MyLayout
myConfig = def
   { terminal        = myTerminal
   , modMask         = myModMask
   , workspaces      = myWorkspaces
   , borderWidth     = myBorderWidth
   , layoutHook      = myLayout
   , manageHook      = myManageHook
   , logHook         = myLogHook
   , handleEventHook = myHandleEventHook
   , startupHook     = myStartupHook
   } `additionalKeysP` myKeysP `removeMouseBindings` myKeysToRemove

myTerminal :: String
myTerminal = "alacritty"

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 0

border :: Border
border = (\x -> Border x x x x) 4

type MyLayout = ModifiedLayout AvoidStruts (ModifiedLayout Spacing (Choose ResizableTall Full))

myLayout :: Eq a => MyLayout a
myLayout = avoidStruts $ spaceSetting layout
  where
    spaceSetting = spacingRaw False border True border True
    layout       = mode1 ||| mode2
    mode1        = ResizableTall 1 0.01 0.5 []
    mode2        = Full

myBrowser :: String
myBrowser = "chromium"

openBrowser, openBrowser', appLauncher, screenShot, screenShot', reStart, reCompile :: X ()
openBrowser  = spawn myBrowser
openBrowser' = spawn $ myBrowser <> " --incognito"
appLauncher  = spawn "rofi -show run"
screenShot   = spawn "screenshot.sh 0.7 60"
screenShot'  = spawn "screenshot.sh 0.7 60 --focused"
reStart      = spawn "xmonad --restart"
reCompile    = spawn "xmonad --recompile && xmonad --restart"

bLight, amixer :: String -> X ()
bLight = spawn . ("xbacklight " ++) . (++ " -time 1")
amixer = spawn . ("amixer set Master " ++)

myKeysP :: [(String, X ())]
myKeysP = [ ( "M-u"
            , spawn myTerminal
            )
          , ( "M-s"
            , openBrowser
            )
          , ( "M-S-s"
            , openBrowser'
            )
          , ( "M-p"
            , appLauncher
            )
          , ( "<Print>"
            , screenShot
            )
          , ( "S-<Print>"
            , screenShot'
            )
          , ( "M-S-m"
            , windows W.swapMaster
            )
          , ( "M-h"
            , moveTo Prev NonEmptyWS
            )
          , ( "M-l"
            , moveTo Next NonEmptyWS
            )
          , ( "M-S-l"
            , moveTo Next EmptyWS
            )
          , ( "M-S-h"
            , moveTo Prev EmptyWS
            )
          , ( "M-<Tab>"
            , nextScreen
            )
          , ( "M-S-<Tab>"
            , prevScreen
            )
          , ( "M-<L>"
            , sendMessage Shrink
            )
          , ( "M-<R>"
            , sendMessage Expand
            )
          , ( "M-<U>"
            , sendMessage MirrorExpand
            )
          , ( "M-<D>"
            , sendMessage MirrorShrink
            )
          , ("M-0"
            , do setScreenSpacing border
                 setWindowSpacing border
            )
          , ( "M-S-="
            , decScreenWindowSpacing 4
            )
          , ( "M--"
            , incScreenWindowSpacing 4
            )
          , ( "M-<XF86ApplicationRight>"
            , bLight "+5"
            )
          , ( "<XF86MonBrightnessUp>"
            , bLight "+5"
            )
          , ( "<XF86MonBrightnessDown>"
            , bLight "-5"
            )
          , ( "S-<XF86MonBrightnessUp>"
            , bLight "+100"
            )
          , ( "S-<XF86MonBrightnessDown>"
            , bLight "-100"
            )
          , ( "<XF86AudioRaiseVolume>"
            , amixer "1%+"
            )
          , ( "<XF86AudioLowerVolume>"
            , amixer "1%-"
            )
          , ( "<XF86AudioMute>"
            , amixer "toggle"
            )
          , ( "M-S-c"
            , mempty
            )
          , ( "M-<Return>"
            , mempty
            )
          , ( "M-S-<Return>"
            , mempty
            )
          , ( "M-S-r"
            , reCompile
            )
          , ( "M-r"
            , reStart
            )
          , ( "M-S-q"
            , mempty
            )
          , ( "M-q"
            , mempty
            )
          , ( "M-c"
            , kill
            )
          ]

myKeysToRemove :: [(ButtonMask, Button)]
myKeysToRemove = [ (mod4Mask, button1)
                 , (mod4Mask, button2)
                 , (mod4Mask, button3)
                 ]

myHandleEventHook :: Event -> X All
myHandleEventHook = composeAll
         [ handleEventHook def
         , fullscreenEventHook
         , ewmhDesktopsEventHook
         ]

myManageHook :: ManageHook
myManageHook = composeAll
         [ className =? "feh"              --> doCenterFloat
         , className =? "jetbrains-studio" --> doFloat
         , className =? "jetbrains-idea"   --> doFloat
         , className =? "Galculator"       --> doCenterFloat
         , isFullscreen                    --> doFullFloat
         , isDialog                        --> doCenterFloat
         ]

data WsState
  = Current
  | NotEmpty
  | Empty

getWsLog :: X String
getWsLog = do
      winset <- gets windowset
      let idx = W.currentTag winset
          wss = W.workspaces winset
          rawState = sortOn fst $ zip (map W.tag wss) (map W.stack wss)
      return . join . map (stateToSym . toState idx) $ rawState
      where
         toState current (idx, mw)
            | current == idx = Current
            | otherwise      = bool Empty NotEmpty . isJust $ mw

         stateToSym = \case
           Current  -> "\63022"
           NotEmpty -> "\61842"
           Empty    -> "\63023"

writeWsLog :: X ()
writeWsLog = do
  filename <- XS.gets wsLogFile
  io . appendFile filename . (++ "\n") =<< getWsLog

myLogHook :: X ()
myLogHook = writeWsLog

myStartupHook :: X ()
myStartupHook = do
  XS.put $ GlobalConfig
    { wsLogFile = "/tmp/.xmonad-workspace-log"
    }
  homeDir <- io getHomeDirectory
  setDefaultCursor xC_left_ptr
  setWMName "LG3D"
  wallpaperSetter $ mkWallpaperConf homeDir

createWsLogFile :: X ()
createWsLogFile = do
  file <- XS.gets wsLogFile
  io $ unlessM (doesFileExist file) $ createNamedPipe file stdFileMode

mkWallpaperConf :: FilePath -> WallpaperConf
mkWallpaperConf homeDir = WallpaperConf
  { wallpaperBaseDir = baseDir
  , wallpapers = WallpaperList $ map ( , WallpaperFix name ) myWorkspaces
  } where
    baseDir = homeDir </> "Pictures"
    name = ".xmonad-wallpaper"

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 7 :: Int]
