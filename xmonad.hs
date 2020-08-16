import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Layout.ThreeColumns
import           XMonad.Util.Run                ( spawnPipe
						, hPutStrLn
						)
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.NoBorders        ( noBorders )
import           XMonad.Layout.Gaps
import           XMonad.Layout.Spacing          ( smartSpacing )
import           XMonad.Layout.Fullscreen       ( fullscreenSupport
						, fullscreenFocus
						)
import           XMonad.Util.Cursor
import qualified Data.Map                      as M
import           XMonad.Util.EZConfig
import           XMonad.Actions.Promote
import           XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet               as W

myGaps = smartSpacing gap . gaps gapSides
  where
    gap = 12
    gapSides = zip [U,D,L,R] (repeat gap)

myLayout = fullscreenFocus $ myGaps $ noBorders $ avoidStruts $ ThreeColMid
  1
  (3 / 100)
  (1 / 2)

myKeyBindings = (`removeKeysP` blacklist) . (`additionalKeysP` whitelist)
 where
  whitelist =
    [ ("M-<Return>", spawn "alacritty")
    , ("<F2>", spawn "pickwallpaper")
    , ("<XF86AudioNext>", spawn "quodlibet --next")
    , ("<XF86AudioPrev>", spawn "quodlibet --previous")
    , ("<XF86AudioPlay>", spawn "quodlibet --play-pause")
    , ("<F3>", spawn "picom-trans -c 80")
    , ("S-<F3>", spawn "picom-trans -c 100")
    ] ++ scratchpadsKeybindings myScratchpads

  blacklist =
    [ "M-S-<Return>" -- Terminal bound to M-<Return>
    , "M-."
    , "M-,"
    ]

centeredFloat w h = customFloating $ W.RationalRect ((50 - w / 2) / 100)
						    ((50 - h / 2) / 100)
						    (w / 100)
						    (h / 100)

myScratchpads =
  [ ( NS "htop" "alacritty --title htop -e htop" (title =? "htop") terminalFloat
    , "<F1>"
    )
  , ( NS "quodlibet" "quodlibet" (className =? "Quodlibet") mediaFloat
    , "M-m"
    )
  , ( NS "terminal"
	 "alacritty --title ScratchpadAlacritty"
	 (title =? "ScratchpadAlacritty")
	 terminalFloat
    , "M-x"
    )
  ]
  where
    terminalFloat = centeredFloat 50 60
    mediaFloat = centeredFloat 70 80


scratchpads = map fst myScratchpads
scratchpadsKeybindings :: [(NamedScratchpad, String)] -> [(String, X ())]
scratchpadsKeybindings nskbd = map processNS nskbd
  where processNS (ns, kb) = (kb, namedScratchpadAction scratchpads (name ns))

myLogHook barPipe =
  dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ def
    { ppCurrent         = wrap "[" "]"
    , ppTitle           = shorten 136
    , ppUrgent          = id
    , ppHiddenNoWindows = const ""
    , ppSep             = wrap " " " " $ xmobarColor "#FFFFFF" "#880E4F" "<>"
    , ppWsSep           = " "
    , ppLayout          = const ""
    , ppOrder           = id
    , ppOutput          = hPutStrLn barPipe
    , ppExtras          = []
    }
myStatusBar = "xmobar -x0 /home/raven/.xmonad/xmobar.conf"

myConfig barPipe = myKeyBindings $ def
  { modMask       = mod4Mask
  , terminal      = "alacritty"
  , layoutHook    = myLayout
  , logHook       = myLogHook barPipe
  , workspaces    = ["GEN", "WRK", "SYS"] ++ (map show [4 .. 9])
  , manageHook    = namedScratchpadManageHook scratchpads
		    <+> manageDocks
		    <+> manageHook def
  , startupHook   = setDefaultCursor xC_left_ptr
  }

main :: IO ()
main = do
  barPipe <- spawnPipe myStatusBar
  xmonad $ fullscreenSupport $ docks $ ewmh $ myConfig barPipe
