-- Check file
-- xmonad --recompile

import XMonad
import XMonad.Util.EZConfig

main = xmonad $ defaultConfig
    { terminal = "terminator" }
    `additionalKeysP`
    [ ("C-M-l", spawn "xlock -model ant")
    , ("M-S-p", spawn "exe=`dmenu_path | dmenu` && eval \"exec sudo -A $exe\"")
    , ("M-x", spawn "xquit")]
