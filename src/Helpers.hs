module Helpers where

import Data.Char

startsWithDigit :: String -> Bool
startsWithDigit (s:_) = isDigit s