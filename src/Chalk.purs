{- https://raw.githubusercontent.com/gcanti/purescript-text-chalk/master/src/Text/Chalk.purs -}
module Chalk where

import Prelude

-- | Helper function.
text :: Int -> Int -> String -> String
text start end source = "\x1b[" <> (show start) <> "m" <> source <> "\x1b[" <> (show end) <> "m"

--
-- colors
--

black :: String -> String
black = text 30 39

red :: String -> String
red = text 31 39

green :: String -> String
green = text 32 39

yellow :: String -> String
yellow = text 33 39

blue :: String -> String
blue = text 34 39

magenta :: String -> String
magenta = text 35 39

cyan :: String -> String
cyan = text 36 39

white :: String -> String
white = text 37 39

gray :: String -> String
gray = text 90 39

--
-- background colors
--

black' :: String -> String
black' = text 40 49

red' :: String -> String
red' = text 41 49

green' :: String -> String
green' = text 42 49

yellow' :: String -> String
yellow' = text 43 49

blue' :: String -> String
blue' = text 44 49

magenta' :: String -> String
magenta' = text 45 49

cyan' :: String -> String
cyan' = text 46 49

white' :: String -> String
white' = text 47 49

--
-- effects
--

reset :: String -> String
reset = text 0 0

bold :: String -> String
bold = text 1 21

dim :: String -> String
dim = text 2 22

-- | Not widely supported.
italic :: String -> String
italic = text 3 23

underline :: String -> String
underline = text 4 24

blink :: String -> String
blink = text 5 25

inverse :: String -> String
inverse = text 7 27

hidden :: String -> String
hidden = text 8 28

-- | Not widely supported.
strikethrough :: String -> String
strikethrough = text 9 29
