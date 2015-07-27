module Move where

import Data.Char

data Command = Up | Down | Left | Right | Stay
             deriving (Show)

data Move = Move { command :: Command,
                   bomb :: Bool,
                   comment :: String }

-- |
-- >>> show $ Move Up True "foo"
-- "UP,true,foo"
instance Show Move where
  show mov = (upcase $ show (command mov)) ++ "," ++
             (downcase $ show (bomb mov)) ++ "," ++
             comment mov
    where
      upcase   = map toUpper
      downcase = map toLower
