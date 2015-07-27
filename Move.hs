module Move where

data Command = Up | Down | Left | Right | Stay
             deriving (Show)

data Move = Move { command :: Command,
                   bomb :: Bool,
                   comment :: String }

instance Show Move where
  show mov = show (command mov) ++ "," ++
             show (bomb mov) ++ "," ++
             comment mov
