module Move where

import System.Random
import Data.Char

data Command = Up | Down | Left | Right | Stay
             deriving (Eq, Show, Enum, Ord, Bounded)

instance Random Command where
    random gen = randomR (Up, Stay) gen

    randomR (beg, end) gen =
        let (randomInt, gen') = next gen
        in let val = sampleSpace !! (mod randomInt $ length sampleSpace)
           in (val, gen')
        where
          sampleSpace = [beg .. end]

-- 範囲:
--   Move Up False ~ Move Stay True
data Move = Move { command :: Command,
                   bomb :: Bool }
          deriving (Eq, Ord, Bounded)


instance Random Move where
    random gen = randomR (Move Up False, Move Stay True) gen
    randomR (Move cmdBeg boolBeg, Move cmdEnd boolEnd) gen =
        let (command, gen') = randomR (cmdBeg, cmdEnd) gen
        in let (bomb, gen'') = randomR (boolBeg, boolEnd) gen'
           in (Move command bomb, gen'')

-- | ボムマンサーバーへの送信に適した形に文字列化する。
-- >>> import Move
-- >>> show $ Move Up True
-- "UP,true"
instance Show Move where
  show move =  cmdStr ++ "," ++ moveStr
    where
      cmdStr   = upcase $ show (command move)
      moveStr  = downcase $ show (bomb move)
      upcase   = map toUpper
      downcase = map toLower
