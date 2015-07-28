module AI where

import System.Random
import System.Timeout

import GameState
import Move
import Data.List
import Data.Maybe

-- | xs から１つ要素を選択する。
-- >>> let xs = [0,1,2,3]
-- >>> let (chosen, _) = sample xs (mkStdGen 0)
-- >>> isJust $ find (\x -> x == chosen) xs
-- True
sample :: RandomGen g => [a] -> g -> (a, g)
sample xs gen = let (idx, gen') = randomR (0, length xs - 1) gen
                in (xs !! idx, gen')

legalMoves :: GameState -> Int -> [Move]
legalMoves state id = [Move c b "" | c <- [Move.Up, Move.Down, Move.Left, Move.Right, Move.Stay], b <- [True, False]]

decideMove :: RandomGen g => Int -> GameState -> g -> IO (Move, g)
decideMove id state gen =
    do
      result <- timeout (500*1000) decideMove'
      return $ case result of
                 Just (move, gen') -> (move, gen')
                 Nothing -> (Move Stay False "500msタイムアウト", gen)
    where
      decideMove' =
          do return $ sample (legalMoves state id) gen

