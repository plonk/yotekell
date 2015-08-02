module AI where

import System.Random
import System.Timeout
import Data.List
import Data.Maybe

import GameState
import Move
import Player

-- | xs から１つ要素を選択する。
-- >>> let xs = [0,1,2,3]
-- >>> let (chosen, _) = sample xs (mkStdGen 0)
-- >>> isJust $ find (\x -> x == chosen) xs
-- True
sample :: RandomGen g => [a] -> g -> (a, g)
sample xs gen = let (idx, gen') = randomR (0, length xs - 1) gen
                in (xs !! idx, gen')

-- | その局面で選択可能な手の一覧。
-- TODO: ちゃんと無効なコマンドは省く。
legalMoves :: GameState -> Int -> [Move]
legalMoves state id = [Move c b | c <- [Move.Up, Move.Down, Move.Left, Move.Right, Move.Stay], b <- [True, False]]

-- | 手を決定する。タイムアウト付き。
decideMove :: RandomGen g => Int -> GameState -> g -> IO (Move, g)
decideMove id state gen =
    do
      result <- timeout (500*1000) $ decideMove' id state gen
      return $ case result of
                 Just (move, gen') -> (move, gen')
                 Nothing -> (Move Stay False, gen)

-- | 手を決定する。
decideMove' :: RandomGen g => Int -> GameState -> g -> IO (Move, g)
decideMove' id state gen =
    do return (Move Move.Stay False, gen)


-- | 手を評価する。
scoreMove :: RandomGen g => Move -> GameState -> g -> IO (Double, g)
scoreMove move state gen = undefined
