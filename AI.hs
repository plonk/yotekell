module AI where

import System.Random
import System.Timeout
import Data.List
import Data.Maybe

import GameState
-- Either の Left/Right との衝突を避けるため条件付きで輸入する。
import qualified Move as M
import Player

-- | xs から１つ要素を選択する。
-- >>> let xs = [0,1,2,3]
-- >>> let (chosen, _) = sample xs (mkStdGen 0)
-- >>> isJust $ find (\x -> x == chosen) xs
-- True
sample :: RandomGen g =>
          [a] -> g -> (a, g)
sample xs gen = let (idx, gen') = randomR (0, length xs - 1) gen
                in (xs !! idx, gen')

-- | その局面で選択可能な手の一覧。
-- TODO: ちゃんと無効なコマンドは省く。
legalMoves :: GameState -> Int -> [M.Move]
legalMoves state id = [M.Move c b |
                       c <- [M.Up, M.Down, M.Left, M.Right, M.Stay],
                       b <- [True, False]]

-- | 手を決定する。タイムアウト付き。
decideMove :: RandomGen g =>
              Int -> GameState -> g -> IO (M.Move, g)
decideMove id state gen =
    do
      result <- timeout (500*1000) $ decideMove' id state gen
      case result of
        Just (move, gen') -> return (move, gen')
        -- 時間切れになった時のデフォルトの行動。
        Nothing -> return (M.Move M.Stay False, gen)

splitN :: RandomGen g => g -> Int -> ([g], g)
splitN gen n = gens !! n
  where
    gens = iterate (\(acc, g) ->
                     let (g', g'') = split g
                     in  (g' : acc, g''))
           ([], gen)

partialAverages :: [Double] -> [Double]
partialAverages xs = map avg $ drop 1 $ inits xs
  where
    avg ys = foldr1 (+) ys / (fromIntegral $ length ys)

decentMove :: [Double] -> Bool
decentMove = any (\x -> x > (-50.0))

indexOfMax xs = let maxValue = foldr1 max xs
                in length $ takeWhile (\x -> x /= maxValue) xs

-- | 手を決定する。時間制限は気にしない。それぞれの手について評価値の無限リストを作る。
-- id は自分の ID。
decideMove' :: RandomGen g =>
               Int -> GameState -> g -> IO (M.Move, g)
decideMove' id state gen =
    do
      let moves = legalMoves state id
      let (gens, gen') = splitN gen (length moves)
      let Just scores = find decentMove $ drop 5 $ transpose $ map partialAverages $ zipWith (\move g -> scoreStream id move state g) moves gens
      let idx = indexOfMax scores
      return (moves !! idx, gen')

randomizedValues f gen = let (value, gen') = f gen
                         in value : randomizedValues f gen'

scoreStream :: RandomGen g => Int -> M.Move -> GameState -> g -> [Double]
scoreStream id move state gen = randomizedValues scoreFunc gen
  where
    -- gen を取る。
    scoreFunc = scoreMove id move state

-- | 手を評価する。id は自分の ID、move は自分の手。
scoreMove :: RandomGen g =>
             Int -> M.Move -> GameState -> g -> (Double, g)
scoreMove id move state gen = let (moves, gen') = randomMoves state gen
                              -- 自分の行動はランダムではなく、規定。
                              in let initMoves = take (id-1) moves ++ [move] ++ drop (id-1) moves
                                     (state', gen'') = transition state initMoves gen'
                                 in let (finalState, gen''') = runSimulation state' gen'' !! 14
                                    in (scoreState id finalState, gen''')

scoreState :: Int -> GameState -> Double
scoreState id state = if isAlive me
                      then 0
                      else -100
  where
    me = players state !! (id-1)

randomMoves :: RandomGen g => GameState -> g -> ([M.Move], g)
randomMoves state g = foldl (\(res, g) moves ->
                              let (move, g') = sample moves g
                              in (res ++ [move], g'))
                      ([], g)
                      [legalMoves state id | id <- [1..4]]

runSimulation :: RandomGen g =>
                 GameState -> g -> [(GameState, g)]
runSimulation state gen = let (moves, gen') = randomMoves state gen
                          in let (state', gen'') = transition state moves gen'
                             in (state', gen'') : runSimulation state' gen''
