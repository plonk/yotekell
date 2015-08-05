{-# LANGUAGE DeriveDataTypeable #-}
module GameState where

import Text.JSON.Generic
import qualified Data.Set as S
import Data.List
import Data.Maybe
import System.Random

import Pos
import Bomb
import Item
import Player
import Move

-- | テストデータ
-- 
--   Turn 0
--   ■■■■■■■■■■■■■■■
--   ■予　□□□　　　□　□　発■
--   ■　■□■　■□■　■□■　■
--   ■　□□□□　□　□　□□□■
--   ■□■□■　■□■□■□■□■
--   ■　□□□□□　□　□　□□■
--   ■　■　■□■□■□■□■□■
--   ■□□□□□□□□□□□□　■
--   ■□■□■　■□■□■　■□■
--   ■□　□　□□　□□□□□□■
--   ■□■□■□■□■□■□■　■
--   ■□□□□□□□　□　□　□■
--   ■　■　■□■□■　■　■　■
--   ■よ　□□□□□□□□　　発■
--   ■■■■■■■■■■■■■■■
-- 
-- 0 予定地AI
-- 1 よてぱい
-- 2 発
-- 3 発
sampleState = decodeJSON "{\"turn\":0,\"walls\":[[0,0],[0,1],[0,2],[0,3],[0,4],[0,5],[0,6],[0,7],[0,8],[0,9],[0,10],[0,11],[0,12],[0,13],[0,14],[1,0],[1,14],[2,0],[2,2],[2,4],[2,6],[2,8],[2,10],[2,12],[2,14],[3,0],[3,14],[4,0],[4,2],[4,4],[4,6],[4,8],[4,10],[4,12],[4,14],[5,0],[5,14],[6,0],[6,2],[6,4],[6,6],[6,8],[6,10],[6,12],[6,14],[7,0],[7,14],[8,0],[8,2],[8,4],[8,6],[8,8],[8,10],[8,12],[8,14],[9,0],[9,14],[10,0],[10,2],[10,4],[10,6],[10,8],[10,10],[10,12],[10,14],[11,0],[11,14],[12,0],[12,2],[12,4],[12,6],[12,8],[12,10],[12,12],[12,14],[13,0],[13,14],[14,0],[14,1],[14,2],[14,3],[14,4],[14,5],[14,6],[14,7],[14,8],[14,9],[14,10],[14,11],[14,12],[14,13],[14,14]],\"blocks\":[[2,7],[12,5],[7,7],[8,5],[6,13],[3,10],[9,9],[9,11],[4,3],[7,8],[8,13],[11,6],[12,3],[5,10],[13,5],[1,9],[13,8],[7,3],[6,5],[5,11],[7,2],[3,8],[13,9],[3,11],[9,6],[11,11],[4,1],[3,5],[8,9],[10,5],[11,4],[3,4],[10,9],[5,7],[5,6],[13,4],[3,3],[11,9],[9,13],[9,10],[2,11],[7,6],[11,3],[2,3],[1,4],[7,12],[3,9],[3,13],[1,7],[8,7],[6,9],[12,7],[2,5],[3,7],[6,7],[1,10],[5,5],[6,11],[13,3],[4,5],[1,11],[5,3],[9,1],[9,7],[7,4],[10,7],[13,11],[4,11],[5,12],[12,9],[4,13],[9,3],[7,11],[13,6],[5,1],[11,2],[5,9],[10,13],[7,10],[11,10],[5,13],[4,7],[11,7],[3,2],[9,8],[1,8],[11,1],[3,1],[7,13],[9,4]],\"players\":[{\"name\":\"予定地AI\",\"pos\":{\"x\":1,\"y\":1},\"power\":2,\"setBombLimit\":2,\"ch\":\"予\",\"isAlive\":true,\"setBombCount\":0,\"totalSetBombCount\":0,\"id\":0},{\"name\":\"よてぱい\",\"pos\":{\"x\":1,\"y\":13},\"power\":2,\"setBombLimit\":2,\"ch\":\"よ\",\"isAlive\":true,\"setBombCount\":0,\"totalSetBombCount\":0,\"id\":1},{\"name\":\"発\",\"pos\":{\"x\":13,\"y\":1},\"power\":2,\"setBombLimit\":2,\"ch\":\"発\",\"isAlive\":true,\"setBombCount\":0,\"totalSetBombCount\":0,\"id\":2},{\"name\":\"発\",\"pos\":{\"x\":13,\"y\":13},\"power\":2,\"setBombLimit\":2,\"ch\":\"発\",\"isAlive\":true,\"setBombCount\":0,\"totalSetBombCount\":0,\"id\":3}],\"bombs\":[],\"items\":[],\"fires\":[]}" :: GameState

spiral :: Int -> Int -> Int -> Int -> [(Int, Int)]
spiral 5 _ _ _               = []
spiral left right top bottom =
  goRight ++ goDown ++ goLeft ++ goUp ++ recur
  where
    goRight = map (\x -> (x, top)) [left..right]
    goDown  = map (\y -> (right, y)) [top+1..bottom]
    goLeft  = map (\x -> (x, bottom)) (reverse [left..right-1])
    goUp    = map (\y -> (left, y)) (reverse [top+1..bottom-1])
    recur   = spiral (left+1) (right-1) (top+1) (bottom-1)

-- | 螺旋状の壁振り座標系列
-- >>> length fallingWalls
-- 144
-- >>> (head fallingWalls, last fallingWalls)
-- ((1,1),(4,5))
fallingWalls = spiral 1 13 1 13

data GameState = GameState { turn    :: Int,
                             walls   :: S.Set (Int, Int),
                             blocks  :: S.Set (Int, Int),
                             players :: [Player],
                             bombs   :: [Bomb],
                             items   :: [Item],
                             fires   :: S.Set (Int, Int)
                           }
               deriving (Eq, Data, Typeable)

map2d f matrix = map (\(row, i) ->
                       map (\(c, j) -> f (c, (i, j))) $
                       zip row [0..]) $
                 zip matrix [0..]

overlaySymbols symbol set =
  map2d (\(c, (i, j))  -> if S.member (j, i) set then symbol else c)

overlayObjects pos symf os =
  map2d (\(c, (i, j)) ->
          case find (\obj -> pos obj == Pos j i) os of
            Just obj -> symf obj
            Nothing  -> c)

showMap state = unlines $
                foldl (\acc f -> f acc) matrix $
                [overlayBlocks, overlayBombs, overlayItems,
                 overlayFires, overlayWalls, overlayPlayers]
  where
    overlayPlayers = overlayObjects Player.pos (head . Player.ch) (players state)
    overlayItems   = overlayObjects Item.pos (head . Item.name) (items state)
    overlayBombs   = overlayObjects Bomb.pos (\b -> '●') (bombs state)
    overlayFires   = overlaySymbols '火' (fires state)
    overlayBlocks  = overlaySymbols '□' (blocks state)
    overlayWalls   = overlaySymbols '■' (walls state)
    matrix         = take 15 $ repeat $ take 15 $ repeat '　'

instance Show GameState where
  show state = "Turn " ++ show (turn state) ++
           " Bombs " ++ show (length (bombs state)) ++
	   " Walls " ++ show (S.size (walls state)) ++ "\n" ++
           showMap state

bundleRandom :: RandomGen g => (GameState -> GameState) -> ((GameState, g) -> (GameState, g))
bundleRandom f = \(state, gen) -> (f state, gen)

-- | ゲーム状態を遷移させる。
transition :: RandomGen g => GameState -> [Move] -> g -> (GameState, g)
transition state moves gen = applyThese (state, gen) $
                             map bundleRandom [playersPutBombs moves,
                                               playersMove moves,
                                               turnIncrements,
                                               wallFalls,
                                               bombTimersDecrement,
                                               itemsGetPicked,
                                               bombsExplode,
                                               itemsGetIncinerated,
                                               blocksGetIncinerated,
                                               deadPlayersGetMarked]
    
-- | 複数の関数を数珠繋ぎに初期値に適用する。
-- >>> applyThese 0 [(+1), (+2), (+3)]
-- 6
applyThese :: a -> [a -> a] -> a
applyThese = foldl (flip ($))

-- xs の n 番目の要素を x と入れ替える。
-- n は 0 以上 length xs 未満にしてください。
replaceNth :: Int -> a -> [a] -> [a]
replaceNth n x xs = take n xs ++ [x] ++ drop (n+1) xs

playersPutBombs :: [Move] -> GameState -> GameState
playersPutBombs moves state = applyThese state $
                              map (\id -> putBomb ((players state)!!id) (moves!!id)) [0..numPlayers-1]
  where
    numPlayers = length (players state)
    putBomb player move state =
      if Move.bomb move && playerCanSetBomb player
      then state { players = players', bombs = bombs' }
      else
        state
      where
        players' = replaceNth (Player.id player) player' (players state)
        bombs' = Bomb { Bomb.pos   = Player.pos player,
                        Bomb.timer = 10,
                        Bomb.power = Player.power player } : bombs state
        player' = player { setBombCount      = (setBombCount player) + 1,
                           totalSetBombCount = (totalSetBombCount player) + 1 }
        playerCanSetBomb player = isAlive player &&
                                  (not $ isBomb state (Player.pos player)) &&
                                  setBombLimit player > setBombCount player

-- | pos に爆弾がある。
-- >>> import Pos
-- >>> isBomb sampleState $ Pos 1 1
-- False
-- >>> let state' = playersPutBombs (take 4 $ repeat $ Move Stay True) sampleState
-- >>> isBomb state' $ Pos 1 1
-- True
isBomb :: GameState -> Pos -> Bool
isBomb state pos = find (\b -> Bomb.pos b == pos) (bombs state) /= Nothing

dirOffsets :: Move.Command -> (Int, Int)
dirOffsets dir = case dir of
  Move.Up -> (0, -1)
  Move.Down -> (0, 1)
  Move.Left -> (-1, 0)
  Move.Right -> (1, 0)
  Move.Stay -> (0, 0)

playersMove :: [Move] -> GameState -> GameState
playersMove moves state = applyThese state $
                          map (\id -> playerMoves ((players state)!!id) (moves!!id)) [0..numPlayers-1]
  where
    numPlayers = length (players state)
    playerMoves player move state = if isAlive player && isEnterable state nextPos
                                    then state { players = replaceNth (Player.id player) player { Player.pos = nextPos } (players state) }
                                    else state
      where
        nextPos = Pos.addVec (Player.pos player) $ dirOffsets (Move.command move)

-- | pos に破壊可能なブロックがある。
-- >>> isBlock sampleState $ Pos 1 1
-- False
-- >>> isBlock sampleState $ Pos 3 1
-- True
isBlock state pos = S.member (toVec pos) (blocks state)

-- | pos に壁がある。
-- >>> isWall sampleState $ Pos 0 0
-- True
-- >>> isWall sampleState $ Pos 1 1
-- False
isWall state pos = S.member (toVec pos) (walls state)

-- | プレーヤーは隣接するマスから pos に移動できる。
-- >>> isEnterable sampleState $ Pos 1 1
-- True
-- >>> isEnterable sampleState $ Pos 2 2
-- False
-- >>> let state' = playersPutBombs (take 4 $ repeat $ Move Stay True) sampleState
-- >>> isEnterable state' $ Pos 1 1
-- False
isEnterable state pos = not (isWall state pos || isBlock state pos || isBomb state pos)

-- | ターンカウントを増加させる。
-- >>> turn sampleState
-- 0
-- >>> let state' = turnIncrements sampleState
-- >>> turn state'
-- 1
turnIncrements :: GameState -> GameState
turnIncrements state = state { turn = (turn state) + 1 }

-- | ターン 360 から壁が１つずつ降ってくる
-- 初期の壁の数をマップの大きさから求めると、
-- 
--   (n - 2) * 4 + 4 + (((n-1)/2) - 1)^2
-- 
-- となるので、n = 15 の場合 92 になる。
--
-- >>> import qualified Data.Set as S
-- >>> let state' = sampleState { turn = 360 }
-- >>> S.size (walls state')
-- 92
-- >>> S.size (walls $ wallFalls state')
-- 93
wallFalls :: GameState -> GameState
wallFalls state = if (turn state) >= 360 && (turn state) - 360 < length fallingWalls
              then let pt = fallingWalls !! (turn state - 360)
                   in state { walls = S.union (walls state) (S.singleton pt),
                          blocks = S.difference (blocks state) (S.singleton pt),
                          items = filter (\item -> (Pos.toVec $ Item.pos item) /= pt) (items state),
                          bombs = filter (\bomb -> (Pos.toVec $ Bomb.pos bomb) /= pt) (bombs state) }
              else
                state

bombTimersDecrement :: GameState -> GameState
bombTimersDecrement state = state { bombs = bombs' }
  where
    bombs' = map Bomb.decrementCount (bombs state)

itemEffect :: Item -> Player -> GameState -> GameState
itemEffect item player state = case Item.name item of
  "力" -> state { players = replaceNth (Player.id player) player { Player.power = (Player.power player) + 1 } (players state) }
  "弾" -> state { players = replaceNth (Player.id player) player { setBombLimit = (setBombLimit player) + 1 } (players state) }
  _ -> error "item effect unimplemented"

itemsGetPicked :: GameState -> GameState
itemsGetPicked state = applyThese state $ map (\id -> let player = (players state) !! id
                                              in case find (\i -> (Item.pos i) == (Player.pos player)) (items state) of
                                                Nothing -> Prelude.id
                                                Just item -> itemEffect item player) [0..length (players state) - 1]

bombsExplode :: GameState -> GameState
bombsExplode state = let (bombs', fires') = chainReaction initIgnited (initUnignited, S.empty)
                     in state { bombs = bombs', fires = fires' }
  where
    initIgnited   = [b | b <- (bombs state), (timer b) <= 0]
    initUnignited = [b | b <- (bombs state), (timer b) > 0]
    -- 連鎖爆発
    chainReaction ignited (unignited, fires) = case ignited of
      [] -> (unignited, fires)
      _  -> let fires'     = fires `S.union` (S.fromList $ concatMap (explode state) ignited)
                ignited'   = [b | b <- unignited, S.member (Bomb.posVec b) fires']
                unignited' = [b | b <- unignited, not$S.member (Bomb.posVec b) fires']
            in chainReaction ignited' (unignited', fires')

explode :: GameState -> Bomb -> [(Int, Int)]
explode state b = Pos.toVec(Bomb.pos b) :
            concatMap (\dir -> fireColumn (Bomb.pos b) dir (Bomb.power b)) [(0,-1),(0,1),(-1,0),(1,0)]
  where
    fireColumn _ _ 0 = []
    fireColumn pos dirVec power = if isWall state nextPos
                                  then []
                                  else if isBlock state nextPos || isItem state nextPos
                                       then [Pos.toVec nextPos]
                                       else Pos.toVec nextPos : fireColumn nextPos dirVec (power - 1)
      where nextPos = Pos.addVec pos dirVec

isItem state pos = isJust $ find (\item -> (Item.pos item) == pos) (items state)

itemsGetIncinerated :: GameState -> GameState
itemsGetIncinerated state = state { items = [i | i <- (items state), not $ (S.member (toVec (Item.pos i)) (fires state))] }

-- TODO: アイテムが落ちること。
blocksGetIncinerated :: GameState -> GameState
blocksGetIncinerated state = state { blocks = S.fromList $ [b | b <- S.toList (blocks state), not $ (S.member b (fires state))] }

deadPlayersGetMarked :: GameState -> GameState
deadPlayersGetMarked state =
  state { players = map (\p -> if isDead p
                           then p { isAlive = False, ch = "墓" }
                           else p) (players state) }
  where
    isDead player = let coords = toVec(Player.pos player )
                    in S.member coords (fires state) || S.member coords (walls state)
