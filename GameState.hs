{-# LANGUAGE DeriveDataTypeable #-}
module GameState where

import Text.JSON.Generic
import qualified Data.Set as Set
import Data.List
import Data.Maybe

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
-- >>> head fallingWalls
-- (1,1)
-- >>> last fallingWalls
-- (4,5)
fallingWalls = spiral 1 13 1 13

data GameState = GameState { turn    :: Int,
                             walls   :: Set.Set (Int, Int),
                             blocks  :: Set.Set (Int, Int),
                             players :: [Player],
                             bombs   :: [Bomb],
                             items   :: [Item],
                             fires   :: Set.Set (Int, Int) }
               deriving (Eq, Data, Typeable)

map2d f matrix = map (\(row, i) ->
                       map (\(c, j) -> f (c, (i, j))) $
                       zip row [0..]) $
                 zip matrix [0..]

overlaySymbols symbol set =
  map2d (\(c, (i, j))  -> if Set.member (j, i) set then symbol else c)

overlayObjects pos symf os =
  map2d (\(c, (i, j)) ->
          case find (\obj -> pos obj == Pos j i) os of
            Just obj -> symf obj
            Nothing  -> c)

showMap s = unlines $
                foldl (\acc f -> f acc) matrix $
                [overlayBlocks, overlayBombs, overlayItems,
                 overlayFires, overlayWalls, overlayPlayers]
  where
    overlayPlayers = overlayObjects Player.pos (head . Player.ch) (players s)
    overlayItems   = overlayObjects Item.pos (head . Item.name) (items s)
    overlayBombs   = overlayObjects Bomb.pos (\b -> '●') (bombs s)
    overlayFires   = overlaySymbols '火' (fires s)
    overlayBlocks  = overlaySymbols '□' (blocks s)
    overlayWalls   = overlaySymbols '■' (walls s)
    matrix         = take 15 $ repeat $ take 15 $ repeat '　'

instance Show GameState where
  show s = "Turn " ++ show (turn s) ++
           " Bombs " ++ show (length (bombs s)) ++
	   " Walls " ++ show (Set.size (walls s)) ++ "\n" ++
           showMap s

transition :: GameState -> [Move] -> GameState
transition s moves =
  applyThese s [playersPutBombs moves,
                playersMove moves,
                turnIncrements,
                wallFalls,
                bombTimersDecrement,
                itemsGetPicked,
                bombsExplode,
                itemsGetIncinerated,
                blocksGetIncinerated,
                deadPlayersGetMarked]

applyThese = foldl (flip ($))

-- xs の n 番目の要素を x と入れ替える。
-- n は 0 以上 length xs 未満にしてください。
replaceNth n x xs = take n xs ++ [x] ++ drop (n+1) xs

playersPutBombs :: [Move] -> GameState -> GameState
playersPutBombs moves s = applyThese s $
                              map (\id -> putBomb ((players s)!!id) (moves!!id)) [0..numPlayers-1]
  where
    numPlayers = length (players s)
    putBomb player move s =
      if Move.bomb move && playerCanSetBomb player
      then s { players = players', bombs = bombs' }
      else
        s
      where
        players' = replaceNth (Player.id player) player' (players s)
        bombs' = Bomb { Bomb.pos   = Player.pos player,
                        Bomb.timer = 10,
                        Bomb.power = Player.power player } : bombs s
        player' = player { setBombCount      = (setBombCount player) + 1,
                           totalSetBombCount = (totalSetBombCount player) + 1 }
        playerCanSetBomb player = isAlive player &&
                                  (not $ isBomb s (Player.pos player)) &&
                                  setBombLimit player > setBombCount player

-- | pos に爆弾がある。
-- >>> isBomb sampleState $ Pos 1 1
-- False
-- >>> let state' = playersPutBombs (take 4 $ repeat $ Move Stay True "") sampleState
-- >>> isBomb state' $ Pos 1 1
-- True
isBomb :: GameState -> Pos -> Bool
isBomb s pos = find (\b -> Bomb.pos b == pos) (bombs s) /= Nothing

dirOffsets :: Move.Command -> (Int, Int)
dirOffsets dir = case dir of
  Move.Up -> (0, -1)
  Move.Down -> (0, 1)
  Move.Left -> (-1, 0)
  Move.Right -> (1, 0)
  Move.Stay -> (0, 0)

playersMove :: [Move] -> GameState -> GameState
playersMove moves s = applyThese s $
                          map (\id -> playerMoves ((players s)!!id) (moves!!id)) [0..numPlayers-1]
  where
    numPlayers = length (players s)
    playerMoves player move s = if isAlive player && isEnterable s nextPos
                                    then s { players = replaceNth (Player.id player) player { Player.pos = nextPos } (players s) }
                                    else s
      where
        nextPos = Pos.addVec (Player.pos player) $ dirOffsets (Move.command move)

-- | pos に破壊可能なブロックがある。
-- >>> isBlock sampleState $ Pos 1 1
-- False
-- >>> isBlock sampleState $ Pos 3 1
-- True
isBlock s pos = Set.member (toVec pos) (blocks s)

-- | pos に壁がある。
-- >>> isWall sampleState $ Pos 0 0
-- True
-- >>> isWall sampleState $ Pos 1 1
-- False
isWall s pos = Set.member (toVec pos) (walls s)

-- | プレーヤーは隣接するマスから pos に移動できる。
-- >>> isEnterable sampleState $ Pos 1 1
-- True
-- >>> isEnterable sampleState $ Pos 2 2
-- False
-- >>> let state' = playersPutBombs (take 4 $ repeat $ Move Stay True "") sampleState
-- >>> isEnterable state' $ Pos 1 1
-- False
isEnterable s pos = not (isWall s pos || isBlock s pos || isBomb s pos)

-- | ターンカウントを増加させる。
-- >>> turn sampleState
-- 0
-- >>> let state' = turnIncrements sampleState
-- >>> turn state'
-- 1
turnIncrements :: GameState -> GameState
turnIncrements s = s { turn = (turn s) + 1 }

-- | ターン 360 から壁が１つずつ降ってくる
-- 初期の壁の数をマップの大きさから求めると、
-- 
--   (n - 2) * 4 + 4 + (((n-1)/2) - 1)^2
-- 
-- となるので、n = 15 の場合 92 になる。
-- 
-- >>> let state' = sampleState { turn = 360 }
-- >>> Set.size (walls state')
-- 92
-- >>> Set.size (walls $ wallFalls state')
-- 93
wallFalls :: GameState -> GameState
wallFalls s = if (turn s) >= 360 && (turn s) - 360 < length fallingWalls
              then let pt = fallingWalls !! (turn s - 360)
                   in s { walls = Set.union (walls s) (Set.singleton pt),
                          blocks = Set.difference (blocks s) (Set.singleton pt),
                          items = filter (\item -> (Pos.toVec $ Item.pos item) /= pt) (items s),
                          bombs = filter (\bomb -> (Pos.toVec $ Bomb.pos bomb) /= pt) (bombs s) }
              else
                s

bombTimersDecrement :: GameState -> GameState
bombTimersDecrement s = s { bombs = bombs' }
  where
    bombs' = map (\b -> b { timer = (timer b) - 1 }) (bombs s)

itemEffect :: Item -> Player -> GameState -> GameState
itemEffect item player s = case Item.name item of
  "力" -> s { players = replaceNth (Player.id player) player { Player.power = (Player.power player) + 1 } (players s) }
  "弾" -> s { players = replaceNth (Player.id player) player { setBombLimit = (setBombLimit player) + 1 } (players s) }
  _ -> error "item effect unimplemented"

itemsGetPicked :: GameState -> GameState
itemsGetPicked s = applyThese s $ map (\id -> let player = (players s) !! id
                                              in case find (\i -> (Item.pos i) == (Player.pos player)) (items s) of
                                                Nothing -> Prelude.id
                                                Just item -> itemEffect item player) [0..length (players s) - 1]

bombsExplode :: GameState -> GameState
bombsExplode s = let (bombsUnexploded, fires') = iter [b | b <- (bombs s), (timer b) <= 0] ((bombs s), Set.empty)
                 in s { bombs = bombsUnexploded, fires = fires' }
  where
    iter bombsToExplode (bombsUnexploded, fires) = case bombsToExplode of
      [] -> (bombsUnexploded, fires)
      _ -> let fires' = Set.union fires $ Set.fromList $ concatMap (explode s) bombsToExplode
               bombsToExplode' = [b | b <- bombsUnexploded, Set.member (Pos.toVec (Bomb.pos b)) fires']
               bombsUnexploded' = [b | b <- bombsUnexploded, not $ Set.member (Pos.toVec (Bomb.pos b)) fires']
           in iter bombsToExplode' (bombsUnexploded', fires')

explode :: GameState -> Bomb -> [(Int, Int)]
explode s b = Pos.toVec(Bomb.pos b) :
            concatMap (\dir -> fireColumn (Bomb.pos b) dir (Bomb.power b)) [(0,-1),(0,1),(-1,0),(1,0)]
  where
    fireColumn _ _ 0 = []
    fireColumn pos dirVec power = if isWall s nextPos
                                  then []
                                  else if isBlock s nextPos || isItem s nextPos
                                       then [Pos.toVec nextPos]
                                       else Pos.toVec nextPos : fireColumn nextPos dirVec (power - 1)
      where nextPos = Pos.addVec pos dirVec

isItem s pos = isJust $ find (\item -> (Item.pos item) == pos) (items s)

itemsGetIncinerated :: GameState -> GameState
itemsGetIncinerated s = s { items = [i | i <- (items s), not $ (Set.member (toVec (Item.pos i)) (fires s))] }

-- TODO: アイテムが落ちること。
blocksGetIncinerated :: GameState -> GameState
blocksGetIncinerated s = s { blocks = Set.fromList $ [b | b <- Set.toList (blocks s), not $ (Set.member b (fires s))] }

deadPlayersGetMarked :: GameState -> GameState
deadPlayersGetMarked s =
  s { players = map (\p -> if isDead p
                           then p { isAlive = False, ch = "墓" }
                           else p) (players s) }
  where
    isDead player = let coords = toVec(Player.pos player )
                    in Set.member coords (fires s) || Set.member coords (walls s)
