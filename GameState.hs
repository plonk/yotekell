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

-- 螺旋状の壁振り座標系列
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

isBlock s pos = Set.member (toVec pos) (blocks s)

isWall s pos = Set.member (toVec pos) (walls s)

isEnterable s pos = not (isWall s pos || isBlock s pos || isBomb s pos)

turnIncrements :: GameState -> GameState
turnIncrements s = s { turn = (turn s) + 1 }

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
bombsExplode s = let (bs, fs) = iter [b | b <- (bombs s), (timer b) <= 0] ((bombs s), Set.empty)
                 in s { bombs = bs, fires = fs }
  where
    iter bombsToExplode (bombs, fires) = case bombsToExplode of
      [] -> (bombs, fires)
      _ -> let fires' = Set.union fires $ Set.fromList $ concatMap (explode s) bombs
               bombsToExplode' = [b | b <- bombs, Set.member (Pos.toVec (Bomb.pos b)) fires']
               bombs' = [b | b <- bombs, not $ Set.member (Pos.toVec (Bomb.pos b)) fires']
           in iter bombsToExplode' (bombs', fires')

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

