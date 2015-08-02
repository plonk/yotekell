{-# LANGUAGE DeriveDataTypeable #-}
module Bomb where
import Pos
import Text.JSON.Generic

data Bomb = Bomb { pos :: Pos,
                   timer :: Int,
                   power :: Int }
            deriving (Show, Eq, Data, Typeable)

-- | 爆発までのカウントを減じる
-- >>> let bomb = Bomb Pos { x = 1, y = 1 } 10 2
-- >>> timer $ decrementCount bomb
-- 9
decrementCount :: Bomb -> Bomb
decrementCount Bomb { timer = 0 } = error "cannot decrement timer"
decrementCount bomb = bomb { timer = (timer bomb) - 1 }

posVec :: Bomb -> (Int, Int)
posVec b = Pos.toVec (pos b)
