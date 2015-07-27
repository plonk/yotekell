{-# LANGUAGE DeriveDataTypeable #-}
module Bomb where
import Pos
import Text.JSON.Generic

data Bomb = Bomb { pos :: Pos,
                   timer :: Int,
                   power :: Int }
            deriving (Show, Eq, Data, Typeable)
