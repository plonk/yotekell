{-# LANGUAGE DeriveDataTypeable #-}
module Player where
import Pos
import Text.JSON.Generic

data Player = Player { name :: String,
                       pos :: Pos,
                       power :: Int,
                       setBombLimit :: Int,
                       ch :: String,
                       isAlive :: Bool,
                       setBombCount :: Int,
                       totalSetBombCount :: Int,
                       id :: Int }
              deriving (Eq, Show, Data, Typeable)
