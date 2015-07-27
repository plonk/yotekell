{-# LANGUAGE DeriveDataTypeable #-}
module Pos where
import Text.JSON.Generic

data Pos = Pos { x :: Int,
                 y :: Int } deriving (Eq, Show, Data, Typeable)

addVec pos (xoff, yoff) = Pos { x = (x pos) + xoff,
                                y = (y pos) + yoff }

toVec pos = (x pos, y pos)
