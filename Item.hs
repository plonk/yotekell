{-# LANGUAGE DeriveDataTypeable #-}
module Item where
import Pos
import Text.JSON.Generic

data Item = Item { pos :: Pos,
                   name :: String }
            deriving (Eq, Show, Data, Typeable)
