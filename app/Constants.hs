{-# LANGUAGE PatternSynonyms #-}
module Constants
  ( pattern InputPortConst
  , pattern ResultPortConst
  ) where

import Types(Port(..), PortNo(..))

pattern InputPortConst :: Port
pattern InputPortConst = Port (PortNo 0) True

pattern ResultPortConst :: Port
pattern ResultPortConst = Port (PortNo 1) False
