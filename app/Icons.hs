{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icons
    (
    findIcon,
    findMaybeIconFromName,
    findMaybeIconsFromNames
    ) where

import qualified Control.Arrow as Arrow
import qualified Data.IntMap as IMap
import Data.List(find)
import Data.Maybe(listToMaybe, isJust, fromJust, mapMaybe)

import           Types(
  Icon(..)
  , NodeName(..)
  , NamedIcon
  , Labeled(..)
  , IconInfo
  , Named(..)
  , DiagramIcon(..)
  , laValue
  , naVal
  )
import Diagrams.Prelude

findMaybeIconsFromNames :: IconInfo -> [Maybe NodeName] -> [Maybe NamedIcon]
findMaybeIconsFromNames iconInfo args = over (mapped . _Just) (findIconFromName iconInfo) args

findMaybeIconFromName :: IconInfo -> Maybe NodeName -> Maybe NamedIcon
findMaybeIconFromName iconInfo maybeName = over _Just (findIconFromName iconInfo) maybeName

findIconFromName :: IconInfo -> NodeName -> NamedIcon
findIconFromName icons name@(NodeName nameInt)
  = Named name $ IMap.findWithDefault
    (error $ "findIconFromName: icon not found.\nicons="
      <> show icons <> "\nname=" <> show name)
    nameInt
    icons

findIcon :: IconInfo -> NodeName -> [Maybe NamedIcon] -> Maybe (Int, Icon)
findIcon iconInfo name args = Just (-1000, findIconFromName iconInfo name ^. naVal)