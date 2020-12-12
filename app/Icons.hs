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
findIcon iconInfo name args = icon where
  numberedArgs = zip ([0,1..] :: [Int]) args
  filteredArgs = Arrow.second fromJust <$> filter (isJust . snd) numberedArgs
  nameMatches (_, Named n _) = n == name
  icon = case find nameMatches filteredArgs of
    Nothing -> listToMaybe $ mapMaybe findSubSubIcon filteredArgs
    Just (argNum, Named _ finalIcon) -> Just (argNum, finalIcon)
    where
      findSubSubIcon (argNum, Named _ subIcon)
        = case findNestedIcon iconInfo name subIcon of
            Nothing -> Nothing
            Just x -> Just (argNum, x)

findNestedIcon :: IconInfo -> NodeName -> Icon -> Maybe Icon
findNestedIcon iconInfo name (Icon icon _) = case icon of
  
  NestedApply _ headIcon args -> mFoundIcon where 
    mFoundIcon = over _Just snd (findIcon iconInfo name mNestedIcons) 
    mNestedIcons ::[Maybe NamedIcon]
    mNestedIcons = over (mapped . _Just) (findIconFromName iconInfo) (headIcon : args)

  NestedPatternApp constructor args _rhsName -> mFoundIcon where
    mFoundIcon = over _Just snd (findIcon iconInfo name mNestedIcons) 
    mNestedIcons :: [Maybe NamedIcon]
    mNestedIcons = toListOf (traverse . laValue) (constructor:args)
  _ -> Nothing