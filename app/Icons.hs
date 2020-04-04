{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icons
    (
    findIconFromName,
    findIcon
    ) where

import qualified Control.Arrow as Arrow
import qualified Data.IntMap as IM
import Data.List(find)
import Data.Maybe(listToMaybe, isJust, fromJust, mapMaybe)

import Types(Icon(..)
            , NodeName(..), NamedIcon, Labeled(..), IconInfo
            , Named(..))

{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Unnecessary hiding" #-}

-- BEGIN Exported icon functions --

findIconFromName :: IconInfo -> NodeName -> NamedIcon
findIconFromName icons name@(NodeName nameInt)
  = Named name $ IM.findWithDefault
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
findNestedIcon iconInfo name icon = case icon of
  NestedApply _ headIcon args
    -> snd
        <$> findIcon
        iconInfo
        name
        ((fmap . fmap) (findIconFromName iconInfo) (headIcon : args))
  NestedPatternApp constructor args ->
    snd <$> findIcon iconInfo name (fmap laValue (constructor:args))
  _ -> Nothing

