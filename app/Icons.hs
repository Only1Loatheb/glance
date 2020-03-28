{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icons
    (
    inputPort,
    resultPort,
    argumentPorts,
    caseRhsPorts,
    casePatternPorts,
    multiIfRhsPorts,
    multiIfBoolPorts,
    findIconFromName,
    findIcon,
    argPortsConst
    ) where

import qualified Control.Arrow as Arrow
import qualified Data.IntMap as IM
import Data.List(find)
import Data.Maybe(listToMaybe, isJust, fromJust, mapMaybe)

import Constants(pattern InputPortConst, pattern ResultPortConst)
import Types(Icon(..)
            , NodeName(..), Port(..),
            SyntaxNode(..), NamedIcon, Labeled(..), IconInfo
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
  NestedPApp constructor args ->
    snd <$> findIcon iconInfo name (fmap laValue (constructor:args))
  _ -> Nothing

-- BEGIN Port numbers

argPortsConst :: [Port]
argPortsConst = fmap Port [2,3..]

-- TODO It's a bit strange that the parameter is a SyntaxNode, not an Icon.
inputPort :: SyntaxNode -> Port
inputPort = const InputPortConst

resultPort :: SyntaxNode -> Port
resultPort = const ResultPortConst

caseRhsPorts :: [Port]
caseRhsPorts = fmap Port [3,5..]

casePatternPorts :: [Port]
casePatternPorts = fmap Port [2,4..]

multiIfRhsPorts :: [Port]
multiIfRhsPorts = casePatternPorts

multiIfBoolPorts :: [Port]
multiIfBoolPorts = caseRhsPorts

argumentPorts :: SyntaxNode -> [Port]
argumentPorts n = case n of
  (ApplyNode _ _) -> defaultPorts
  PatternApplyNode _ _-> defaultPorts
  (FunctionDefNode _ _) -> defaultPorts
  CaseOrMultiIfNode _ _ -> defaultPorts
  NameNode _ -> []
  BindNameNode _ -> []
  LiteralNode _ -> []
  CaseResultNode -> []
  where
    defaultPorts = argPortsConst
-- END Port numbers
