{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EdgeAngles
    (
      getPortAngles
    ) where

import Diagrams.Prelude hiding ((&), (#), Name)

import qualified Control.Arrow as Arrow
import Data.Either(partitionEithers)
import qualified Data.IntMap as IM
import Data.List(find)
import Data.Maybe(listToMaybe, isJust, fromJust, mapMaybe)
import Data.Typeable(Typeable)

import Constants(pattern InputPortConst, pattern ResultPortConst)
import DrawingColors(colorScheme, ColorStyle(..))
import Types(Icon(..), SpecialQDiagram, SpecialBackend, SpecialNum
            , NodeName(..), Port(..), LikeApplyFlavor(..),
            SyntaxNode(..), NamedIcon, Labeled(..), IconInfo
            , Named(..))
import Icons(findIconFromName,findIcon)

{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Unnecessary hiding" #-}

applyPortAngles :: Floating n => Port -> [Angle n]
applyPortAngles (Port x) = fmap (@@ turn) $ case x of
  0 -> [3/8, 1/2, 5/8] -- TODO Don't use angle of 1/2 for nested icons here
  1 -> [3/4] -- line goes out with value to label
  _ -> [1/4, 1/4] -- [idk, side fromline comes with value like in lambda]

lambdaPortAngles :: Floating n => Bool -> Port -> [Angle n]
lambdaPortAngles embedded (Port x) = fmap (@@ turn) $ case x of
  -- 0 == lambda return value Icon
  0 -> if embedded
       then [3/4, 3/4]
       else [1/4, 1/4, 1/4]
  -- 1 == value port
  --1 -> [1/8, 7/8, 0]
  1 -> [3/4]
  _ -> [3/4, 3/4]

pAppPortAngles :: Floating n => Port -> [Angle n]
pAppPortAngles (Port x) = fmap (@@ turn) $ case x of
  0 -> [1/4]
  1 -> [0]
  _ -> [1/2]

multiIfPortAngles :: Floating n => Port -> [Angle n]
multiIfPortAngles (Port port) = case port of
  0 -> [1/4 @@ turn]
  1 -> [3/4 @@ turn]
  _ -> otherAngles where otherAngles
                           | even port = [0 @@ turn]
                           | otherwise = [1/2 @@ turn]

nestedMultiIfPortAngles :: SpecialNum n
  => IconInfo
  -> [Maybe NamedIcon]
  -> Port
  -> Maybe NodeName
  -> [Angle n]
nestedMultiIfPortAngles iconInfo args port maybeNodeName = case maybeNodeName of
  Nothing -> multiIfPortAngles port
  Just name -> case findIcon iconInfo name args of
    Nothing -> []
    -- TODO Don't use hardcoded numbers
    -- The arguments correspond to ports [0, 2, 3, 4 ...]
    Just (argNum, icon) -> if odd argNum && argNum >= 1
      -- The icon will be reflected
      then fmap reflectXAngle subAngles
      else subAngles
      where
        subAngles = getPortAnglesHelper True iconInfo icon port Nothing


generalNestedPortAngles :: SpecialNum n
  => IconInfo
  -> (Port -> [Angle n])
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> Port -> Maybe NodeName -> [Angle n]
generalNestedPortAngles iconInfo defaultAngles headIcon args port maybeNodeName =
  case maybeNodeName of
    Nothing -> defaultAngles port
    Just name -> case findIcon iconInfo name (headIcon : args) of
      Nothing -> []
      Just (_, icon) -> getPortAnglesHelper True iconInfo icon port Nothing

reflectXAngle :: SpecialNum n => Angle n -> Angle n
reflectXAngle x = reflectedAngle where
  normalizedAngle = normalizeAngle x
  reflectedAngle = (-) <$> halfTurn <*> normalizedAngle

getPortAngles :: SpecialNum n
  => IconInfo -> Icon -> Port -> Maybe NodeName -> [Angle n]
getPortAngles = getPortAnglesHelper False

getPortAnglesHelper :: SpecialNum n
  => Bool -> IconInfo -> Icon -> Port -> Maybe NodeName -> [Angle n]
getPortAnglesHelper embedded iconInfo icon port maybeNodeName = case icon of
  TextBoxIcon _ -> []
  BindTextBoxIcon _ -> []
  MultiIfIcon _ -> multiIfPortAngles port
  CaseIcon _ -> multiIfPortAngles port
  CaseResultIcon -> []
  LambdaIcon _ _ _ -> lambdaPortAngles embedded port
  NestedApply _ headIcon args
    -> generalNestedPortAngles
      iconInfo
      applyPortAngles
      -- TODO Refactor with iconToDiagram
      (fmap (findIconFromName iconInfo) headIcon)
      ((fmap . fmap) (findIconFromName iconInfo) args)
      port
      maybeNodeName
  NestedPApp headIcon args
    -> generalNestedPortAngles
      iconInfo
      pAppPortAngles
      (laValue headIcon)
      (fmap laValue args)
      port
      maybeNodeName
  NestedCaseIcon args
    -> nestedMultiIfPortAngles
      iconInfo
      ((fmap . fmap) (findIconFromName iconInfo) args)
      port
      maybeNodeName
  NestedMultiIfIcon args
    -> nestedMultiIfPortAngles
      iconInfo
      ((fmap . fmap) (findIconFromName iconInfo) args)
      port
      maybeNodeName