{-# LANGUAGE AllowAmbiguousTypes, NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module NodePlacementMap (
  placeNode
  , getQueryRects
) where

import qualified Data.Map as Map
import qualified Diagrams.Prelude as Dia
import qualified Diagrams.BoundingBox as Box
import Data.Maybe(listToMaybe)

import Types(EmbedInfo(..), AnnotatedGraph, Edge(..)
  , Drawing(..), NameAndPort(..)
  , SpecialDiagram, SpecialBackend, SpecialNum, NodeName(..)
  , SpecialQDiagram
  , NamedIcon(..), Icon(..), NodeInfo(..), IconInfo
  , Named(..)
  , TransformParams(..)
  , EdgeOption(..)
  , QueryValue(..)
  , DiaQuery
  , NodeQueryValue(..)
  )

import TextBox(transparentAlpha, letterHeight)

import IconToDiagram(iconToDiagram)

placeNode :: SpecialBackend b Double
  => IconInfo 
  -> Double
  -> (NamedIcon, Dia.P2 Double) 
  -> (NamedIcon, SpecialDiagram b Double)
placeNode iconInfo graphVizScale (namedIcon@(Named name icon), layoutPosition)
  = (namedIcon, Dia.place transformedDia diaPosition) where
      origDia = iconToDiagram
                iconInfo
                icon
                (TransformParams name 0)
      transformedDia = Dia.centerXY origDia
      diaPosition = getDiaPosition graphVizScale layoutPosition

-- getDiaPosition :: (Functor f, Fractional a) => f a -> f a
getDiaPosition graphVizScale layoutPosition = graphVizScale Dia.*^ layoutPosition


boundingBoxPadding :: Double
boundingBoxPadding = 0.5 * letterHeight

getQueryRects :: SpecialBackend b Double
  =>[(NamedIcon, SpecialDiagram b Double)]
  -> [SpecialQDiagram b Double]
getQueryRects iconAndPlacedNodes 
  = [box | (icon, diagram) <- iconAndPlacedNodes,
    let 
      box = Dia.value (queryValue icon) 
        $ Dia.opacity transparentAlpha 
        -- $ Dia.lc Dia.blue
        $ Dia.boundingRect
        $ Dia.frame boundingBoxPadding diagram]

queryValue :: (NamedIcon -> DiaQuery)
queryValue (Named name (Icon _ srcRef)) = [NodeQv $ NodeQueryValue srcRef name]