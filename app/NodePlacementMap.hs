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
  , NamedIcon, Icon(..), NodeInfo(..), IconInfo
  , Named(..)
  , TransformParams(..)
  , EdgeOption(..)
  )

import TextBox(transparentAlpha)

import IconToSymbolDiagram  ( iconToDiagram)

import Util(queryValue)

-- CONSTANT
graphvizScaleFactor :: (Fractional a) => a
graphvizScaleFactor = 0.12 -- For Neato
-- For Fdp
--scaleFactor = 0.09
--scaleFactor = 0.04

placeNode :: SpecialBackend b Double
  => IconInfo 
  -> (NamedIcon, Dia.P2 Double) 
  -> (NamedIcon, SpecialDiagram b Double)
placeNode iconInfo (namedIcon@(Named name icon), layoutPosition)
  = (namedIcon, Dia.place transformedDia diaPosition) where
      origDia = iconToDiagram
                iconInfo
                icon
                (TransformParams name 0)
      transformedDia = Dia.centerXY origDia
      diaPosition = getDiaPosition layoutPosition

getDiaPosition :: (Functor f, Fractional a) => f a -> f a
getDiaPosition layoutPosition = graphvizScaleFactor Dia.*^ layoutPosition


boundingBoxPadding :: Double
boundingBoxPadding = 2

getQueryRects :: SpecialBackend b Double
  =>[(NamedIcon, SpecialDiagram b Double)]
  -> [SpecialQDiagram b Double]
getQueryRects iconAndPlacedNodes 
  = [box | (icon, diagram) <- iconAndPlacedNodes,
    let 
      box = Dia.value (queryValue icon) 
        $ Dia.opacity transparentAlpha $  Dia.boundingRect
        $ Dia.frame boundingBoxPadding diagram]