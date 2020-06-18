{-# LANGUAGE AllowAmbiguousTypes, NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module NodePlacementMap (
  getDiagramWidthAndHeight
  , placeNode
  , makePointToIcon
) where

import qualified Data.Map as Map
import qualified Diagrams.Prelude as Dia
import Data.Maybe(listToMaybe)

import Types(EmbedInfo(..), AnnotatedGraph, Edge(..)
  , Drawing(..), NameAndPort(..)
  , SpecialQDiagram, SpecialBackend, SpecialNum, NodeName(..)
  , NamedIcon, Icon(..), NodeInfo(..), IconInfo
  , Named(..)
  , TransformParams(..)
  , EdgeOption(..)
  )

import Diagrams.Prelude ( 
  height
  , width
  )

import IconToSymbolDiagram  ( iconToDiagram)
-- CONSTANT
graphvizScaleFactor :: (Fractional a) => a
graphvizScaleFactor = 0.12 -- For Neato
-- For Fdp
--scaleFactor = 0.09
--scaleFactor = 0.04

drawingToGraphvizScaleFactor :: Double
-- For Neato, ScaleOverlaps
--drawingToGraphvizScaleFactor = 0.08
drawingToGraphvizScaleFactor = 0.15

-- GVA.Width and GVA.Height have a minimum of 0.01
minialDiaDimention :: Double
minialDiaDimention = 0.01

getDiagramWidthAndHeight ::forall b . SpecialBackend b Double
  => SpecialQDiagram b Double
  -> (Double, Double)
getDiagramWidthAndHeight dummyDiagram = (diaWidth, diaHeight) where
  diaWidth = max (drawingToGraphvizScaleFactor * width dummyDiagram) minialDiaDimention
  diaHeight = max (drawingToGraphvizScaleFactor * height dummyDiagram) minialDiaDimention    


placeNode :: SpecialBackend b Double
  => IconInfo 
  -> (NamedIcon, Dia.P2 Double) 
  -> (NamedIcon, SpecialQDiagram b Double)
placeNode namedIcons (key@(Named name icon), targetPosition)
  = (key, Dia.place transformedDia diaPosition) where
      origDia = iconToDiagram
                namedIcons
                icon
                (TransformParams name 0)
      transformedDia = Dia.centerXY origDia
      diaPosition = graphvizScaleFactor Dia.*^ targetPosition

makePointToIcon :: [(Icon, Dia.BoundingBox Dia.V2 Double)]
                     -> Dia.P2 Double -> Maybe Icon
makePointToIcon iconAndBoudingRect point = maybeIcon where
  insideIcons = [i | (i, rect) <- iconAndBoudingRect, rect `Dia.contains'` point]
  maybeIcon = listToMaybe insideIcons