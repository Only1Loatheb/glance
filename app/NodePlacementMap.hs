{-# LANGUAGE AllowAmbiguousTypes, NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module NodePlacementMap (
  placeNode
  , makePointToIcon
) where

import qualified Data.Map as Map
import qualified Diagrams.Prelude as Dia
import qualified Diagrams.BoundingBox as Box
import Data.Maybe(listToMaybe)

import Types(EmbedInfo(..), AnnotatedGraph, Edge(..)
  , Drawing(..), NameAndPort(..)
  , SpecialQDiagram, SpecialBackend, SpecialNum, NodeName(..)
  , NamedIcon, Icon(..), NodeInfo(..), IconInfo
  , Named(..)
  , TransformParams(..)
  , EdgeOption(..)
  )

import IconToSymbolDiagram  ( iconToDiagram)
-- CONSTANT
graphvizScaleFactor :: (Fractional a) => a
graphvizScaleFactor = 0.12 -- For Neato
-- For Fdp
--scaleFactor = 0.09
--scaleFactor = 0.04

placeNode :: SpecialBackend b Double
  => IconInfo 
  -> (NamedIcon, Dia.P2 Double) 
  -> (NamedIcon, SpecialQDiagram b Double)
placeNode namedIcons (key@(Named name icon), layoutPosition)
  = (key, Dia.place transformedDia diaPosition) where
      origDia = iconToDiagram
                namedIcons
                icon
                (TransformParams name 0)
      transformedDia = Dia.centerXY origDia
      diaPosition = getDiaPosition layoutPosition

getDiaPosition :: (Functor f, Fractional a) => f a -> f a
getDiaPosition layoutPosition = graphvizScaleFactor Dia.*^ layoutPosition

makePointToIcon :: [(NamedIcon, Dia.BoundingBox Dia.V2 Double)]
                     -> Dia.P2 Double -> Maybe NamedIcon
makePointToIcon iconAndBoudingRect point = maybeIcon where
  insideIcons = [i | (i, rect) <- iconAndBoudingRect, rect `Box.contains` point]
  maybeIcon = listToMaybe insideIcons