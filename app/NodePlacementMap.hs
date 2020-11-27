{-# LANGUAGE AllowAmbiguousTypes, NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module NodePlacementMap (
  placeNode
  , getQueryRects
) where

import qualified Diagrams.Prelude as Dia

import Types(SpecialDiagram, SpecialBackend
  , SpecialQDiagram
  , NamedIcon
  , Icon(..)
  , IconInfo
  , Named(..)
  , TransformParams(..)
  , QueryValue(..)
  , DiaQuery
  , NodeQueryValue(..)
  )

import TextBox(transparentAlpha, letterHeight)

import IconToDiagram(iconToDiagram)
import DrawingColors (ColorStyle)

placeNode :: SpecialBackend b
  => IconInfo
  -> ColorStyle Double 
  -> Double
  -> (NamedIcon, Dia.P2 Double) 
  -> (NamedIcon, SpecialDiagram b)
placeNode iconInfo colorStyle graphVizScale (namedIcon@(Named name icon), layoutPosition)
  = (namedIcon, Dia.place transformedDia diaPosition) where
      origDia = iconToDiagram
                iconInfo
                colorStyle
                icon
                (TransformParams name 0)
      transformedDia = Dia.centerXY origDia
      diaPosition = getDiaPosition graphVizScale layoutPosition

getDiaPosition :: (Functor f, Num a) => a -> f a -> f a
getDiaPosition graphVizScale layoutPosition = graphVizScale Dia.*^ layoutPosition


boundingBoxPadding :: Double
boundingBoxPadding = 0.5 * letterHeight

getQueryRects :: SpecialBackend b
  =>[(NamedIcon, SpecialDiagram b)]
  -> [SpecialQDiagram b]
getQueryRects iconAndPlacedNodes 
  = [box | (icon, diagram) <- iconAndPlacedNodes,
    let 
      box = Dia.value (queryValue icon) 
        $ Dia.opacity transparentAlpha 
        {-\$ Dia.lc Dia.blue -}
        $ Dia.boundingRect
        $ Dia.frame boundingBoxPadding diagram]

queryValue :: (NamedIcon -> DiaQuery)
queryValue (Named name (Icon _ srcRef)) = [NodeQv $ NodeQueryValue srcRef name]