{-# LANGUAGE AllowAmbiguousTypes, NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module NodePlacementMap (
  placeNode
  , getQueryRects
) where

import qualified Diagrams.Prelude as Dia

import Types(InCaseOrInApply(..), SpecialDiagram, SpecialBackend
  , SpecialQDiagram
  , NamedIcon
  , Icon(..)
  , IconInfo
  , Named(..)
  , DrawingInfo(..)
  , QueryValue(..)
  , DiaQuery
  , NodeQueryValue(..)
  , ColorStyle
  , ColorStyle'(..)
  )

import TextBox(transparentAlpha, letterHeight)

import IconToDiagram(iconToDiagram)

placeNode :: SpecialBackend b
  => IconInfo
  -> ColorStyle 
  -> Double
  -> (NamedIcon, Dia.P2 Double) 
  -> (NamedIcon, SpecialDiagram b)
placeNode iconInfo colorStyle graphVizScale (namedIcon@(Named name icon), layoutPosition)
  = (namedIcon, Dia.place transformedDia diaPosition) where
      origDia = iconToDiagram
                iconInfo
                icon
                (DrawingInfo name 0 None colorStyle)
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