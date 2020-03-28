{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Symbols
  ( ColorStyle(..)
  , colorScheme
  , defaultLineWidth
  , iconToDiagram
  , multilineComment
  , lambdaRegionSymbol
  , getArrowOpts
  )
where

import Diagrams.Prelude hiding ((&), (#), Name)
import Data.Maybe(fromMaybe)
import Data.Either(partitionEithers)
import Data.Typeable(Typeable)

import Icons(findIconFromName,argPortsConst)
import           TextBox  ( bindTextBox
                          , defaultLineWidth
                          , coloredTextBox
                          , transformCorrectedTextBox
                          , transformableBindTextBox
                          , multilineComment
                          )
import Constants(pattern InputPortConst, pattern ResultPortConst)
import DrawingColors(colorScheme, ColorStyle(..))
import Types(Icon(..), SpecialQDiagram, SpecialBackend, SpecialNum
            , NodeName(..), Port(..), LikeApplyFlavor(..)
            , NamedIcon, Labeled(..), IconInfo
            , Named(..), NameAndPort(..)
            ,TransformParams(..),TransformableDia)

{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Unnecessary hiding" #-}

-- CONSTANTS --

sizeUnit :: (Fractional a) => a
sizeUnit = 0.5

defaultOpacity :: (Fractional a) => a
defaultOpacity = 0.4

-- COLORS --
lineColorValue :: Colour Double
lineColorValue = lineC colorScheme

-- BEGIN diagram basic symbols --
lineStartingSymbol ::  SpecialBackend b n
  => Colour Double -> SpecialQDiagram b n
lineStartingSymbol borderColor
  = lc borderColor $ lwG defaultLineWidth $ fc borderColor $ circle defaultLineWidth

applySymbol :: SpecialBackend b n => Colour Double -> SpecialQDiagram b n
applySymbol col
  = fc col $ lw none $ rotateBy (1/2) $ eqTriangle (2 * sizeUnit)

composeSymbol :: SpecialBackend b n => Colour Double -> SpecialQDiagram b n
composeSymbol col
  = lc col $ lwG defaultLineWidth $ wedge sizeUnit yDir halfTurn

portSymbol :: SpecialBackend b n => SpecialQDiagram b n
portSymbol = lw none $ fc lineColorValue $ circle (sizeUnit * 0.5)

multiIfConstSymbol :: SpecialBackend b n =>
  SpecialQDiagram b n -> SpecialQDiagram b n
multiIfConstSymbol portDia = coloredSymbol ||| portDia
  where
    line = strokeLine $ hrule (2 * sizeUnit)
    coloredSymbol
      = lwG defaultLineWidth $ lc (boolC colorScheme) line

multiIfVarSymbol :: SpecialBackend b n
  => Colour Double
  ->  SpecialQDiagram b n
multiIfVarSymbol color = coloredSymbol
  where
    symbol = hrule (2 * sizeUnit)
    coloredSymbol
      = lwG defaultLineWidth $ lc color (strokeLine symbol)

lambdaSymbol :: SpecialBackend b n =>
  SpecialQDiagram b n
lambdaSymbol
  = coloredTextBox (regionPerimC colorScheme) (opaque (bindTextBoxC colorScheme)) "lambda"

inIfConstBox :: SpecialBackend b n
  => Colour Double
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
inIfConstBox borderColor diagram 
  = finalDiagram where
    line = hrule (width diagram)
    coloredLine = lwG defaultLineWidth $  lc borderColor line
    triangleToRight = centerY $ fromOffsets triangleOfssets
    halfHeight = (height diagram) / 2
    triangleOfssets = [halfHeight *^ (unitY+unitX), halfHeight *^ (unitY-unitX)]
    coloredTriangleToRight =  lwG defaultLineWidth $  lc borderColor $ triangleToRight
    diagramWithLines = centerY $ vcat [coloredLine, diagram, coloredLine]
    finalDiagram = centerX $ hcat [reflectX coloredTriangleToRight, diagramWithLines, coloredTriangleToRight]


appArgBox :: (HasStyle a, Typeable (N a)
             , TrailLike a, RealFloat (N a), V a ~ V2)
          => Colour Double -> N a -> N a -> a
appArgBox borderColor topAndBottomWidth portHeight
  = coloredArgBox where
    coloredArgBox = lwG defaultLineWidth $ lcA (withOpacity borderColor defaultOpacity) argBox
    argBox = rect topAndBottomWidth (portHeight + verticalSeparation)
    verticalSeparation = sizeUnit

-- TODO Improve design to be more than a circle.
caseResult :: SpecialBackend b n 
  => SpecialQDiagram b n
caseResult = applySymbol caseCColor where
  caseCColor = caseRhsC colorScheme

caseC :: SpecialBackend b n 
  => SpecialQDiagram b n -> SpecialQDiagram b n
caseC portDia = caseResult <> portDia

-- >>>>>>>>>>>>>>>>>>>>>> SUB Diagrams <<<<<<<<<<<<<<<<<<<<<<<<
-- TODO Detect if we are in a loop (have called iconToDiagram on the same node
-- before)
iconToDiagram :: SpecialBackend b n
  => IconInfo
  -> Icon
  -> TransformableDia b n
iconToDiagram iconInfo icon = case icon of
  TextBoxIcon s -> textBox s
  BindTextBoxIcon s -> identDiaFunc $ bindTextBox s
  MultiIfIcon n -> nestedMultiIfDia iconInfo $ replicate (1 + (2 * n)) Nothing
  CaseIcon n -> nestedCaseDia iconInfo $ replicate (1 + (2 * n)) Nothing
  CaseResultIcon -> identDiaFunc caseResult
  LambdaIcon x bodyExp _
    -> nestedLambda iconInfo x (findIconFromName iconInfo <$> bodyExp)
  NestedApply flavor headIcon args
    -> nestedApplyDia
       iconInfo
       flavor
       (fmap (findIconFromName iconInfo) headIcon)
       ((fmap . fmap) (findIconFromName iconInfo) args)
  NestedPApp constructor args
    -> nestedPAppDia iconInfo (repeat $ patternC colorScheme) constructor args
  NestedCaseIcon args -> nestedCaseDia
                         iconInfo
                         ((fmap . fmap) (findIconFromName iconInfo) args)
  NestedMultiIfIcon args -> nestedMultiIfDia
                            iconInfo
                            ((fmap . fmap) (findIconFromName iconInfo) args)

-- BEGIN Diagram helper functions --

-- | Make an identity TransformableDia
identDiaFunc :: SpecialNum n => SpecialQDiagram b n -> TransformableDia b n
identDiaFunc dia transformParams = nameDiagram (tpName transformParams) dia                          

-- | Like beside, but it puts the second dia atop the first dia
beside' :: (Semigroup a, Juxtaposable a) => V a (N a) -> a -> a -> a
beside' dir dia1 dia2 = juxtapose dir dia1 dia2 <> dia1
-- TODO REMOVE TEXT FROM HERE
textBox :: SpecialBackend b n =>
  String -> TransformableDia b n
textBox t (TransformParams name _ reflect angle)
  = nameDiagram name $ transformCorrectedTextBox
    t (textBoxTextC colorScheme) (textBoxC colorScheme) reflect angle

-- | Names the diagram and puts all sub-names in the namespace of the top level
-- name.
nameDiagram :: SpecialNum n =>
  NodeName
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
nameDiagram name dia = named name (name .>> dia)

makeTransformedText :: SpecialBackend b n
  => IconInfo
  -> TransformParams n
  -> Labeled (Maybe NamedIcon)
  -> SpecialQDiagram b n
makeTransformedText iconInfo tp maybeFunText = case laValue maybeFunText of
  Just _ ->
    makeAppInnerIcon iconInfo tp True InputPortConst maybeFunText
  Nothing -> mempty

-- | Make an port with an integer name. Always use <> to add a ports
-- (not === or |||)  since mempty has no size and will not be placed where you
-- want it.
makePort :: SpecialNum n => Port -> SpecialQDiagram b n
makePort x = named x mempty
--makePort x = circle 0.2 # fc green # named x
-- Note, the version of makePort below seems to have a different type.
--makePort x = textBox (show x) # fc green # named x

makeQualifiedPort :: SpecialNum n => NodeName -> Port -> SpecialQDiagram b n
makeQualifiedPort n x = n .>> makePort x

makeLabelledPort :: SpecialBackend b n =>
  NodeName -> Bool -> Angle n -> String -> Port -> SpecialQDiagram b n
makeLabelledPort name reflect angle str portNum = case str of
  -- Don't display " tempvar" from Translate.hs/matchesToCase
  (' ':_) -> portAndCircle
  (_:_:_) -> label ||| portAndCircle
  _ -> portAndCircle
  where
    portAndCircle = makeQualifiedPort name portNum <> portSymbol
    label = transformableBindTextBox str reflect angle

-- END Diagram helper functions

-- BEGIN Icons --

-- BEGIN Main icons

-- BEGIN Apply like icons

makeAppInnerIcon :: SpecialBackend b n =>
  IconInfo ->
  TransformParams n ->
  Bool ->  -- If False then add one to the nesting level.
  Port ->  -- Port number (if the NamedIcon is Nothing)
  Labeled (Maybe NamedIcon) ->  -- The icon
  SpecialQDiagram b n
makeAppInnerIcon _iconInfo (TransformParams name _ reflect angle) _ portNum
  (Labeled Nothing str)
  = centerX $ makeLabelledPort name reflect angle str portNum
makeAppInnerIcon iconInfo (TransformParams _ nestingLevel reflect angle) func _
  (Labeled (Just (Named iconNodeName icon)) _)
  = iconToDiagram
    iconInfo
    icon
    (TransformParams iconNodeName innerLevel reflect angle)
  where
    innerLevel = if func then nestingLevel else nestingLevel + 1

nestedPAppDia :: SpecialBackend b n
  => IconInfo
  -> [Colour Double]
  -> Labeled (Maybe NamedIcon)
  -> [Labeled (Maybe NamedIcon)]
  -> TransformableDia b n
nestedPAppDia
  iconInfo
  borderColors
  maybeFunText
  subIcons
  tp@(TransformParams name nestingLevel _ _)
  = named name $ centerXY
    $ centerY finalDia ||| beside' unitX transformedText resultDia
  where
    borderColor = borderColors !! nestingLevel
    transformedText = makeTransformedText iconInfo tp maybeFunText
    resultDia = makeQualifiedPort name ResultPortConst
    separation = sizeUnit * 1.5
    casesDia = vsep separation paternCases
    paternCases = zipWith (makeAppInnerIcon iconInfo tp False) argPortsConst subIcons
    inputPortAndCases = makeQualifiedPort name InputPortConst <> alignT casesDia
    argBox = alignT $ appArgBox -- TODO consider this alignment
             borderColor
             (width inputPortAndCases)
             (height inputPortAndCases)
    finalDia = argBox <> inputPortAndCases



-- | apply port locations:
-- InputPortConst: Function
-- ResultPortConst: Result
-- Ports 2,3..: Arguments
generalNestedDia :: SpecialBackend b n
  => IconInfo
  -> (Colour Double -> SpecialQDiagram b n)
  -> [Colour Double]
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> TransformableDia b n
generalNestedDia
  iconInfo
  aplicationDia
  borderColors
  maybeFunText
  args
  tp@(TransformParams name nestingLevel _ _)
  -- beside Place two monoidal objects (i.e. diagrams, paths, animations...) next to each other along the given vector.
  = named name finalDia
    where
      borderColor = borderColors !! nestingLevel

      argPortsUncentred =  zipWith ( makeAppInnerIcon iconInfo tp False) argPortsConst (fmap pure args)
      argPortsCentred  = fmap centerY argPortsUncentred
      argPorts = centerX $ hsep sizeUnit argPortsCentred

      argsInputPorts =  makeQualifiedPort name InputPortConst <> argPorts
      argBox  = appArgBox
                borderColor
                (width argsInputPorts)
                (height argsInputPorts)
      argsInputDiagram = argBox <> argsInputPorts

      transformedName = centerX $ makeTransformedText iconInfo tp (pure maybeFunText)

      nameUnderArgsDia = beside (-unitY) argsInputDiagram transformedName

      resultPortDia =  (makeQualifiedPort name ResultPortConst) <> (lineStartingSymbol borderColor)

      finalDia = beside (-unitY) nameUnderArgsDia resultPortDia

nestedApplyDia :: SpecialBackend b n
  => IconInfo
  -> LikeApplyFlavor
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> TransformableDia b n
nestedApplyDia iconInfo flavor = case flavor of
  ApplyNodeFlavor
    -> generalNestedDia iconInfo applySymbol (nestingC colorScheme)
  ComposeNodeFlavor
    -> generalNestedDia iconInfo composeSymbol (repeat $ apply1C colorScheme)

-- END Apply like diagrams

-- BEGIN MultiIf and case icons --

-- | generalNestedMultiIf port layout:
-- 0 -> top
-- 1 -> bottom
-- odds -> left
-- evens -> right
generalNestedMultiIf :: SpecialBackend b n
                   => IconInfo
                   -> Colour Double
                   -> (SpecialQDiagram b n -> SpecialQDiagram b n)
                   -> SpecialQDiagram b n
                   -> [Maybe NamedIcon]
                   -> TransformableDia b n
generalNestedMultiIf iconInfo triangleColor _ bottomDia inputAndArgs
  (TransformParams name nestingLevel reflect angle)
  = named name $ case inputAndArgs of
  [] -> mempty
  input : subicons -> centerXY finalDia where
    finalDia = alignT (bottomDia <> makeQualifiedPort name ResultPortConst)
               <> alignB
               (inputIcon === (bigVerticalLine
                               <> multiIfDia
                               <> makeQualifiedPort name InputPortConst))

    inputIcon = placeSubIcon False input

    (iFConstIcons, iFVarIcons)
      = partitionEithers $ zipWith iconMapper argPortsConst subicons

    iconMapper (Port portNum) subicon
      | even portNum = Right ${- middle -} multiIfVarSymbol triangleColor ||| port ||| placeSubIcon True subicon
      | otherwise = Left $ inIfConstBox triangleColor $ placeSubIcon False subicon ||| port {- middle -}
      where
        port = makeQualifiedPort name (Port portNum)

    iFVarAndConstIcons =
      zipWith combineIfIcons iFVarIcons iFConstIcons

    combineIfIcons iFVarIcon iFConstIcon
      = verticalLine === placedAtRight where
        placedAtRight = (alignR iFConstIcon) <>  (alignL iFVarIcon)
        verticalLine = strutY 0.4

    multiIfDia = vcat (alignT iFVarAndConstIcons)
    bigVerticalLine
      = alignT
        $ lwG defaultLineWidth $ lc triangleColor $ vrule (height multiIfDia)

    placeSubIcon innerReflected mNameAndIcon = case mNameAndIcon of
      Nothing -> mempty
      Just (Named iconNodeName icon) -> if innerReflected
        then reflectX dia
        else dia
        where
          dia = iconToDiagram
                iconInfo
                icon
                (TransformParams
                  iconNodeName
                  nestingLevel
                  (innerReflected /= reflect)
                  angle)

-- | The ports of the multiIf icon are as follows:
-- InputPortConst: Top result port (not used)
-- ResultPortConst: Bottom result port
-- Ports 3,5...: The left ports for the booleans
-- Ports 2,4...: The right ports for the values
nestedMultiIfDia :: SpecialBackend b n =>
  IconInfo
  -> [Maybe NamedIcon]
  -> TransformableDia b n
nestedMultiIfDia iconInfo = generalNestedMultiIf iconInfo lineColorValue multiIfConstSymbol mempty



-- | The ports of the case icon are as follows:
-- InputPortConst: Top input port
-- ResultPortConst: Bottom result port
-- Ports 3,5...: The left ports for the results
-- Ports 2,4...: The right ports for the patterns
nestedCaseDia :: SpecialBackend b n
  => IconInfo
  -> [Maybe NamedIcon]
  -> TransformableDia b n
nestedCaseDia iconInfo
  = generalNestedMultiIf iconInfo (patternC colorScheme) caseC caseResult

-- END MultiIf and case icons

-- | The ports of flatLambdaIcon are:
-- 0: Result icon
-- 1: The lambda function value
-- 2,3.. : The parameters
nestedLambda ::  SpecialBackend b n
  => IconInfo
  -> [String]
  -> Maybe NamedIcon
  -> TransformableDia b n
nestedLambda iconInfo paramNames mBodyExp (TransformParams name level reflect angle)
  -- = centerXY $ lambdaBodyDiagram ||| centerY (named name inputOutputDiagram)
  = centerXY (named name inputsResultAndBodyDia)
  -- centerY (named name inputOutputDiagram)
  where
  portPorts = zipWith (makeLabelledPort name reflect angle) paramNames argPortsConst
  placedImputPorts = centerXY $ vsep (1 * sizeUnit) portPorts
  inputDiagram = placedImputPorts <> appArgBox (lamArgResC colorScheme) (width placedImputPorts) (height placedImputPorts)

  lambdaBodyDiagram = case mBodyExp of
    Nothing -> mempty
    Just (Named bodyNodeName bodyIcon)
      -> iconToDiagram
         iconInfo
         bodyIcon
         (TransformParams bodyNodeName level reflect angle)

  resultDiagram = (alignB lambdaSymbol ) <> (alignT $ makeQualifiedPort name ResultPortConst)
         
  inputsResultAndBodyDia = vcat [lambdaBodyDiagram,inputDiagram, resultDiagram]

lambdaRegionSymbol :: forall b . SpecialBackend b Double
  => [SpecialQDiagram b Double]
  -> SpecialQDiagram b Double
lambdaRegionSymbol enclosedDiagarms
  = moveTo (centerPoint combinedDia) coloredContentsRect
  where
    combinedDia = mconcat enclosedDiagarms
    rectPadding = 2 * sizeUnit
    contentsRect = dashingG [0.7 * sizeUnit, 0.3 * sizeUnit] 0
                   $ rect
                   (rectPadding + width combinedDia)
                   (rectPadding + height combinedDia)
    coloredContentsRect = lc lightgreen (lwG defaultLineWidth contentsRect)
-- END Main icons
-- END Icons

-- https://archives.haskell.org/projects.haskell.org/diagrams/doc/arrow.html

getArrowOpts :: (RealFloat n, Typeable n) =>
  (Maybe (Point V2 n),Maybe (Point V2 n))
  -> NameAndPort
  -> ([Angle n], [Angle n])
  -> (ArrowOpts n, Colour Double)
getArrowOpts
  (formMaybePoint, toMaybePoint)
  (NameAndPort (NodeName nodeNum) mPort)
  (anglesFrom,anglesTo)
  = (arrowOptions, shaftColor)
  where
    edgeColors = edgeListC colorScheme
    Port portNum = fromMaybe (Port 0) mPort
    namePortHash = mod (portNum + (503 * nodeNum)) (length edgeColors)
    shaftColor = edgeColors !! namePortHash
    formPoint = fromMaybe (p2 (0.0,0.0)) formMaybePoint
    toPoint = fromMaybe (p2  (0.0,-1.0)) toMaybePoint
    arrowOptions =
      -- arrowHead .~ DIA.noHead
      arrowHead .~ tri
      $ headStyle %~ fc shaftColor
      $ arrowTail .~ noTail
      $ arrowShaft .~ edgeSymbol formPoint toPoint anglesFrom anglesTo
      -- TODO Don't use a magic number for lengths (headLength and tailLength)
      $ lengths .~ global 0.5
      $ with

edgeSymbol formPoint toPoint angleFrom angleTo = fromSegments [bezier3 offsetToControl1 offsetToControl2 offsetToEnd] where
  scaleFactor = sizeUnit * 10.0
  offsetToEnd = toPoint .-. formPoint
  offsetToControl1 = scale scaleFactor (-unitY)
  offsetToControl2 = (scale scaleFactor unitY) ^+^ offsetToEnd