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
  , getArrowShadowOpts
  , getArrowBaseOpts
  , textBox
  )
where

import Diagrams.Prelude hiding ((&), (#), Name)
import Data.Maybe(fromMaybe)
import Data.Either(partitionEithers)
import Data.Typeable(Typeable)
import           Data.List                      ( isPrefixOf )

import Icons(findIconFromName)
import           TextBox  ( bindTextBox
                          , defaultLineWidth
                          , transformCorrectedTextBox
                          , transformableBindTextBox
                          , multilineComment
                          , letterHeight
                          )

import PortConstants(
  pattern InputPortConst,
  pattern ResultPortConst,
  argPortsConst,
  isInputPort,
  mixedPorts,
  resultPortsConst,
  casePortPairs
  )

import DrawingColors(colorScheme, ColorStyle(..))
import Types(Icon(..), SpecialQDiagram, SpecialBackend, SpecialNum
            , NodeName(..), Port(..), LikeApplyFlavor(..)
            , NamedIcon, Labeled(..), IconInfo
            , Named(..), NameAndPort(..)
            ,TransformParams(..),TransformableDia)

{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Unnecessary hiding" #-}

-- CONSTANTS --
symbolSize :: (Fractional a) => a
symbolSize = 0.5

boxPadding :: Fractional a => a
boxPadding = 2 * defaultLineWidth

portSeparationSize :: (Fractional a) => a
portSeparationSize = 0.3

lambdaRectPadding :: (Fractional a) => a
lambdaRectPadding = 3 * symbolSize -- so result symbols on edge can fit

defaultOpacity :: (Fractional a) => a
defaultOpacity = 0.4

defaultShadowOpacity :: (Fractional a) => a
defaultShadowOpacity = 0.6

arrowLineWidth :: Fractional a => a
arrowLineWidth = 2 * defaultLineWidth

arrowShadowWidth :: Fractional a => a
arrowShadowWidth = 3.8 * defaultLineWidth

-- COLORS --
lineColorValue :: Colour Double
lineColorValue = lineC colorScheme

-- BEGIN diagram basic symbols --
inputPortSymbol :: SpecialBackend b n => SpecialQDiagram b n
inputPortSymbol = lw none $ fc lineColorValue $ circle (symbolSize * 0.5)

resultPortSymbol :: SpecialBackend b n
  => SpecialQDiagram b n
resultPortSymbol = symbol where
  symbol = fc color $ lw none $ rotateBy (1/2) $ eqTriangle (5/4 * symbolSize) -- cant be to big to fit in diagrams
  color = caseRhsC colorScheme

multiIfVarSymbol :: SpecialBackend b n
  => Colour Double
  ->  SpecialQDiagram b n
multiIfVarSymbol color = alignB coloredSymbol  where
    symbol = vrule (2 * symbolSize)
    coloredSymbol
      = lwG defaultLineWidth $ lc color (strokeLine symbol)

lambdaBodySymbol :: SpecialBackend b n
  => Maybe String
  -> SpecialQDiagram b n
lambdaBodySymbol functionName
  = transformCorrectedTextBox (fromMaybe "lambda" functionName) (regionPerimC colorScheme) (bindTextBoxC colorScheme)

inMultiIfConstBox :: SpecialBackend b n
  => SpecialQDiagram b n -> SpecialQDiagram b n
inMultiIfConstBox = generalInConstBox (boolC colorScheme)

inCaseConstBox :: SpecialBackend b n
  => SpecialQDiagram b n -> SpecialQDiagram b n
inCaseConstBox = generalInConstBox (caseRhsC colorScheme)

generalInConstBox :: SpecialBackend b n
  => Colour Double
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
generalInConstBox borderColor diagram
  = finalDiagram where
    line = hrule (width diagram)
    coloredLine = lwG defaultLineWidth $  lc borderColor line
    triangleToRight = centerY $ fromOffsets triangleOfssets
    halfHeight = (boxPadding + height diagram) / 2
    triangleOfssets = [halfHeight *^ (unitY+unitX), halfHeight *^ (unitY-unitX)]
    coloredTriangleToRight =  lwG defaultLineWidth $  lc borderColor $ triangleToRight
    diagramWithLines = centerY $ vsep defaultLineWidth [coloredLine, diagram, coloredLine]
    finalDiagram = centerX $ hcat [reflectX coloredTriangleToRight, diagramWithLines, coloredTriangleToRight]

boxForDiagram :: SpecialBackend b n
  => Colour Double
  -> n
  -> n
  -> SpecialQDiagram b n
boxForDiagram borderColor topAndBottomWidth portHeight
  = coloredArgBox where
    rectWidth = max (topAndBottomWidth + boxPadding) letterHeight
    rectHeight = max (portHeight + boxPadding) letterHeight
    coloredArgBox = lwG defaultLineWidth $ lcA (withOpacity borderColor defaultOpacity) argBox
    argBox = rect rectWidth rectHeight

-- BEGIN Diagram helper functions --

-- | Make an identity TransformableDia
identDiaFunc :: SpecialNum n => SpecialQDiagram b n -> TransformableDia b n
identDiaFunc dia transformParams = nameDiagram (tpName transformParams) dia

textBox :: SpecialBackend b n =>
  String -> TransformableDia b n
textBox t (TransformParams name _)
  = nameDiagram name $ transformCorrectedTextBox
    t (textBoxTextC colorScheme) (textBoxC colorScheme)

-- | Names the diagram and puts all sub-names in the namespace of the top level
-- name.
nameDiagram :: SpecialNum n =>
  NodeName
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
nameDiagram name dia = named name (name .>> dia)

-- | Make an port with an integer name. Always use <> to add a ports
-- (not === or |||)  since mempty has no size and will not be placed where you
-- want it.
makePort ::  SpecialBackend b n => Port -> SpecialQDiagram b n
makePort x = named x mempty
--makePort x = circle 0.2 # fc green # named x
-- Note, the version of makePort below seems to have a different type.
--makePort x = textBox (show x) # fc green # named x

makeQualifiedPort :: SpecialBackend b n =>
  Port -> NodeName -> SpecialQDiagram b n
makeQualifiedPort port name = portAndSymbol where
  namedPort = name .>> (makePort port)
  portAndSymbol = namedPort <> symbol
  symbol = if isInputPort port then inputPortSymbol else resultPortSymbol

-- Don't display " tempvar" from Translate.hs/matchesToCase
makeLabelledPort :: SpecialBackend b n =>
  NodeName -> Port -> String ->  SpecialQDiagram b n
makeLabelledPort name port str
  | " tempvar" `isPrefixOf` str  = portAndSymbol
  | not (null str) = portSymbolAndLabel
  | otherwise = portAndSymbol
  where
    portAndSymbol = makeQualifiedPort port name
    label = transformableBindTextBox str
    portSymbolAndLabel = if isInputPort port
      then portAndSymbol ||| label
      else label ||| portAndSymbol

makePassthroughPorts :: SpecialBackend b n =>
  NodeName -> (Port,Port) -> String ->  SpecialQDiagram b n
makePassthroughPorts name (port1,port2) str
  | " tempvar" `isPrefixOf` str  = portsDiagram
  | not (null str) = portSymbolAndLabel
  | otherwise = portsDiagram
  where
    portAndSymbol1 = makeQualifiedPort port1 name
    portAndSymbol2 = makeQualifiedPort port2 name
    label = transformableBindTextBox str
    portsDiagram =  portAndSymbol1 === portAndSymbol2
    portSymbolAndLabel = portAndSymbol1 === label === portAndSymbol2

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
  CaseResultIcon -> identDiaFunc resultPortSymbol
  LambdaIcon x maybeName bodyExp _
    -> nestedLambda iconInfo x (findIconFromName iconInfo <$> bodyExp) maybeName
  NestedApply flavor headIcon args
    -> nestedApplyDia
       iconInfo
       flavor
       (fmap (findIconFromName iconInfo) headIcon)
       ((fmap . fmap) (findIconFromName iconInfo) args)
  NestedPatternApp constructor args
    -> nestedPatternAppDia iconInfo (repeat $ patternC colorScheme) constructor args
  NestedCaseIcon args -> nestedCaseDia
                         iconInfo
                         ((fmap . fmap) (findIconFromName iconInfo) args)
  NestedMultiIfIcon args -> nestedMultiIfDia
                            iconInfo
                            ((fmap . fmap) (findIconFromName iconInfo) args)

makeInputDiagram :: SpecialBackend b n
  => IconInfo
  -> TransformParams n
  -> Labeled (Maybe NamedIcon)
  -> NodeName
  -> SpecialQDiagram b n
makeInputDiagram iconInfo tp maybeFunText name = case laValue maybeFunText of
  Just _ ->
    makeAppInnerIcon iconInfo tp True InputPortConst maybeFunText
  Nothing -> makeQualifiedPort InputPortConst name
      -- becaues it can only be [function name, lambda, imputPort]

makeResultDiagram :: SpecialBackend b n
  => NodeName
  -> SpecialQDiagram b n
makeResultDiagram = makeQualifiedPort ResultPortConst

makeAppInnerIcon :: SpecialBackend b n
  => IconInfo
  -> TransformParams n
  -> Bool  -- If False then add one to the nesting level. 
  -> Port  -- Port number (if the NamedIcon is Nothing)
  -> Labeled (Maybe NamedIcon) -- The icon 
  -> SpecialQDiagram b n
makeAppInnerIcon _iconInfo (TransformParams name _) _isSameNestingLevel  port
  (Labeled Nothing str)
  = centerX $ makeLabelledPort name port str
makeAppInnerIcon iconInfo (TransformParams _ nestingLevel ) isSameNestingLevel _port
  (Labeled (Just (Named iconNodeName icon)) _)
  = iconToDiagram
    iconInfo
    icon
    (TransformParams iconNodeName innerLevel)
  where
    innerLevel = if isSameNestingLevel then nestingLevel else nestingLevel + 1

makePassthroughIcon :: SpecialBackend b n
  => IconInfo
  -> TransformParams n
  -> Bool  -- If False then add one to the nesting level. 
  -> (Port, Port)  -- Port number (if the NamedIcon is Nothing)
  -> Labeled (Maybe NamedIcon) -- The icon 
  -> SpecialQDiagram b n
makePassthroughIcon _iconInfo (TransformParams name _) _isSameNestingLevel  ports
  (Labeled Nothing str)
  = centerX $ makePassthroughPorts name ports str
makePassthroughIcon iconInfo (TransformParams _ nestingLevel ) isSameNestingLevel _ports
  (Labeled (Just (Named iconNodeName icon)) _)
  = iconToDiagram
    iconInfo
    icon
    (TransformParams iconNodeName innerLevel)
  where
    innerLevel = if isSameNestingLevel then nestingLevel else nestingLevel + 1

nestedPatternAppDia :: forall b n. SpecialBackend b n
  => IconInfo
  -> [Colour Double]
  -> Labeled (Maybe NamedIcon)
  -> [Labeled (Maybe NamedIcon)]
  -> TransformableDia b n
nestedPatternAppDia
  iconInfo
  borderColors
  maybeConstructorName
  subIcons
  tp@(TransformParams name nestingLevel)
  = named name $ centerY finalDia
  where
    borderColor = borderColors !! nestingLevel
    resultDia = makeResultDiagram name


    constructorDiagram = makeInputDiagram iconInfo tp maybeConstructorName name

    patternCases::[SpecialQDiagram b n]
    patternCases = zipWith (makePassthroughIcon iconInfo tp False) casePortPairs subIcons
    patternCasesCentred = fmap centerY patternCases
    patternDiagram = hsep portSeparationSize (constructorDiagram : patternCasesCentred)

    patternDiagramInBox = (centerXY $ patternDiagram)
      <> boxForDiagram borderColor (width patternDiagram) (height patternDiagram)

    finalDia = patternDiagramInBox  === resultDia



-- | apply port locations:
-- InputPortConst: Function
-- ResultPortConst: Result
-- Ports 2,3..: Arguments
generalNestedDia :: SpecialBackend b n
  => IconInfo
  -> [Colour Double]
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> TransformableDia b n
generalNestedDia
  iconInfo
  borderColors
  maybeFunText
  args
  tp@(TransformParams name nestingLevel)
  -- beside Place two monoidal objects (i.e. diagrams, paths, animations...) next to each other along the given vector.
  = named name finalDia
    where
      borderColor = borderColors !! nestingLevel
      boxWidth =  max (width transformedName) (width argPorts)

      argPortsUncentred =  zipWith ( makeAppInnerIcon iconInfo tp False) argPortsConst (fmap pure args)
      argPortsCentred  = fmap alignB argPortsUncentred
      argPorts = centerX $ hsep portSeparationSize argPortsCentred
      argsDiagram = (centerXY argPorts) <> (boxForDiagram borderColor boxWidth (height argPorts))

      resultDiagram =  makeResultDiagram name

      transformedName = makeInputDiagram iconInfo tp (pure maybeFunText) name

      functionDiagramInBox = transformedName <> (boxForDiagram borderColor boxWidth (height transformedName))

      finalDia = vcat [ argsDiagram,functionDiagramInBox, resultDiagram]

nestedApplyDia :: SpecialBackend b n
  => IconInfo
  -> LikeApplyFlavor
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> TransformableDia b n
nestedApplyDia iconInfo flavor = case flavor of
  ApplyNodeFlavor
    -> generalNestedDia iconInfo (nestingC colorScheme)
  ComposeNodeFlavor
    -> generalNestedDia iconInfo (repeat $ apply1C colorScheme)

-- BEGIN MultiIf and case icons --
-- | The ports of the multiIf icon are as follows:
-- InputPortConst: Top result port (not used)
-- ResultPortConst: Bottom result port
-- Ports 3,5...: The left ports for the booleans
-- Ports 2,4...: The right ports for the values
nestedMultiIfDia :: SpecialBackend b n =>
  IconInfo
  -> [Maybe NamedIcon]
  -> TransformableDia b n
nestedMultiIfDia iconInfo
  = generalNestedMultiIf iconInfo lineColorValue inMultiIfConstBox

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
  = generalNestedMultiIf iconInfo (patternC colorScheme) inCaseConstBox

-- | generalNestedMultiIf port layout:
-- 0 -> top
-- 1 -> bottom
-- odds -> left
-- evens -> right
generalNestedMultiIf ::forall b n. SpecialBackend b n
                   => IconInfo
                   -> Colour Double
                   -> (SpecialQDiagram b n -> SpecialQDiagram b n)
                   -> [Maybe NamedIcon]
                   -> TransformableDia b n
generalNestedMultiIf iconInfo triangleColor inConstBox inputAndArgs
  tp@(TransformParams name _nestingLevel)
  = named name $ case inputAndArgs of
  [] -> error "empty multiif"-- mempty
  input : subicons -> centerXY finalDia where
    finalDia = hcat [inputDiagram, allCases ,resultPort]

    inputDiagram = makeInputDiagram iconInfo tp (pure input) name

    resultPort = makeResultDiagram name

    (iFConstIcons, iFVarIcons)
      = partitionEithers $ zipWith iconMapper mixedPorts subicons

    isSameNestingLevel = True

    iconMapper port subicon
      | isInputPort port = Left $ inConstBox innerIcon{- middle -}
      | otherwise = Right ${- middle -} vcat [multiIfVarSymbol triangleColor, innerIcon]
      where
        innerIcon = makeAppInnerIcon iconInfo tp isSameNestingLevel port (Labeled subicon "")

    iFVarAndConstIcons =
      zipWith combineIfIcons iFVarIcons iFConstIcons

    combineIfIcons iFVarIcon iFConstIcon
      = decisionDiagram where
        spaceForBigLine = strutY defaultLineWidth
        ifConstDiagram = vcat [iFConstIcon,spaceForBigLine]
        decisionDiagram = (alignB ifConstDiagram) <>  (alignT iFVarIcon)

    multiIfDia = centerX $ hsep portSeparationSize  iFVarAndConstIcons
    bigVerticalLine = lwG defaultLineWidth $ lc triangleColor $ hrule (width multiIfDia)
    allCases = multiIfDia <> bigVerticalLine

-- END MultiIf and case icons

-- | The ports of flatLambdaIcon are:
-- 0: Result icon
-- 1: The lambda function value
-- 2,3.. : The parameters
nestedLambda ::  SpecialBackend b n
  => IconInfo
  -> [String]
  -> Maybe NamedIcon
  -> Maybe String
  -> TransformableDia b n
nestedLambda iconInfo paramNames mBodyExp maybeName tp@(TransformParams name _level)
  = centerXY (named name inputsResultAndBodyDia)
  where
  innerOutputPorts = zipWith (makeLabelledPort name) resultPortsConst paramNames
  placedOutputPorts = centerXY $ vsep portSeparationSize innerOutputPorts
  innerOutputDiagram = placedOutputPorts
    <> boxForDiagram (lamArgResC colorScheme) (width placedOutputPorts) (height placedOutputPorts)

  inputDiagram = makeInputDiagram iconInfo tp (pure mBodyExp) name
  inputDiagramInBox = inputDiagram
    <> boxForDiagram (lamArgResC colorScheme) (width inputDiagram) (height inputDiagram)

  resultDiagram = makeResultDiagram name

  inputsResultAndBodyDia = vcat [inputDiagramInBox,innerOutputDiagram,lambdaBodySymbol maybeName, resultDiagram]

lambdaRegionSymbol :: forall b . SpecialBackend b Double
  => [SpecialQDiagram b Double]
  -> SpecialQDiagram b Double
lambdaRegionSymbol enclosedDiagarms
  = moveTo (centerPoint combinedDia) coloredContentsRect
  where
    combinedDia = mconcat enclosedDiagarms
    contentsRect = dashingG [0.7 * symbolSize, 0.3 * symbolSize] 0
                   $ rect
                   (lambdaRectPadding + width combinedDia)
                   (lambdaRectPadding + height combinedDia)
    coloredContentsRect = lc lightgreen (lwG defaultLineWidth contentsRect)
-- END Main icons
-- END Icons

getArrowShadowOpts :: (RealFloat n, Typeable n)
  => (Maybe (Point V2 n),Maybe (Point V2 n))
  -> (Maybe (Angle n), Maybe (Angle n))
  -> ArrowOpts n
getArrowShadowOpts maybePoints maybeAngles =
  shaftStyle %~ (lwG arrowShadowWidth .
                lcA $ withOpacity shaftColor defaultShadowOpacity)
  $ headStyle %~ fc shaftColor
  $ getArrowOpts maybePoints maybeAngles where
    shaftColor = backgroundC colorScheme

getArrowBaseOpts :: (RealFloat n, Typeable n)
  => NameAndPort
  -> (Maybe (Point V2 n),Maybe (Point V2 n))
  -> (Maybe (Angle n), Maybe (Angle n))
  -> ArrowOpts n
getArrowBaseOpts (NameAndPort (NodeName nodeNum) mPort) maybePoints maybeAngles
  = shaftStyle %~ (lwG arrowLineWidth . lc shaftColor)
  $ headStyle %~ fc shaftColor
  $ getArrowOpts maybePoints maybeAngles where
    edgeColors = edgeListC colorScheme
    Port portNum = fromMaybe (Port 0) mPort
    namePortHash = mod (portNum + (503 * nodeNum)) (length edgeColors)
    shaftColor = edgeColors !! namePortHash

getArrowOpts :: (RealFloat n, Typeable n)
  => (Maybe (Point V2 n),Maybe (Point V2 n))
  -> (Maybe (Angle n), Maybe (Angle n))
  -> ArrowOpts n
getArrowOpts
  (formMaybePoint, toMaybePoint)
  (anglesFrom,anglesTo)
  = arrowOptions
  where
    formPoint = fromMaybe (p2 (0.0,0.0)) formMaybePoint
    toPoint = fromMaybe (p2  (0.0,-1.0)) toMaybePoint
    arrowOptions =
      -- arrowHead .~ DIA.noHead
      arrowHead .~ tri
      $ arrowTail .~ noTail
      $ arrowShaft .~ edgeSymbol formPoint toPoint anglesFrom anglesTo
      -- TODO Don't use a magic number for lengths (headLength and tailLength)
      $ lengths .~ global 0.5
      $ with

-- https://archives.haskell.org/projects.haskell.org/diagrams/doc/arrow.html

edgeSymbol :: (R1 (Diff p), Affine p, Transformable (Diff p (N t)),
                 TrailLike t, Floating (N (Diff p (N t))), Eq (N (Diff p (N t))),
                 V (Diff p (N t)) ~ V2, V t ~ Diff p)
  => p (N t)
  -> p (N t)
  -> Maybe (Angle (N (Diff p (N t))))
  -> Maybe (Angle (N (Diff p (N t))))
  -> t
edgeSymbol formPoint toPoint anglesFrom anglesTo = fromSegments [bezier3 offsetToControl1 offsetToControl2 offsetToEnd] where
  angleFrom = fromMaybe (3/4 @@ turn) anglesFrom  -- } edges defaults to go down for unnamed nodes
  angleTo = fromMaybe (1/4 @@ turn) anglesTo  -- }
  scaleFactor = symbolSize * 10.0
  offsetToEnd = toPoint .-. formPoint
  offsetToControl1 = rotate angleFrom (scale scaleFactor unitX)
  offsetToControl2 = rotate angleTo (scale scaleFactor unitX) ^+^ offsetToEnd