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
  , textBox
  )
where

import Diagrams.Prelude hiding ((&), (#), Name)
import Data.Maybe(fromMaybe)
import Data.Either(partitionEithers)
import Data.Typeable(Typeable)
import           Data.List                      ( isPrefixOf )

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
portSeparationSize :: (Fractional a) => a
portSeparationSize = 0.5

defaultOpacity :: (Fractional a) => a
defaultOpacity = 0.4

-- COLORS --
lineColorValue :: Colour Double
lineColorValue = lineC colorScheme

-- BEGIN diagram basic symbols --
applySymbol :: SpecialBackend b n => Colour Double -> SpecialQDiagram b n
applySymbol col
  = fc col $ lw none $ rotateBy (1/2) $ eqTriangle (2 * sizeUnit)

composeSymbol :: SpecialBackend b n => Colour Double -> SpecialQDiagram b n
composeSymbol col
  = lc col $ lwG defaultLineWidth $ wedge sizeUnit yDir halfTurn

inputPortSymbol :: SpecialBackend b n => SpecialQDiagram b n
inputPortSymbol = lw none $ fc lineColorValue $ circle (sizeUnit * 0.5)

multiIfVarSymbol :: SpecialBackend b n
  => Colour Double
  ->  SpecialQDiagram b n
multiIfVarSymbol color = alignB coloredSymbol  where
    symbol = vrule (2 * sizeUnit)
    coloredSymbol
      = lwG defaultLineWidth $ lc color (strokeLine symbol)

resultSymbol :: SpecialBackend b n 
  => SpecialQDiagram b n
resultSymbol = symbol where
  symbol = fc color $ lw none $ rotateBy (1/2) $ eqTriangle (5/4 * sizeUnit) -- cant be to big to fit in diagrams
  color = caseRhsC colorScheme

lambdaBodySymbol :: SpecialBackend b n =>
  SpecialQDiagram b n
lambdaBodySymbol
  = coloredTextBox (regionPerimC colorScheme) (opaque (bindTextBoxC colorScheme)) "lambda"

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
    halfHeight = (height diagram) / 2
    triangleOfssets = [halfHeight *^ (unitY+unitX), halfHeight *^ (unitY-unitX)]
    coloredTriangleToRight =  lwG defaultLineWidth $  lc borderColor $ triangleToRight
    diagramWithLines = centerY $ vcat [coloredLine, diagram, coloredLine]
    finalDiagram = centerX $ hcat [reflectX coloredTriangleToRight, diagramWithLines, coloredTriangleToRight]


appArgBox :: SpecialBackend b n
  => Colour Double 
  -> n
  -> n
  -> SpecialQDiagram b n
appArgBox borderColor topAndBottomWidth portHeight
  = coloredArgBox where
    coloredArgBox = lwG defaultLineWidth $ lcA (withOpacity borderColor defaultOpacity) argBox
    argBox = rect topAndBottomWidth (portHeight + portSeparationSize)

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
  CaseResultIcon -> identDiaFunc resultSymbol
  LambdaIcon x bodyExp _
    -> nestedLambda iconInfo x (findIconFromName iconInfo <$> bodyExp)
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
makePort ::  SpecialBackend b n => Port -> SpecialQDiagram b n
makePort x = named x mempty
--makePort x = circle 0.2 # fc green # named x
-- Note, the version of makePort below seems to have a different type.
--makePort x = textBox (show x) # fc green # named x

makeQualifiedPort :: SpecialBackend b n =>
  SpecialQDiagram b n -> Port -> NodeName -> SpecialQDiagram b n
makeQualifiedPort  symbol portNum name = portAndSymbol where 
  port = name .>> (makePort portNum)
  portAndSymbol = port <> symbol

-- Don't display " tempvar" from Translate.hs/matchesToCase
makeLabelledPort :: SpecialBackend b n =>
  Bool -> SpecialQDiagram b n ->  NodeName -> Port -> String ->  SpecialQDiagram b n
makeLabelledPort isInput symbol  name portNum str  
  | " tempvar" `isPrefixOf` str  = portAndSymbol
  | not (null str) = portSymbolAndLabel
  | otherwise = portAndSymbol
  where
    portAndSymbol = makeQualifiedPort symbol  portNum name
    label = transformableBindTextBox str 
    portSymbolAndLabel = if isInput
      then portAndSymbol ||| label
      else label ||| portAndSymbol


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
makeAppInnerIcon _iconInfo (TransformParams name _) _ portNum
  (Labeled Nothing str)
  = centerX $ makeLabelledPort True inputPortSymbol name portNum str 
makeAppInnerIcon iconInfo (TransformParams _ nestingLevel ) func _
  (Labeled (Just (Named iconNodeName icon)) _)
  = iconToDiagram
    iconInfo
    icon
    (TransformParams iconNodeName innerLevel)
  where
    innerLevel = if func then nestingLevel else nestingLevel + 1

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
  = named name $ centerXY finalDia
  where
    borderColor = borderColors !! nestingLevel
    
    constructorDiagram = makeTransformedText iconInfo tp maybeConstructorName
    
    paternCases::[SpecialQDiagram b n]
    paternCases = zipWith (makeAppInnerIcon iconInfo tp False) argPortsConst subIcons
    paternCasesCentredY = fmap centerY paternCases
    casesDia = hsep portSeparationSize paternCasesCentredY
    
    inputPortAndCases = makeLabelledPort True (centerX casesDia) name InputPortConst ""
    argBox = appArgBox -- TODO consider this alignment
             borderColor
             (width inputPortAndCases)
             (height inputPortAndCases)
    inputPortAndCasesInBox = argBox <> inputPortAndCases

    resultDia = makeQualifiedPort resultSymbol ResultPortConst name 
    
    finalDia = vcat [ inputPortAndCasesInBox, constructorDiagram , resultDia ]



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
  colorResultSymbol
  borderColors
  maybeFunText
  args
  tp@(TransformParams name nestingLevel)
  -- beside Place two monoidal objects (i.e. diagrams, paths, animations...) next to each other along the given vector.
  = named name finalDia
    where
      borderColor = borderColors !! nestingLevel

      argsDiagram =  argBox <> centerXY argPorts where
        argPortsUncentred =  zipWith ( makeAppInnerIcon iconInfo tp False) argPortsConst (fmap pure args)
        argPortsCentred  = fmap centerY argPortsUncentred
        argPorts = centerX $ hsep portSeparationSize argPortsCentred 
        argBox  = appArgBox
          borderColor
          (width argPorts)
          (height argPorts)

      transformedName = centerX $ makeTransformedText iconInfo tp (pure maybeFunText)
      resultPortDia =  (makeQualifiedPort resultSymbol ResultPortConst name ) 

      inputPort =  ( makeQualifiedPort inputPortSymbol InputPortConst name )
      finalDia = vcat [inputPort, argsDiagram, transformedName, resultPortDia] 

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
  (TransformParams name nestingLevel)
  = named name $ case inputAndArgs of
  [] -> mempty
  input : subicons -> centerXY finalDia where
    finalDia = hcat [inputIcon, allCases ,resultPort]

    -- TODO aplay this pattern to do other imput diagram in places
    inputPort = makeQualifiedPort inputPortSymbol InputPortConst name 
    inputIcon = case input of
      Just (Named _ _) -> (alignR (alignL $ placeSubIcon False input) <> inputPort)
      otherwise -> alignR (inputPortSymbol <> inputPort)

    resultPort = makeQualifiedPort resultSymbol ResultPortConst name 

    (iFConstIcons, iFVarIcons)
      = partitionEithers $ zipWith iconMapper argPortsConst subicons

    iconMapper (Port portNum) subicon
      | even portNum = Right ${- middle -} vcat [multiIfVarSymbol triangleColor, placeSubIcon True subicon, port ]
      | otherwise = Left $ vcat [inConstBox $ placeSubIcon False subicon , port] {- middle -}
      where
        port = makeQualifiedPort resultSymbol (Port portNum) name 

    iFVarAndConstIcons =
      zipWith combineIfIcons iFVarIcons iFConstIcons

    combineIfIcons iFVarIcon iFConstIcon
      = decisionDiagram where
        decisionDiagram = (alignB iFConstIcon) <>  (alignT iFVarIcon)

    multiIfDia = centerX (hsep portSeparationSize (alignL iFVarAndConstIcons))
    bigVerticalLine = lwG defaultLineWidth $ lc triangleColor $ hrule (width multiIfDia)
    allCases = multiIfDia <> bigVerticalLine

    placeSubIcon::(SpecialBackend b n) => Bool -> Maybe (Named Icon) -> SpecialQDiagram b n
    placeSubIcon _ mNameAndIcon = case mNameAndIcon of
      Nothing -> mempty
      Just (Named iconNodeName icon) -> 
                iconToDiagram
                iconInfo
                icon
                (TransformParams
                iconNodeName
                nestingLevel
                )

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
nestedLambda iconInfo paramNames mBodyExp (TransformParams name level)
  -- = centerXY $ lambdaBodyDiagram ||| centerY (named name inputOutputDiagram)
  = centerXY (named name inputsResultAndBodyDia)
  -- centerY (named name inputOutputDiagram)
  where
  portPorts = zipWith (makeLabelledPort True inputPortSymbol name) argPortsConst paramNames
  placedImputPorts = centerXY $ vsep portSeparationSize portPorts
  inputDiagram = placedImputPorts <> appArgBox (lamArgResC colorScheme) (width placedImputPorts) (height placedImputPorts)

  lambdaBodyDiagram = case mBodyExp of
    Nothing -> mempty
    Just (Named bodyNodeName bodyIcon)
      -> iconToDiagram
         iconInfo
         bodyIcon
         (TransformParams bodyNodeName level)

  resultDiagram = (alignB lambdaBodySymbol ) <> alignT( makeQualifiedPort resultSymbol ResultPortConst name )
         
  inputsResultAndBodyDia = vcat [lambdaBodyDiagram,inputDiagram, resultDiagram]

lambdaRegionSymbol :: forall b . SpecialBackend b Double
  => [SpecialQDiagram b Double]
  -> SpecialQDiagram b Double
lambdaRegionSymbol enclosedDiagarms
  = moveTo (centerPoint combinedDia) coloredContentsRect
  where
    combinedDia = mconcat enclosedDiagarms
    rectPadding = 3 * sizeUnit -- so result symbols on edge can fit
    contentsRect = dashingG [0.7 * sizeUnit, 0.3 * sizeUnit] 0
                   $ rect
                   (rectPadding + width combinedDia)
                   (rectPadding + height combinedDia)
    coloredContentsRect = lc lightgreen (lwG defaultLineWidth contentsRect)
-- END Main icons
-- END Icons


getArrowOpts :: (RealFloat n, Typeable n) =>
  (Maybe (Point V2 n),Maybe (Point V2 n))
  -> NameAndPort
  -> (Maybe (Angle n), Maybe (Angle n))
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

-- https://archives.haskell.org/projects.haskell.org/diagrams/doc/arrow.html

edgeSymbol :: (R1 (Diff p), Affine p, Transformable (Diff p (N t)),
                 TrailLike t, Floating (N (Diff p (N t))), Eq (N (Diff p (N t))),
                 V (Diff p (N t)) ~ V2, V t ~ Diff p) =>
                p (N t)
                -> p (N t)
                -> Maybe (Angle (N (Diff p (N t))))
                -> Maybe (Angle (N (Diff p (N t))))
                -> t
edgeSymbol formPoint toPoint anglesFrom anglesTo = fromSegments [bezier3 offsetToControl1 offsetToControl2 offsetToEnd] where
  angleFrom = fromMaybe (3/4 @@ turn) anglesFrom  -- } edges defaults to go down for unnamed nodes
  angleTo = fromMaybe (1/4 @@ turn) anglesTo  -- }
  scaleFactor = sizeUnit * 10.0
  offsetToEnd = toPoint .-. formPoint
  offsetToControl1 = rotate angleFrom (scale scaleFactor unitX)
  offsetToControl2 = rotate angleTo (scale scaleFactor unitX) ^+^ offsetToEnd