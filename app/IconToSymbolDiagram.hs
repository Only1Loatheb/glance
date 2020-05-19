{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IconToSymbolDiagram
  ( ColorStyle(..)
  , colorScheme
  , defaultLineWidth
  , iconToDiagram
  , multilineComment
  , getArrowShadowOpts
  , getArrowBaseOpts
  , lambdaRegionToDiagram
  , nameDiagram
  )
where

import Diagrams.Prelude hiding ((&), (#), Name)
import Diagrams.TwoD.Combinators(strutR2)

import Data.Maybe(fromMaybe)
import Data.Either(partitionEithers)
import Data.Typeable(Typeable)
import Data.List(isPrefixOf)

import Icons(
  findMaybeIconFromName
  , findMaybeIconsFromNames
  )
import TextBox (
  coloredTextBox
  , defaultLineWidth
  , multilineComment
  , letterHeight
  )

import PortConstants(
  pattern InputPortConst
  , pattern ResultPortConst
  , argPortsConst
  , resultPortsConst
  , isInputPort
  , mixedPorts
  , pattern PatternValuePortConst
  )
import StringSymbols(
  ifConditionConst
  , tempVarPrefix
  , patternSubscribedValueStr
  )

import DrawingColors(colorScheme, ColorStyle(..))
import Types(Icon(..), SpecialQDiagram, SpecialBackend, SpecialNum
            , NodeName(..), Port(..), LikeApplyFlavor(..)
            , NamedIcon, Labeled(..), IconInfo
            , Named(..), NameAndPort(..)
            ,TransformParams(..),TransformableDia
            ,CaseOrMultiIfTag(..))

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
lambdaRectPadding = 2 * letterHeight + defaultLineWidth

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
resultPortSymbol = valueSymbol -- memptyWithPosition
  
valueSymbol ::SpecialBackend b n => SpecialQDiagram b n
valueSymbol = fc color $ lw none $ rotateBy (1/2) $ eqTriangle (5/4 * symbolSize) where -- cant be to big to fit in diagrams
  color = caseRhsC colorScheme

multiIfVarSymbol :: SpecialBackend b n
  => Colour Double
  ->  SpecialQDiagram b n
multiIfVarSymbol color = alignB coloredSymbol  where
    symbol = vrule (2 * symbolSize)
    coloredSymbol
      = lwG defaultLineWidth $ lc color (strokeLine symbol)

inNoteFrame :: SpecialBackend b n
  => Colour Double
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
inNoteFrame borderColor diagram
  = centerXY diagram <> coloredFrame where
  
    boxHeight = boxPadding + height diagram
    boxWidth = boxPadding + width diagram
    cornerSize = letterHeight / 2
    notCornerHeight = boxHeight - cornerSize 
    frameWidth = boxWidth + 2 * cornerSize
  
    offsets = [
      frameWidth *^ unitX
      , notCornerHeight *^ unitY
      , cornerSize *^ (-unitX + unitY)
      , cornerSize *^ (-unitY)
      , cornerSize *^ unitX
      , cornerSize *^ (-unitX + unitY)
      , (boxWidth + cornerSize) *^ (-unitX)
      , boxHeight *^ (-unitY)
      ]
  
    decisionFrame = centerXY $ strokeLoop $  closeLine $ fromOffsets offsets
  
    coloredFrame = lwG (defaultLineWidth/2) $  lc borderColor decisionFrame

lambdaBodySymbol :: SpecialBackend b n
  => String
  -> SpecialQDiagram b n
lambdaBodySymbol t = inNoteFrame borderColor textBox where
  borderColor = regionPerimC colorScheme
  textBox = coloredTextBox borderColor t 

inMultiIfDecisionFrame :: SpecialBackend b n
  => SpecialQDiagram b n -> SpecialQDiagram b n
inMultiIfDecisionFrame = inDecisionFrame (boolC colorScheme)

inCaseDecisionFrame :: SpecialBackend b n
  => SpecialQDiagram b n -> SpecialQDiagram b n
inCaseDecisionFrame = inDecisionFrame (caseRhsC colorScheme)

inDecisionFrame :: SpecialBackend b n
  => Colour Double
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
inDecisionFrame borderColor diagram
  = centerXY diagram <> coloredFrame where
    boxHeight = boxPadding + height diagram
    halfBoxHeight = boxHeight / 2
    boxWidth = boxPadding + width diagram

    topRightOffsets = [
      halfBoxHeight *^ (unitY+unitX)
      , halfBoxHeight *^ (unitY-unitX)
      , boxWidth *^ (-unitX)
      ]
    bottomLeftOffsets = map negate topRightOffsets
    offsets = topRightOffsets ++ bottomLeftOffsets

    decisionFrame = centerXY $ strokeLoop $  closeLine $ fromOffsets offsets

    coloredFrame = lwG defaultLineWidth $  lc borderColor decisionFrame

boxForDiagram :: SpecialBackend b n
  => SpecialQDiagram b n
  -> Colour Double
  -> n
  -> n
  -> SpecialQDiagram b n
boxForDiagram diagram borderColor diagramWidth diagramHeight
  = centerXY diagram <> coloredArgBox where
    rectWidth = max (diagramWidth + boxPadding) letterHeight
    rectHeight = max (diagramHeight + boxPadding) letterHeight
    argBox = rect rectWidth rectHeight
    coloredArgBox = lwG defaultLineWidth $ lcA (withOpacity borderColor defaultOpacity) argBox

-- BEGIN Diagram helper functions --
memptyWithPosition :: SpecialBackend b n => SpecialQDiagram b n
memptyWithPosition = strutR2 (V2 0 0)
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

makeQualifiedPort :: SpecialBackend b n 
  =>  Port -> NodeName -> SpecialQDiagram b n
makeQualifiedPort = makeQualifiedPort' inputPortSymbol resultPortSymbol

makeQualifiedPort' :: SpecialBackend b n => 
                        SpecialQDiagram b n
                        -> SpecialQDiagram b n -> Port -> NodeName -> SpecialQDiagram b n
makeQualifiedPort' inputSymbol resultSymbol port name = portAndSymbol where
  namedPort = name .>> (makePort port)
  portAndSymbol = namedPort <> symbol
  symbol = if isInputPort port then inputSymbol else resultSymbol

-- Don't display " tempvar" from SimpSyntaxToSyntaxGraph.hs/matchesToCase
makeLabelledPort :: SpecialBackend b n =>
  NodeName -> Port -> String ->  SpecialQDiagram b n
makeLabelledPort name port str
  = choosePortDiagram str portAndSymbol portSymbolAndLabel where
    portAndSymbol = makeQualifiedPort port name
    label = coloredTextBox (textBoxTextC colorScheme) str
    portSymbolAndLabel = if isInputPort port
      then portAndSymbol === label
      else label === portAndSymbol

choosePortDiagram :: SpecialBackend b n =>
  String -> SpecialQDiagram b n -> SpecialQDiagram b n ->SpecialQDiagram b n
choosePortDiagram str portAndSymbol portSymbolAndLabel
  = centerX symbol where
    symbol
      | tempVarPrefix `isPrefixOf` str = portAndSymbol
      | not (null str) = portSymbolAndLabel
      | otherwise = portAndSymbol
      
-- makePassthroughPorts :: SpecialBackend b n =>
--   NodeName -> (Port,Port) -> String ->  SpecialQDiagram b n
-- makePassthroughPorts name (port1,port2) str
--   = choosePortDiagram str portsDiagram portSymbolsAndLabel where
--     portAndSymbol1 = makeQualifiedPort port1 name
--     portAndSymbol2 = makeQualifiedPort port2 name
--     label = transformablePortTextBox str
--     portsDiagram =  portAndSymbol1 === portAndSymbol2
--     portSymbolsAndLabel = portAndSymbol1 === label === portAndSymbol2

-- >>>>>>>>>>>>>>>>>>>>>> SUB Diagrams <<<<<<<<<<<<<<<<<<<<<<<<
-- TODO Detect if we are in a loop (have called iconToDiagram on the same node
-- before)

-- | Make an identity TransformableDia
identDiaFunc :: SpecialNum n => SpecialQDiagram b n -> TransformableDia b n
identDiaFunc dia transformParams = nameDiagram (tpName transformParams) dia

iconToDiagram :: SpecialBackend b n
  => IconInfo
  -> Icon
  -> TransformableDia b n
iconToDiagram iconInfo icon = case icon of
  TextBoxIcon s -> literalDiagram s
  BindTextBoxIcon s -> bindDiagram s
  MultiIfIcon n -> nestedMultiIfDia iconInfo (replicate (1 + (2 * n)) Nothing) MultiIfTag
  CaseIcon n -> nestedCaseDia iconInfo (replicate (1 + (2 * n)) Nothing) CaseTag
  CaseResultIcon -> identDiaFunc resultPortSymbol
  FunctionArgIcon argumentNames -> functionArgDia argumentNames
  FunctionDefIcon (Labeled _ str) _ -> functionDefDia str 
  NestedApply flavor headIcon args
    -> nestedApplyDia
       iconInfo
       flavor
       (findMaybeIconFromName iconInfo headIcon)
       (findMaybeIconsFromNames iconInfo args)
  NestedPatternApp constructor args rhsNodeName
    -> nestedPatternAppDia iconInfo (repeat $ patternC colorScheme) constructor args (findMaybeIconFromName iconInfo rhsNodeName)
  NestedCaseIcon args -> nestedCaseDia
                         iconInfo
                         (findMaybeIconsFromNames iconInfo args)
                         CaseTag
  NestedMultiIfIcon args -> nestedMultiIfDia
                            iconInfo
                            (findMaybeIconsFromNames iconInfo args)
                            MultiIfTag
  ListCompIcon {} -> listCompDiagram iconInfo (nestingC colorScheme) Nothing (replicate (1 + 7) Nothing) --(findMaybeIconsFromNames iconInfo args) -- listCompDiagram

bindDiagram :: SpecialBackend b n =>
  String -> TransformableDia b n
bindDiagram t transformParams = finalDia where
  bindDia = nameDiagram (tpName transformParams) inputPortSymbol 
  bindText = coloredTextBox (bindTextBoxTextC colorScheme) t
  finalDia = bindDia === bindText

literalDiagram :: SpecialBackend b n =>
  String -> TransformableDia b n
literalDiagram t transformParams = finalDia where
  finalDia = bindText === outputDia
  outputDia =  nameDiagram (tpName transformParams) memptyWithPosition
  bindText = coloredTextBox (textBoxTextC colorScheme) t

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
  = makeLabelledPort name port str
makeAppInnerIcon iconInfo (TransformParams _ nestingLevel ) isSameNestingLevel _port
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
  -> Maybe NamedIcon
  -> TransformableDia b n
nestedPatternAppDia
  iconInfo
  borderColors
  maybeConstructorName
  subIcons
  rhsNodeName
  tp@(TransformParams name nestingLevel)
  = named name $ centerY finalDia
  where
    borderColor = borderColors !! nestingLevel
    resultDia = makeResultDiagram name

    subscribedValueDia = alignT $ makeAppInnerIcon iconInfo tp True PatternValuePortConst (Labeled rhsNodeName patternSubscribedValueStr)

    constructorDiagram = alignB $ makeInputDiagram iconInfo tp maybeConstructorName name

    patternCases::[SpecialQDiagram b n]
    patternCases = alignB $ zipWith (makeAppInnerIcon iconInfo tp False) resultPortsConst subIcons
    patternDiagram = hsep portSeparationSize $  constructorDiagram : subscribedValueDia : patternCases

    patternDiagramInBox = boxForDiagram patternDiagram borderColor (width patternDiagram) (height patternDiagram)

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
      argsDiagram = boxForDiagram argPorts borderColor boxWidth (height argPorts)

      resultDiagram =  makeResultDiagram name

      transformedName = makeInputDiagram iconInfo tp (pure maybeFunText) name

      functionDiagramInBox = boxForDiagram transformedName borderColor boxWidth (height transformedName)

      finalDia = vcat [ argsDiagram,functionDiagramInBox, resultDiagram]

listCompDiagram :: SpecialBackend b n
  => IconInfo
  -> [Colour Double]
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> TransformableDia b n
listCompDiagram
  iconInfo
  borderColors
  maybeFunText
  args
  tp@(TransformParams name nestingLevel)
  = named name finalDia
    where
      borderColor = borderColors !! nestingLevel
      boxWidth =  max (width transformedName) (width argPorts)

      argPortsUncentred =  zipWith ( makeAppInnerIcon iconInfo tp False) mixedPorts (fmap pure args)
      argPortsCentred  = fmap alignB argPortsUncentred
      argPorts = centerX $ hsep portSeparationSize argPortsCentred
      argsDiagram = boxForDiagram argPorts borderColor boxWidth (height argPorts)

      resultDiagram =  makeResultDiagram name

      transformedName = makeInputDiagram iconInfo tp (pure maybeFunText) name

      functionDiagramInBox = boxForDiagram transformedName borderColor boxWidth (height transformedName)

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

nestedMultiIfDia :: SpecialBackend b n =>
  IconInfo
  -> [Maybe NamedIcon]
  -> CaseOrMultiIfTag
  -> TransformableDia b n
nestedMultiIfDia iconInfo
  = generalNestedMultiIf iconInfo lineColorValue inMultiIfDecisionFrame

nestedCaseDia :: SpecialBackend b n
  => IconInfo
  -> [Maybe NamedIcon]
  -> CaseOrMultiIfTag
  -> TransformableDia b n
nestedCaseDia iconInfo
  = generalNestedMultiIf iconInfo (patternC colorScheme) inCaseDecisionFrame

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
                   -> CaseOrMultiIfTag
                   -> TransformableDia b n
generalNestedMultiIf iconInfo symbolColor inConstBox inputAndArgs flavor
  tp@(TransformParams name _nestingLevel)
  = named name $ case inputAndArgs of
  [] -> error "empty multiif"-- mempty
  input : subicons -> centerXY finalDia where
    finalDia = vcat [inputDiagram, allCasesAtRight ,resultPort]

    resultPort = makeResultDiagram name

    inputDiagram
      | flavor == MultiIfTag = alignBR 
        $ coloredTextBox symbolColor ifConditionConst
      | otherwise = makeInputDiagram iconInfo tp (pure input) name

    (iFConstIcons, iFVarIcons)
      = partitionEithers $ zipWith iconMapper mixedPorts subicons

    iconMapper port subicon
      | isInputPort port = Left $ inConstBox innerIcon{- middle -}
      | otherwise = Right ${- middle -} vcat [multiIfVarSymbol symbolColor, innerIcon]
      where
        innerIcon = makeAppInnerIcon iconInfo tp isSameNestingLevel port (Labeled subicon "")
        isSameNestingLevel = True

    iFVarAndConstIcons =
      zipWith combineIfIcons iFVarIcons iFConstIcons

    combineIfIcons iFVarIcon iFConstIcon
      = decisionDiagram where
        spaceForBigLine = strutY defaultLineWidth
        ifConstDiagram = vcat [iFConstIcon,spaceForBigLine]
        decisionDiagram = (alignB ifConstDiagram) <>  (alignT iFVarIcon)

    multiIfDia = centerX $ hsep portSeparationSize  iFVarAndConstIcons
    decisionLine = lwG defaultLineWidth $ lc symbolColor $ hrule (width multiIfDia)
    allCases = multiIfDia <> decisionLine
    inputLambdaLine = lwG defaultLineWidth $ lc symbolColor $ vrule (height allCases)
    allCasesAtRight = alignT inputLambdaLine <> alignTL allCases

functionArgDia :: SpecialBackend b n
  =>  [String]
  -> TransformableDia b n
functionArgDia argumentNames (TransformParams name _level) 
  = named name finalDiagram where
    
    -- argumetPort = zipWith (makePassthroughPorts name) (zip argPortsConst resultPortsConst) argumentNames
  argumetPort = zipWith (makeLabelledPort name) resultPortsConst argumentNames
  combinedArgumetPort = hsep portSeparationSize argumetPort
  -- argumetsInBox = boxForDiagram combinedArgumetPort (lamArgResC colorScheme) (width combinedArgumetPort) (height combinedArgumetPort)
  finalDiagram = combinedArgumetPort

functionDefDia ::  SpecialBackend b n
  => String 
  -> TransformableDia b n
functionDefDia functionName (TransformParams name _level)
  = named name finalDiagram where
  lambdaSymbol = lambdaBodySymbol functionName
  inputDiagram = makeQualifiedPort' memptyWithPosition memptyWithPosition InputPortConst name
  outputDiagram = makeQualifiedPort' memptyWithPosition memptyWithPosition ResultPortConst name
  finalDiagram = vcat [inputDiagram, lambdaSymbol, outputDiagram]
-- Done to prevent this:
-- glance-test: circleDiameter too small: 0.0
-- CallStack (from HasCallStack):
-- error, called at app/Rendering.hs:315:18 in main:Rendering

lambdaRegionToDiagram :: SpecialBackend b Double 
  => [SpecialQDiagram b Double]
  -> NodeName
  -> SpecialQDiagram b Double
lambdaRegionToDiagram
    = lambdaRegionSymbol

-- | The ports of flatLambdaIcon are:
-- 0: Result icon
-- 1: The lambda function value
-- 2,3.. : The parameters
lambdaRegionSymbol :: SpecialBackend b Double
  => [SpecialQDiagram b Double]
  -> NodeName
  -> SpecialQDiagram b Double
lambdaRegionSymbol enclosedDiagarms name
  = moveTo (centerPoint enclosedBoundingBox) finalDiagram
  where
    enclosedBoundingBox = boundingBox $ mconcat enclosedDiagarms

    contentsRect = dashingG [0.7 * symbolSize, 0.3 * symbolSize] 0
                   $ rect
                   (lambdaRectPadding + width enclosedBoundingBox)
                   (lambdaRectPadding + height enclosedBoundingBox)
    coloredContentsRect = lc lightgreen (lwG defaultLineWidth contentsRect)

    finalDiagram = vcat [coloredContentsRect]

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
      arrowHead .~ noHead
      -- arrowHead .~ tri
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