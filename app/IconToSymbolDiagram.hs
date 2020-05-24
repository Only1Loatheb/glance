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
import qualified Diagrams.TwoD.Path.Boolean as B

import Data.Maybe(fromMaybe)
import Data.Either(partitionEithers)
import Data.Typeable(Typeable)

import Icons(
  findMaybeIconFromName
  , findMaybeIconsFromNames
  )
import TextBox (
  coloredTextBox
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
  , isTempLabel
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
defaultLineWidth :: (Fractional a) => a
defaultLineWidth = 0.15

symbolSize :: (Fractional a) => a
symbolSize = 0.5

boxPadding :: Fractional a => a
boxPadding = 2 * defaultLineWidth

portSeparationSize :: (Fractional a) => a
portSeparationSize = 0.3

lambdaRegionPadding :: (Fractional a) => a
lambdaRegionPadding = 2 * letterHeight + defaultLineWidth


patternsUpTranslation :: Num a => V2 a
patternsUpTranslation = V2 0 0

defaultOpacity :: (Fractional a) => a
defaultOpacity = 0.4

defaultShadowOpacity :: (Fractional a) => a
defaultShadowOpacity = 0.6

arrowLineWidth :: Fractional a => a
arrowLineWidth = defaultLineWidth

arrowShadowWidth :: Fractional a => a
arrowShadowWidth = 1.9 * arrowLineWidth

-- COLORS --
lineColorValue :: Colour Double
lineColorValue = lineC colorScheme

-- BEGIN diagram basic symbols --
inputPortSymbol :: SpecialBackend b n => SpecialQDiagram b n
inputPortSymbol = memptyWithPosition 

resultPortSymbol :: SpecialBackend b n
  => SpecialQDiagram b n
resultPortSymbol = memptyWithPosition -- valueSymbol
  
inputSymbol :: SpecialBackend b n => SpecialQDiagram b n
inputSymbol = lw none $ fc lineColorValue $ circle (symbolSize * 0.5)

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
  
    boxHeight = height diagram
    boxWidth = width diagram
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
lambdaBodySymbol label = if isTempLabel label
  then mempty
  else coloredTextBox (textBoxTextC colorScheme) label




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
    boxHeight = boxPadding + max (height diagram) letterHeight
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

inFrame :: SpecialBackend b n
  => SpecialQDiagram b n
  -> Colour Double
  -> n
  -> n
  -> SpecialQDiagram b n
inFrame diagram borderColor diagramWidth diagramHeight
  = centerXY diagram <> coloredArgBox where
    rectWidth = boxPadding + max diagramWidth letterHeight
    rectHeight = boxPadding + max diagramHeight letterHeight
    argBox = rect rectWidth rectHeight
    coloredArgBox = lwG defaultLineWidth $ lcA (withOpacity borderColor defaultOpacity) argBox

-- BEGIN Diagram helper functions --
memptyWithPosition :: SpecialBackend b n => SpecialQDiagram b n
memptyWithPosition = strutR2 (V2 symbolSize 0)
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
makeQualifiedPort' inputDia resultDia port name = portAndSymbol where
  namedPort = name .>> (makePort port)
  portAndSymbol = namedPort <> symbol
  symbol = if isInputPort port then inputDia else resultDia

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
      | isTempLabel str = portAndSymbol
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
  FunctionDefIcon funcName _ _-> functionDefDia funcName 
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
  -- bindText = coloredTextBox (bindTextBoxTextC colorScheme) t
  finalDia = bindDia -- === bindText

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

    patterns::[SpecialQDiagram b n]
    patterns = alignB $ zipWith (makeAppInnerIcon iconInfo tp False) resultPortsConst subIcons
    patternDiagram = hsep portSeparationSize $  constructorDiagram : subscribedValueDia : patterns

    patternDiagramInBox = inFrame patternDiagram borderColor (width patternDiagram) (height patternDiagram)

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
      argsDiagram = inFrame argPorts borderColor boxWidth (height argPorts)

      resultDiagram =  makeResultDiagram name

      transformedName = makeInputDiagram iconInfo tp (pure maybeFunText) name

      functionDiagramInBox = inFrame transformedName borderColor boxWidth (height transformedName)

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
      argsDiagram = inFrame argPorts borderColor boxWidth (height argPorts)

      resultDiagram =  makeResultDiagram name

      transformedName = makeInputDiagram iconInfo tp (pure maybeFunText) name

      functionDiagramInBox = inFrame transformedName borderColor boxWidth (height transformedName)

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
  -- argumetsInBox = inFrame combinedArgumetPort (lamArgResC colorScheme) (width combinedArgumetPort) (height combinedArgumetPort)
  finalDiagram = combinedArgumetPort

functionDefDia ::  SpecialBackend b n
  => String 
  -> TransformableDia b n
functionDefDia functionName (TransformParams name _level)
  = named name finalDiagram where
  lambdaSymbol = lambdaBodySymbol functionName
  inputDiagram = makeQualifiedPort' memptyWithPosition memptyWithPosition InputPortConst name
  outputDiagram = makeQualifiedPort' memptyWithPosition memptyWithPosition ResultPortConst name
  finalDiagram = lambdaSymbol ||| (inputDiagram === outputDiagram)
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
lambdaRegionSymbol enclosedDiagarms (NodeName nameInt)
  = regionSymbol
  where
    -- TODO Add lambda ranks/levels
    paddingSize =  lambdaRegionPadding + (lambdaRegionPadding/30.0) * (fromIntegral $ length enclosedDiagarms)
    paddedDiagrams = fmap (frame paddingSize) enclosedDiagarms
    diagramBoxes = map  boundingRect paddedDiagrams
    boxesPath = mconcat diagramBoxes
    contentsRect = strokePath $ B.union Winding boxesPath

    edgeColors = edgeListC colorScheme
    namePortHash = mod nameInt (length edgeColors)
    lineColor = edgeColors !! namePortHash

    regionSymbol = dashingG [0.3 * symbolSize, 0.7 * symbolSize] 0 
      $ lc lineColor (lwG defaultLineWidth contentsRect)

getArrowShadowOpts :: (RealFloat n, Typeable n)
  => (Maybe (Point V2 n),Maybe (Point V2 n))
  -> (Maybe (Angle n), Maybe (Angle n))
  -> Icon
  -> ArrowOpts n
getArrowShadowOpts maybePoints maybeAngles iconTo=
  shaftStyle %~ (lwG arrowShadowWidth .
                lcA $ withOpacity shaftColor defaultShadowOpacity)
  $ headStyle %~ fc shaftColor
  $ getArrowOpts maybePoints maybeAngles iconTo where
    shaftColor = backgroundC colorScheme

getArrowBaseOpts :: (RealFloat n, Typeable n)
  => NameAndPort
  -> (Maybe (Point V2 n),Maybe (Point V2 n))
  -> (Maybe (Angle n), Maybe (Angle n))
  -> (Icon, Icon)
  -> ArrowOpts n
getArrowBaseOpts (NameAndPort (NodeName nodeNum) mPort) maybePoints maybeAngles 
  icons@(_, iconTo)
  = shaftStyle %~ (lwG arrowLineWidth . lc shaftColor)
  $ headStyle %~ fc shaftColor
  $ getArrowOpts maybePoints maybeAngles iconTo where
    Port portNum = fromMaybe (Port 0) mPort
    shaftColor = getShaftColor nodeNum portNum icons

getShaftColor = getShaftColor' edgeColors where
  edgeColors = edgeListC colorScheme

getShaftColor' _ _ _ (FunctionDefIcon {}, _) = regionPerimC colorScheme
getShaftColor' _ _ _ (_, FunctionDefIcon {}) = regionPerimC colorScheme
getShaftColor' edgeColors nodeNum portNum _ = shaftColor where
  namePortHash = mod (portNum + (503 * nodeNum)) (length edgeColors)
  shaftColor = edgeColors !! namePortHash

getArrowOpts :: (RealFloat n, Typeable n)
  => (Maybe (Point V2 n),Maybe (Point V2 n))
  -> (Maybe (Angle n), Maybe (Angle n))
  -> Icon
  -> ArrowOpts n
getArrowOpts (formMaybePoint, toMaybePoint) (anglesFrom,anglesTo) iconTo
  = arrowOptions where
    formPoint = fromMaybe (p2 (0.0,0.0)) formMaybePoint
    toPoint = fromMaybe (p2  (0.0,-1.0)) toMaybePoint
    arrowOptions =
      -- arrowHead .~ noHead
      arrowHead .~ getArrowHead iconTo
      $ arrowTail .~ noTail
      $ arrowShaft .~ edgeSymbol formPoint toPoint anglesFrom anglesTo
      -- TODO Don't use a magic number for lengths (headLength and tailLength)
      $ lengths .~ global 0.5
      $ with

-- getArrowHead :: Icon -> 
getArrowHead :: RealFloat n => Icon -> ArrowHT n
getArrowHead FunctionDefIcon {} = noHead
getArrowHead _ = tri
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
  scaleFactor = symbolSize * 8.0
  offsetToEnd = toPoint .-. formPoint
  offsetToControl1 = rotate angleFrom (scale scaleFactor unitX)
  offsetToControl2 = rotate angleTo (scale scaleFactor unitX) ^+^ offsetToEnd