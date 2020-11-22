{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IconToDiagram
  ( ColorStyle(..)
  , colorScheme
  , iconToDiagram
  , lambdaRegionToDiagram
  , nameDiagram
  , lambdaRegionPadding
  )
where

import Diagrams.Prelude hiding ((&), (#), Name)

import qualified Diagrams.TwoD.Path.Boolean as B

import Data.Either(partitionEithers)
import Data.Maybe(isNothing)

import Icons(
  findMaybeIconFromName
  , findMaybeIconsFromNames
  )
import TextBox (
  coloredTextBox
  , letterHeight
  )

import PortConstants(
  pattern InputPortConst
  , pattern ResultPortConst
  , argPortsConst
  , resultPortsConst
  , isArgPort
  , mixedPorts
  , pattern PatternUnpackingPort
  , listCompQualPorts
  )

import StringSymbols(
  ifConditionConst
  , isTempLabel
  , patternSubscribedValueStr
  )

import DrawingColors(colorScheme, ColorStyle(..))
import Types(
  Icon(..)
  , DiagramIcon(..)
  , SpecialDiagram
  , SpecialBackend
  , SpecialNum
  , NodeName(..)
  , Port(..)
  , LikeApplyFlavor(..)
  , NamedIcon
  , Labeled(..)
  , IconInfo
  , Named(..)
  , NameAndPort(..)
  , TransformParams(..)
  , TransformableDia
  , CaseOrMultiIfTag(..)
  , SpecialQDiagram 
  , Edge(..)
  , EdgeOption(..)
  , Connection
  , Delimiters
  )

import DiagramSymbols(
  defaultLineWidth
  , symbolSize
  , boxPadding
  , portSeparationSize
  , lambdaRegionPadding
  , defaultOpacity
  , inputPortSymbol
  , resultPortSymbol
  , caseVarSymbol
  , inNoteFrame
  , inCaseDecisionFrame
  , inDecisionFrame
  , inFrame
  , memptyWithPosition
  , inItemFrame
  , listDots
  , listCompPipe
  , listLitDelimiterDia
  )
import Data.List (transpose)

{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Unnecessary hiding" #-}


-- name.
nameDiagram :: SpecialNum n =>
  NodeName
  -> SpecialDiagram b n
  -> SpecialDiagram b n
nameDiagram name dia = named name (name .>> dia)

-- | Make an port with an integer name. Always use <> to add a ports
-- (not === or |||)  since mempty has no size and will not be placed where you
-- want it.
makePort ::  SpecialBackend b n => Port -> SpecialDiagram b n
makePort x = named x mempty
--makePort x = circle 0.2 # fc green # named x
-- Note, the version of makePort below seems to have a different type.
--makePort x = textBox (show x) # fc green # named x

makeQualifiedPort :: SpecialBackend b n 
  =>  Port -> NodeName -> SpecialDiagram b n
makeQualifiedPort = makeQualifiedPort' inputPortSymbol resultPortSymbol

makeQualifiedPort' :: SpecialBackend b n => 
                        SpecialDiagram b n
                        -> SpecialDiagram b n -> Port -> NodeName -> SpecialDiagram b n
makeQualifiedPort' inputDia resultDia port name = portAndSymbol where
  namedPort = name .>> (makePort port)
  portAndSymbol = namedPort <> symbol
  symbol = if isArgPort port then inputDia else resultDia

-- Don't display " tempvar" from SimpSyntaxToSyntaxGraph.hs/matchesToCase
makeLabelledPort :: SpecialBackend b n =>
  NodeName -> Port -> String ->  SpecialDiagram b n
makeLabelledPort name port str
  = choosePortDiagram str portAndSymbol portSymbolAndLabel where
    portAndSymbol = makeQualifiedPort port name
    label = coloredTextBox (textBoxTextC colorScheme) str
    portSymbolAndLabel = if isArgPort port
      then portAndSymbol === label
      else label === portAndSymbol

choosePortDiagram :: SpecialBackend b n =>
  String -> SpecialDiagram b n -> SpecialDiagram b n ->SpecialDiagram b n
choosePortDiagram str portAndSymbol portSymbolAndLabel
  = centerX symbol where
    symbol
      | isTempLabel str = portAndSymbol
      | not (null str) = portSymbolAndLabel
      | otherwise = portAndSymbol
      

makeInputDiagram :: SpecialBackend b n
  => IconInfo
  -> TransformParams n
  -> Labeled (Maybe NamedIcon)
  -> NodeName
  -> SpecialDiagram b n
makeInputDiagram iconInfo tp maybeFunText name = case laValue maybeFunText of
  Just _ ->
    makeAppInnerIcon iconInfo tp True InputPortConst maybeFunText
  Nothing -> makeQualifiedPort InputPortConst name
      -- becaues it can only be [function name, lambda, imputPort]

makeResultDiagram :: SpecialBackend b n
  => NodeName
  -> SpecialDiagram b n
makeResultDiagram = makeQualifiedPort ResultPortConst

makeAppInnerIcon :: SpecialBackend b n
  => IconInfo
  -> TransformParams n
  -> Bool  -- If False then add one to the nesting level. 
  -> Port  -- Port number (if the NamedIcon is Nothing)
  -> Labeled (Maybe NamedIcon) -- The icon 
  -> SpecialDiagram b n
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

makePassthroughIcon :: SpecialBackend b n
  => IconInfo
  -> TransformParams n
  -> Bool  -- If False then add one to the nesting level. 
  -> (Port,Port)  -- Port number (if the NamedIcon is Nothing)
  -> Labeled (Maybe NamedIcon) -- The icon 
  -> SpecialDiagram b n
makePassthroughIcon iconInfo tp@(TransformParams name _) isSameNestingLevel (inPort,outPort) labledMaybeNamedIcon 
  = inIcon === valuePort where
    inIcon = makeAppInnerIcon iconInfo tp isSameNestingLevel inPort labledMaybeNamedIcon
    --  makeLabelledPort name port str
    valuePort = makeQualifiedPort outPort name

lambdaBodySymbol :: SpecialBackend b n
  => String
  -> SpecialDiagram b n
lambdaBodySymbol label = if isTempLabel label
  then mempty
  else coloredTextBox (textBoxTextC colorScheme) label
-- >>>>>>>>>>>>>>>>>>>>>> SUB Diagrams <<<<<<<<<<<<<<<<<<<<<<<<
-- TODO Detect if we are in a loop (have called iconToDiagram on the same node
-- before)

-- | Make an identity TransformableDia


iconToDiagram :: SpecialBackend b n
  => IconInfo
  -> Icon
  -> TransformableDia b n
iconToDiagram iconInfo (Icon icon _) = case icon of
  TextBoxIcon s -> literalDiagram s
  BindTextBoxIcon s -> bindDiagram s
  CaseResultIcon -> caseResultDiagram
  FunctionArgIcon argumentNames -> functionArgDia argumentNames
  FunctionDefIcon funcName _ inputNode -> functionDefDia iconInfo funcName (findMaybeIconFromName iconInfo inputNode)
  NestedApply flavor headIcon args
    -> nestedApplyDia
       iconInfo
       flavor
       (findMaybeIconFromName iconInfo headIcon)
       (findMaybeIconsFromNames iconInfo args)
  NestedPatternApp constructor args rhsNodeName
    -> nestedPatternAppDia iconInfo (repeat $ patternC colorScheme) constructor args (findMaybeIconFromName iconInfo rhsNodeName)
  NestedCaseIcon styleTag args -> nestedCaseDia
                         iconInfo
                         (findMaybeIconsFromNames iconInfo args)
                         styleTag
  ListCompIcon itemName gensNames qualsNames -> listCompDiagram iconInfo 
    (findMaybeIconFromName iconInfo itemName) 
    (findMaybeIconsFromNames iconInfo gensNames)
    (findMaybeIconsFromNames iconInfo qualsNames)
  ListLitIcon args delimiters -> listLitDiagram iconInfo (findMaybeIconsFromNames iconInfo args) delimiters

caseResultDiagram :: SpecialBackend b n => TransformableDia b n
caseResultDiagram transformParams = finalDia where
  name = tpName transformParams
  caseResultDia = nameDiagram name memptyWithPosition
  output = makeQualifiedPort ResultPortConst name
  finalDia = caseResultDia === output

bindDiagram :: SpecialBackend b n =>
  String -> TransformableDia b n
bindDiagram _bindName transformParams = finalDia where
  name = tpName transformParams
  bindDia = nameDiagram name memptyWithPosition

  input = makeQualifiedPort InputPortConst name
  finalDia = input === bindDia

literalDiagram :: SpecialBackend b n =>
  String -> TransformableDia b n
literalDiagram t transformParams = finalDia where
  finalDia = bindText === outputDia
  outputDia = makeResultDiagram (tpName transformParams)
  bindText = coloredTextBox (textBoxTextC colorScheme) t

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
  = centerY finalDia
  where
    borderColor = borderColors !! nestingLevel
    resultDia = makeResultDiagram name

    subscribedValueDia = alignT $ makeAppInnerIcon iconInfo tp True PatternUnpackingPort (Labeled rhsNodeName patternSubscribedValueStr)

    constructorDiagram = alignB $ makeInputDiagram iconInfo tp maybeConstructorName name

    patterns::[SpecialDiagram b n]
    patterns = alignB $ zipWith (makeAppInnerIcon iconInfo tp False) resultPortsConst subIcons
    patternDiagram = constructorDiagram ||| subscribedValueDia ||| hsep portSeparationSize patterns

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
  = finalDia where
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
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> [Maybe NamedIcon]
  -> TransformableDia b n
listCompDiagram
  iconInfo
  maybeItem
  genIcons
  qualIcons
  tp@(TransformParams name _)
  = finalDia where 
    boxWidth =  width argPorts

    qualDiagrams = zipWith ( makeAppInnerIcon iconInfo tp False) listCompQualPorts (fmap pure qualIcons)
    qualDiagram = hcat $ map inCaseDecisionFrame qualDiagrams
    qualDiagramWithLine = (alignB $ listCompPipe (letterHeight + height qualDiagram)) <> (alignBL qualDiagram)

    argPortsUncentred =  zipWith 
      (makePassthroughIcon iconInfo tp False) 
      (zip argPortsConst resultPortsConst) 
      (fmap pure genIcons)

    argPortsCentred  = fmap alignB argPortsUncentred
    argPorts = centerX $ hsep portSeparationSize argPortsCentred
    argsDiagram = inFrame argPorts (listC colorScheme) boxWidth (height argPorts)

    resultDiagram =  makeResultDiagram name

    itemDiagram = makeInputDiagram iconInfo tp (pure maybeItem) name

    listCompItemDiagram = inItemFrame itemDiagram

    finalDia = vcat [argsDiagram, qualDiagramWithLine, listCompItemDiagram, resultDiagram]
    

listLitDiagram :: SpecialBackend b n
  => IconInfo
  -> [Maybe NamedIcon]
  -> Delimiters
  -> TransformableDia b n
listLitDiagram iconInfo literals delimiters transformParams = finalDia where
  name = tpName transformParams
  literalDias = alignB $ zipWith ( makeAppInnerIcon iconInfo transformParams False) argPortsConst (fmap pure literals)
  delimitersDias = map listLitDelimiterDia delimiters
  litDiagram = concat . transpose $ [delimitersDias,literalDias]
  listDia = centerX $ hcat litDiagram
  resultDia = centerX $ makeResultDiagram name
  finalDia = listDia === resultDia

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
    -> generalNestedDia iconInfo (applyCompositionC colorScheme)



nestedCaseDia :: SpecialBackend b n
  => IconInfo
  -> [Maybe NamedIcon]
  -> CaseOrMultiIfTag
  -> TransformableDia b n
nestedCaseDia iconInfo
  = generalNestedCaseDia iconInfo (caseRhsC colorScheme) inCaseDecisionFrame

-- | generalNestedCaseDia port layout:
-- 0 -> top
-- 1 -> bottom
-- odds -> left
-- evens -> right
generalNestedCaseDia ::forall b n. SpecialBackend b n
                   => IconInfo
                   -> Colour Double
                   -> (SpecialDiagram b n -> SpecialDiagram b n)
                   -> [Maybe NamedIcon]
                   -> CaseOrMultiIfTag
                   -> TransformableDia b n
generalNestedCaseDia iconInfo symbolColor inConstBox inputAndArgs flavor
  tp@(TransformParams name _nestingLevel)
  = case inputAndArgs of
  [] -> error "empty multiif"-- mempty
  input : subicons -> finalDia where
    finalDia = vcat [inputDiagram, allCasesAtRight, resultDia]

    resultDia = makeResultDiagram name

    inputDiagram
      | flavor == MultiIfTag = alignBR 
        $ coloredTextBox (textBoxTextC colorScheme) ifConditionConst
      | otherwise = makeInputDiagram iconInfo tp (pure input) name

    (iFConstIcons, iFVarIcons)
      = partitionEithers $ zipWith iconMapper mixedPorts subicons

    iconMapper port subicon
      | isArgPort port = Left $ inConstBox innerIcon{- middle -}
      | otherwise = Right ${- middle -} vcat [caseVarSymbol symbolColor, innerIcon]
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
  -- argumetsInBox = inFrame combinedArgumetPort (lambdaC colorScheme) (width combinedArgumetPort) (height combinedArgumetPort)
  finalDiagram = combinedArgumetPort

functionDefDia ::  SpecialBackend b n
  => IconInfo
  -> String
  -> Maybe NamedIcon 
  -> TransformableDia b n
functionDefDia iconInfo functionName input transformParams = finalDiagram where
  name = tpName transformParams
  lambdaSymbol = alignBR $ (lambdaBodySymbol functionName ||| memptyWithPosition)
  inputDiagram = makeInputDiagram iconInfo transformParams (pure input) name
  resultDiagram =  makeResultDiagram name
  finalDiagram = lambdaSymbol <> (inputDiagram === resultDiagram)

lambdaRegionToDiagram :: SpecialBackend b Double 
  => [SpecialDiagram b Double]
  -> NodeName
  -> Int
  -> SpecialDiagram b Double
lambdaRegionToDiagram
    = lambdaRegionSymbol

-- | The ports of flatLambdaIcon are:
-- 0: Result icon
-- 1: The lambda function value
-- 2,3.. : The parameters
lambdaRegionSymbol :: SpecialBackend b Double
  => [SpecialDiagram b Double]
  -> NodeName
  -> Int
  -> SpecialDiagram b Double
lambdaRegionSymbol enclosedDiagarms (NodeName nameInt) level
  = regionSymbol
  where
    -- TODO Add lambda ranks/levels
    paddingSize =  lambdaRegionPadding + 2 * defaultLineWidth * (fromIntegral level)
    paddedDiagrams = fmap (frame paddingSize) enclosedDiagarms
    diagramBoxes = map  boundingRect paddedDiagrams
    boxesPath = mconcat diagramBoxes
    contentsRect = strokePath $ B.union Winding boxesPath

    edgeColors = edgeListC colorScheme
    namePortHash = mod nameInt (length edgeColors)
    regionLineColor = edgeColors !! namePortHash

    regionSymbol = dashingG [0.3 * symbolSize, 0.7 * symbolSize] 0 
      $ lc regionLineColor (lwG defaultLineWidth contentsRect)