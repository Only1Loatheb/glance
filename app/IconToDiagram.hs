{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IconToDiagram
  ( iconToDiagram
  , lambdaRegionToDiagram
  , nameDiagram
  , lambdaRegionPadding
  )
where

import Diagrams.Prelude hiding ((&), (#), Name)

import qualified Diagrams.TwoD.Path.Boolean as B

import Data.Either(partitionEithers)
import Data.List (transpose)

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

import DrawingColors(ColorStyle(..))
import Types(
  Icon(..)
  , DiagramIcon(..)
  , SpecialDiagram
  , SpecialBackend
  
  , NodeName(..)
  , Port(..)
  , ApplyFlavor(..)
  , NamedIcon
  , Labeled(..)
  , IconInfo
  , Named(..)
  , TransformParams(..)
  , TransformableDia
  , CaseFlavor(..)
  , Delimiters
  , ListLitFlavor(..)
  )

import DiagramSymbols(
  defaultLineWidth
  , symbolSize
  , portSeparationSize
  , lambdaRegionPadding
  , inputPortSymbol
  , resultPortSymbol
  , caseVarSymbol
  , inCaseDecisionFrame
  , inFrame
  , memptyWithPosition
  , inItemFrame
  , listCompPipe
  , listLitDelimiterDia
  )

-- name.
nameDiagram ::
  NodeName
  -> SpecialDiagram b
  -> SpecialDiagram b
nameDiagram name dia = named name (name .>> dia)

-- | Make an port with an integer name. Always use <> to add a ports
-- (not === or |||)  since mempty has no size and will not be placed where you
-- want it.
makePort :: SpecialBackend b => Port -> SpecialDiagram b
makePort x = named x mempty
--makePort x = circle 0.2 # fc green # named x
-- Note, the version of makePort below seems to have a different type.
--makePort x = textBox (show x) # fc green # named x

makeQualifiedPort :: SpecialBackend b 
  =>  Port -> NodeName -> SpecialDiagram b
makeQualifiedPort = makeQualifiedPort' inputPortSymbol resultPortSymbol

makeQualifiedPort' :: SpecialBackend b => 
                        SpecialDiagram b
                        -> SpecialDiagram b -> Port -> NodeName -> SpecialDiagram b
makeQualifiedPort' inputDia resultDia port name = portAndSymbol where
  namedPort = name .>> (makePort port)
  portAndSymbol = namedPort <> symbol
  symbol = if isArgPort port then inputDia else resultDia

-- Don't display " tempvar" from SimpSyntaxToSyntaxGraph.hs/matchesToCase
makeLabelledPort :: SpecialBackend b =>
  ColorStyle Double -> NodeName -> Port -> String -> SpecialDiagram b
makeLabelledPort colorStyle name port str
  = choosePortDiagram str portAndSymbol portSymbolAndLabel where
    portAndSymbol = makeQualifiedPort port name
    label = coloredTextBox (textBoxTextC colorStyle) str
    portSymbolAndLabel = if isArgPort port
      then portAndSymbol === label
      else label === portAndSymbol

choosePortDiagram :: SpecialBackend b =>
  String -> SpecialDiagram b -> SpecialDiagram b ->SpecialDiagram b
choosePortDiagram str portAndSymbol portSymbolAndLabel
  = centerX symbol where
    symbol
      | isTempLabel str = portAndSymbol
      | not (null str) = portSymbolAndLabel
      | otherwise = portAndSymbol
      

makeInputDiagram :: SpecialBackend b
  => IconInfo
  -> ColorStyle Double
  -> TransformParams n
  -> Labeled (Maybe NamedIcon)
  -> NodeName
  -> SpecialDiagram b
makeInputDiagram iconInfo colorStyle tp maybeFunText name = case laValue maybeFunText of
  Just _ ->
    makeAppInnerIcon iconInfo colorStyle tp True InputPortConst maybeFunText
  Nothing -> makeQualifiedPort InputPortConst name
      -- becaues it can only be [function name, lambda, imputPort]

makeResultDiagram :: SpecialBackend b
  => NodeName
  -> SpecialDiagram b
makeResultDiagram = makeQualifiedPort ResultPortConst

makeAppInnerIcon :: SpecialBackend b
  => IconInfo
  -> ColorStyle Double
  -> TransformParams n
  -> Bool  -- If False then add one to the nesting level. 
  -> Port  -- Port number (if the NamedIcon is Nothing)
  -> Labeled (Maybe NamedIcon) -- The icon 
  -> SpecialDiagram b
makeAppInnerIcon _iconInfo colorStyle (TransformParams name _) _isSameNestingLevel  port
  (Labeled Nothing str)
  = makeLabelledPort colorStyle name port str
makeAppInnerIcon iconInfo colorStyle (TransformParams _ nestingLevel ) isSameNestingLevel _port
  (Labeled (Just (Named iconNodeName icon)) _)
  = iconToDiagram
    iconInfo
    colorStyle
    icon
    (TransformParams iconNodeName innerLevel)
  where
    innerLevel = if isSameNestingLevel then nestingLevel else nestingLevel + 1

makePassthroughIcon :: SpecialBackend b
  => IconInfo
  -> ColorStyle Double
  -> TransformParams n
  -> Bool  -- If False then add one to the nesting level. 
  -> (Port,Port)  -- Port number (if the NamedIcon is Nothing)
  -> Labeled (Maybe NamedIcon) -- The icon 
  -> SpecialDiagram b
makePassthroughIcon iconInfo colorStyle tp@(TransformParams name _) isSameNestingLevel (inPort,outPort) labledMaybeNamedIcon 
  = inIcon === valuePort where
    inIcon = makeAppInnerIcon iconInfo colorStyle tp isSameNestingLevel inPort labledMaybeNamedIcon
    --  makeLabelledPort name port str
    valuePort = makeQualifiedPort outPort name

lambdaBodySymbol :: SpecialBackend b
  => String
  -> ColorStyle Double 
  -> SpecialDiagram b
lambdaBodySymbol label colorStyle = if isTempLabel label
  then mempty
  else coloredTextBox (textBoxTextC colorStyle) label
-- >>>>>>>>>>>>>>>>>>>>>> SUB Diagrams <<<<<<<<<<<<<<<<<<<<<<<<
-- TODO Detect if we are in a loop (have called iconToDiagram on the same node
-- before)

-- | Make an identity TransformableDia


iconToDiagram :: SpecialBackend b
  => IconInfo
  -> ColorStyle Double 
  -> Icon
  -> TransformableDia b n
iconToDiagram iconInfo colorStyle (Icon icon _) = case icon of
  TextBoxIcon s -> literalDiagram s colorStyle
  BindTextBoxIcon s -> bindDiagram s
  CaseResultIcon -> caseResultDiagram
  FunctionArgIcon argumentNames -> functionArgDia colorStyle argumentNames
  FunctionDefIcon funcName _ inputNode -> functionDefDia iconInfo colorStyle funcName (findMaybeIconFromName iconInfo inputNode)
  NestedApply flavor headIcon args
    -> nestedApplyDia iconInfo flavor colorStyle
       (findMaybeIconFromName iconInfo headIcon)
       (findMaybeIconsFromNames iconInfo args)
  NestedPatternApp constructor args rhsNodeName
    -> nestedPatternAppDia iconInfo colorStyle (repeat $ patternC colorStyle) constructor args (findMaybeIconFromName iconInfo rhsNodeName)
  NestedCaseIcon styleTag args -> nestedCaseDia iconInfo colorStyle
                         (findMaybeIconsFromNames iconInfo args)
                         styleTag
  ListCompIcon itemName gensNames qualsNames -> listCompDiagram iconInfo colorStyle
    (findMaybeIconFromName iconInfo itemName) 
    (findMaybeIconsFromNames iconInfo gensNames)
    (findMaybeIconsFromNames iconInfo qualsNames)
  ListLitIcon flavor args delimiters -> listLitDiagram iconInfo colorStyle flavor (findMaybeIconsFromNames iconInfo args) delimiters

caseResultDiagram :: SpecialBackend b => TransformableDia b n
caseResultDiagram transformParams = finalDia where
  name = tpName transformParams
  caseResultDia = nameDiagram name memptyWithPosition
  output = makeQualifiedPort ResultPortConst name
  finalDia = caseResultDia === output

bindDiagram :: SpecialBackend b =>
  String -> TransformableDia b n
bindDiagram _bindName transformParams = finalDia where
  name = tpName transformParams
  bindDia = nameDiagram name memptyWithPosition

  input = makeQualifiedPort InputPortConst name
  finalDia = input === bindDia

literalDiagram :: SpecialBackend b =>
  String  
  -> ColorStyle Double 
  -> TransformableDia b n
literalDiagram t colorStyle transformParams = finalDia where
  finalDia = bindText === outputDia
  outputDia = makeResultDiagram (tpName transformParams)
  bindText = coloredTextBox (textBoxTextC colorStyle) t

nestedPatternAppDia :: forall b n. SpecialBackend b
  => IconInfo
  -> ColorStyle Double 
  -> [Colour Double]
  -> Labeled (Maybe NamedIcon)
  -> [Labeled (Maybe NamedIcon)]
  -> Maybe NamedIcon
  -> TransformableDia b n
nestedPatternAppDia
  iconInfo
  colorStyle
  borderColors
  maybeConstructorName
  subIcons
  rhsNodeName
  tp@(TransformParams name nestingLevel)
  = centerY finalDia
  where
    borderColor = borderColors !! nestingLevel
    resultDia = makeResultDiagram name

    subscribedValueDia = alignT $ makeAppInnerIcon iconInfo colorStyle tp True PatternUnpackingPort (Labeled rhsNodeName patternSubscribedValueStr)

    constructorDiagram = alignB $ makeInputDiagram iconInfo colorStyle tp maybeConstructorName name

    patterns::[SpecialDiagram b]
    patterns = alignB $ zipWith (makeAppInnerIcon iconInfo colorStyle tp False) resultPortsConst subIcons
    patternDiagram = constructorDiagram ||| subscribedValueDia ||| hsep portSeparationSize patterns

    patternDiagramInBox = inFrame patternDiagram borderColor (width patternDiagram) (height patternDiagram)

    finalDia = patternDiagramInBox  === resultDia

-- | apply port locations:
-- InputPortConst: Function
-- ResultPortConst: Result
-- Ports 2,3..: Arguments
generalNestedDia :: SpecialBackend b
  => IconInfo
  -> ColorStyle Double
  -> [Colour Double]
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> TransformableDia b n
generalNestedDia
  iconInfo
  colorStyle
  borderColors
  maybeFunText
  args
  tp@(TransformParams name nestingLevel)
  -- beside Place two monoidal objects (i.e. diagrams, paths, animations...) next to each other along the given vector.
  = finalDia where
    borderColor = borderColors !! nestingLevel
    boxWidth =  max (width transformedName) (width argPorts)

    argPortsUncentred =  zipWith ( makeAppInnerIcon iconInfo colorStyle tp False) argPortsConst (fmap pure args)
    argPortsCentred  = fmap alignB argPortsUncentred
    argPorts = centerX $ hsep portSeparationSize argPortsCentred
    argsDiagram = inFrame argPorts borderColor boxWidth (height argPorts)

    resultDiagram =  makeResultDiagram name

    transformedName = makeInputDiagram iconInfo colorStyle tp (pure maybeFunText) name

    functionDiagramInBox = inFrame transformedName borderColor boxWidth (height transformedName)

    finalDia = vcat [ argsDiagram,functionDiagramInBox, resultDiagram]

listCompDiagram :: SpecialBackend b
  => IconInfo
  -> ColorStyle Double
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> [Maybe NamedIcon]
  -> TransformableDia b n
listCompDiagram
  iconInfo
  colorStyle
  maybeItem
  genIcons
  qualIcons
  tp@(TransformParams name _)
  = finalDia where 
    boxWidth =  width argPorts

    qualDiagrams = zipWith ( makeAppInnerIcon iconInfo colorStyle tp False) listCompQualPorts (fmap pure qualIcons)
    qualDiagram = hcat $ map (inCaseDecisionFrame colorStyle) qualDiagrams
    qualDiagramWithLine = (alignB $ listCompPipe colorStyle (letterHeight + height qualDiagram)) <> (alignBL qualDiagram)

    argPortsUncentred =  zipWith 
      (makePassthroughIcon iconInfo colorStyle tp False) 
      (zip argPortsConst resultPortsConst) 
      (fmap pure genIcons)

    argPortsCentred  = fmap alignB argPortsUncentred
    argPorts = centerX $ hsep portSeparationSize argPortsCentred
    argsDiagram = inFrame argPorts (listC colorStyle) boxWidth (height argPorts)

    resultDiagram =  makeResultDiagram name

    itemDiagram = makeInputDiagram iconInfo colorStyle tp (pure maybeItem) name

    listCompItemDiagram = inItemFrame colorStyle itemDiagram

    finalDia = vcat [argsDiagram, qualDiagramWithLine, listCompItemDiagram, resultDiagram]
    

listLitDiagram :: SpecialBackend b
  => IconInfo
  -> ColorStyle Double
  -> ListLitFlavor
  -> [Maybe NamedIcon]
  -> Delimiters
  -> TransformableDia b n
listLitDiagram iconInfo colorStyle flavor literals delimiters transformParams = finalDia where
  name = tpName transformParams
  literalDias = map alignB $ zipWith ( makeAppInnerIcon iconInfo colorStyle transformParams False) argPortsConst (fmap pure literals)
  delimitersDias = map (listLitDelimiterDia colorStyle flavor) delimiters
  litDiagram = concat . transpose $ [delimitersDias,literalDias]
  listDia = centerX $ hcat litDiagram
  resultDia = centerX $ makeResultDiagram name
  finalDia = listDia === resultDia

nestedApplyDia :: SpecialBackend b
  => IconInfo
  -> ApplyFlavor
  -> ColorStyle Double
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> TransformableDia b n
nestedApplyDia iconInfo flavor colorStyle = case flavor of
  ApplyFlavor -> generalNestedDia iconInfo colorStyle (nestingC colorStyle)
  ComposeFlavor -> generalNestedDia iconInfo colorStyle (applyCompositionC colorStyle)



nestedCaseDia :: SpecialBackend b
  => IconInfo
  -> ColorStyle Double
  -> [Maybe NamedIcon]
  -> CaseFlavor
  -> TransformableDia b n
nestedCaseDia iconInfo colorStyle
  = generalNestedCaseDia iconInfo colorStyle (inCaseDecisionFrame colorStyle)

-- | generalNestedCaseDia port layout:
-- 0 -> top
-- 1 -> bottom
-- odds -> left
-- evens -> right
generalNestedCaseDia ::forall b n. SpecialBackend b
  => IconInfo
  -> ColorStyle Double
  -> (SpecialDiagram b -> SpecialDiagram b)
  -> [Maybe NamedIcon]
  -> CaseFlavor
  -> TransformableDia b n
generalNestedCaseDia iconInfo colorStyle inConstBox inputAndArgs flavor
  tp@(TransformParams name _nestingLevel)
  = case inputAndArgs of
  [] -> error "empty multiif"-- mempty
  input : subicons -> finalDia where
    symbolColor = caseRhsC colorStyle

    finalDia = vcat [allCasesAtRight, resultDia]

    resultDia = makeResultDiagram name

    (iFConstIcons, iFVarIcons)
      = partitionEithers $ zipWith iconMapper mixedPorts subicons

    iconMapper port subicon
      | isArgPort port = Left $ inConstBox innerIcon{- middle -}
      | otherwise = Right ${- middle -} vcat [caseVarSymbol symbolColor, innerIcon]
      where
        innerIcon = makeAppInnerIcon iconInfo colorStyle tp isSameNestingLevel port (Labeled subicon "")
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

    inputLambdaLine = alignT inputLine <> inputDiagram
    inputLine = lwG defaultLineWidth $ lc symbolColor $ vrule (height allCases)
    inputDiagram
      | flavor == MultiIfFlavor = alignTR 
        $ coloredTextBox (textBoxTextC colorStyle) ifConditionConst
      | otherwise = centerX $ alignB $ makeInputDiagram iconInfo colorStyle tp (pure input) name

    allCasesAtRight = inputLambdaLine <> alignTL allCases

functionArgDia :: SpecialBackend b
  => ColorStyle Double
  -> [String]
  -> TransformableDia b n
functionArgDia colorStyle argumentNames (TransformParams name _level) 
  = named name finalDiagram where
    
    -- argumetPort = zipWith (makePassthroughPorts name) (zip argPortsConst resultPortsConst) argumentNames
  argumetPort = zipWith (makeLabelledPort colorStyle name) resultPortsConst argumentNames
  combinedArgumetPort = hsep portSeparationSize argumetPort
  -- argumetsInBox = inFrame combinedArgumetPort (lambdaC colorScheme) (width combinedArgumetPort) (height combinedArgumetPort)
  finalDiagram = combinedArgumetPort

functionDefDia :: SpecialBackend b
  => IconInfo
  -> ColorStyle Double
  -> String
  -> Maybe NamedIcon 
  -> TransformableDia b n
functionDefDia iconInfo colorStyle functionName input transformParams = finalDiagram where
  name = tpName transformParams
  lambdaSymbol = alignBR $ (lambdaBodySymbol functionName colorStyle ||| memptyWithPosition)
  inputDiagram = makeInputDiagram iconInfo colorStyle transformParams (pure input) name
  resultDiagram =  makeResultDiagram name
  finalDiagram = lambdaSymbol <> (inputDiagram === resultDiagram)

-- | The ports of flatLambdaIcon are:
-- 0: Result icon
-- 1: The lambda function value
-- 2,3.. : The parameters
lambdaRegionToDiagram :: SpecialBackend b
  => ColorStyle Double
  -> [SpecialDiagram b]
  -> NodeName
  -> Int
  -> SpecialDiagram b
lambdaRegionToDiagram colorStyle enclosedDiagarms (NodeName nameInt) level
  = regionSymbol
  where
    -- TODO Add lambda ranks/levels
    paddingSize =  lambdaRegionPadding + 2 * defaultLineWidth * (fromIntegral level)
    paddedDiagrams = fmap (frame paddingSize) enclosedDiagarms
    diagramBoxes = map  boundingRect paddedDiagrams
    boxesPath = mconcat diagramBoxes
    contentsRect = strokePath $ B.union Winding boxesPath

    edgeColors = edgeListC colorStyle
    namePortHash = mod nameInt (length edgeColors)
    regionLineColor = edgeColors !! namePortHash

    regionSymbol = dashingG [0.3 * symbolSize, 0.7 * symbolSize] 0 
      $ lc regionLineColor (lwG defaultLineWidth contentsRect)