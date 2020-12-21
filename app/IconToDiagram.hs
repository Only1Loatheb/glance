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
  )
where

import Diagrams.Prelude hiding ((&), (#), Name)

import qualified Diagrams.TwoD.Path.Boolean as B

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
  pattern InputPort
  , pattern ResultPort
  , argPortList
  , valuePortList
  , isArgPort
  , casePortPairList
  , pattern PatternUnpackingPort
  , pattern FunDefValuePort
  , listCompQualPortList
  )

import StringSymbols(
  ifConditionConst
  , isTempLabel
  , patternSubscribedValueStr
  )

import Types(NumericType, 
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
  , DrawingInfo(..)
  , TransformableDia
  , CaseFlavor(..)
  , Delimiters
  , ListLitFlavor(..)
  , ColorStyle
  , ColorStyle'(..)
  , InCaseOrInApply(..)
  , laValue
  , setDashing
  )

import DiagramSymbols(
  defaultLineWidth
  , portSeparationSize
  , lambdaRegionPaddingX
  , lambdaRegionPaddingY
  , inputPortSymbol
  , resultPortSymbol
  , caseValSymbol
  , inCaseDecisionFrame
  , inFrame
  , memptyWithPosition
  , inItemFrame
  , listCompPipe
  , listLitDelimiterDia
  )
import Data.Maybe (isJust)

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
  InCaseOrInApply -> ColorStyle -> NodeName -> Port -> String -> SpecialDiagram b
makeLabelledPort inType colorStyle name port str
  = choosePortDiagram str portAndSymbol portSymbolAndLabel where
    portAndSymbol = makeQualifiedPort port name
    label = coloredTextBox (textBoxTextC colorStyle) str
    portSymbolAndLabel = case inType of 
      InCase -> portAndSymbol === label
      _ -> if isArgPort port then portAndSymbol === label else label === portAndSymbol

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
  -> DrawingInfo
  -> Labeled (Maybe NamedIcon)
  -> NodeName
  -> SpecialDiagram b
makeInputDiagram = makeInputDiagram' InputPort

makeInputDiagram' :: SpecialBackend b
  => Port
  -> IconInfo
  -> DrawingInfo
  -> Labeled (Maybe NamedIcon)
  -> NodeName
  -> SpecialDiagram b
makeInputDiagram' port iconInfo di maybeFunText name = if isJust $ maybeFunText ^. laValue
  then makeAppInnerIcon iconInfo di None port maybeFunText
  else makeQualifiedPort port name
      -- becaues it can only be [function name, lambda, imputPort]

makeResultDiagram :: SpecialBackend b
  => NodeName
  -> SpecialDiagram b
makeResultDiagram = makeQualifiedPort ResultPort

makeAppInnerIcon :: SpecialBackend b
  => IconInfo
  -> DrawingInfo
  -> InCaseOrInApply  -- parrent 
  -> Port  -- Port number (if the NamedIcon is Nothing)
  -> Labeled (Maybe NamedIcon) -- The icon 
  -> SpecialDiagram b
makeAppInnerIcon _iconInfo di inType  port
  (Labeled str Nothing)
  = makeLabelledPort inType colorStyle name port str where
    name = diName di
    colorStyle = diColorStyle di
makeAppInnerIcon iconInfo di inType _port
  (Labeled _ (Just (Named iconNodeName icon)))
  = iconToDiagram
    iconInfo
    icon
    (DrawingInfo iconNodeName innerLevel inType colorStyle)
  where
    colorStyle = diColorStyle di
    nestingLevel = diNestingLevel di
    innerLevel = case inType of 
      InApply -> nestingLevel + 1
      _ -> nestingLevel

makePassthroughIcon :: SpecialBackend b
  => IconInfo
  -> DrawingInfo
  -> InCaseOrInApply  -- If False then add one to the nesting level. 
  -> (Port,Port)  -- Port number (if the NamedIcon is Nothing)
  -> Labeled (Maybe NamedIcon) -- The icon 
  -> SpecialDiagram b
makePassthroughIcon iconInfo di inType (inPort,outPort) labledMaybeNamedIcon 
  = inIcon === valuePort where
    inIcon = makeAppInnerIcon iconInfo di inType inPort labledMaybeNamedIcon
    valuePort = makeQualifiedPort outPort name
    name = diName di

lambdaBodySymbol :: SpecialBackend b
  => String
  -> ColorStyle 
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
  -> Icon
  -> TransformableDia b
iconToDiagram iconInfo (Icon icon _) = case icon of
  TextBoxIcon s -> literalDiagram iconInfo s
  FunctionArgIcon argumentNames _ _ -> functionArgDia argumentNames
  FunctionDefIcon funcName inputNode -> functionDefDia iconInfo funcName (findMaybeIconFromName iconInfo inputNode)
  BindTextBoxIcon s -> bindDiagram s
  NestedApply flavor headIcon args -> applyDiagram
    iconInfo flavor
    (findMaybeIconFromName iconInfo headIcon)
    (findMaybeIconsFromNames iconInfo args)
  NestedPatternApp constructor args rhsNodeName -> patternDiagram
    iconInfo patternC constructor 
    (over (mapped . laValue) (findMaybeIconFromName iconInfo) args)
    (findMaybeIconFromName iconInfo rhsNodeName)
  NestedCaseIcon styleTag arg condsAndVals -> caseDiagram iconInfo
    (findMaybeIconFromName iconInfo arg)
    (map (bimap (findMaybeIconFromName iconInfo) (findMaybeIconFromName iconInfo)) condsAndVals)
    styleTag
  CaseResultIcon -> caseResultDiagram
  ListCompIcon itemName gensNames qualsNames -> listCompDiagram iconInfo
    (findMaybeIconFromName iconInfo itemName) 
    (findMaybeIconsFromNames iconInfo gensNames)
    (findMaybeIconsFromNames iconInfo qualsNames)
  ListLitIcon flavor args delimiters -> listLitDiagram iconInfo flavor (findMaybeIconsFromNames iconInfo args) delimiters

caseResultDiagram :: SpecialBackend b => TransformableDia b
caseResultDiagram drawingInfo = finalDia where
  name = diName drawingInfo
  caseResultDia = nameDiagram name memptyWithPosition
  output = makeQualifiedPort ResultPort name
  finalDia = caseResultDia === output

bindDiagram :: SpecialBackend b =>
  String -> TransformableDia b
bindDiagram _bindName drawingInfo = finalDia where
  name = diName drawingInfo
  bindDia = nameDiagram name memptyWithPosition

  input = makeQualifiedPort InputPort name
  finalDia = input === bindDia

literalDiagram :: SpecialBackend b =>
  IconInfo
  -> String  
  -> TransformableDia b
literalDiagram iconInfo t drawingInfo = finalDia where
  name = diName drawingInfo
  asPatternPort = makeAppInnerIcon iconInfo drawingInfo None PatternUnpackingPort (pure Nothing)
  finalDia = vcat [asPatternPort, bindText, outputDia]
  colorStyle = diColorStyle drawingInfo 
  outputDia = makeResultDiagram name
  bindText = coloredTextBox (textBoxTextC colorStyle) t

patternDiagram :: forall b. SpecialBackend b
  => IconInfo
  -> (ColorStyle -> Colour NumericType)
  -> String
  -> [Labeled (Maybe NamedIcon)]
  -> Maybe NamedIcon
  -> TransformableDia b
patternDiagram
  iconInfo
  getBorderColor
  constructorName
  subIcons
  rhsNodeName
  di
  = centerY finalDia where
    colorStyle = diColorStyle di
    borderColor = getBorderColor colorStyle
    name = diName di
    inType = diIsIn di
    resultDia = makeResultDiagram name

    subscribedValueDia = alignT $ makeAppInnerIcon iconInfo di None PatternUnpackingPort (Labeled patternSubscribedValueStr rhsNodeName)

    patterns::[SpecialDiagram b]
    patterns = map alignB $ zipWith (makeAppInnerIcon iconInfo di inType) valuePortList subIcons
    patternDia = constructor ||| subscribedValueDia ||| hsep portSeparationSize patterns

    constructor = alignB $ coloredTextBox (textBoxTextC colorStyle) constructorName

    patternDiagramInBox = inFrame patternDia borderColor (width patternDia) (height patternDia)

    finalDia = patternDiagramInBox  === resultDia

applyDiagram :: SpecialBackend b
  => IconInfo
  -> ApplyFlavor
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> TransformableDia b
applyDiagram iconInfo flavor icon icons di =
  let colorStyle = diColorStyle di
  in case flavor of
    ApplyFlavor -> generalApplyDia iconInfo flavor (nestingC colorStyle) icon icons di
    ComposeFlavor -> generalApplyDia iconInfo flavor (applyCompositionC colorStyle) icon icons di

-- | apply port locations:
-- InputPort: Function
-- ResultPort: Result
-- Ports 2,3..: Arguments
generalApplyDia :: SpecialBackend b
  => IconInfo
  -> ApplyFlavor
  -> [Colour Double]
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> TransformableDia b
generalApplyDia
  iconInfo
  flavor
  borderColors
  maybeFunText
  args
  di
  -- beside Place two monoidal objects (i.e. diagrams, paths, animations...) next to each other along the given vector.
  = finalDia where

    name = diName di 
    nestingLevel = diNestingLevel di

    borderColor = borderColors !! nestingLevel
    boxWidth =  max (width transformedName) (width argPorts)

    argPortsUncentred =  zipWith ( makeAppInnerIcon iconInfo di InApply) argPortList (fmap pure args)
    argPortsCentred  = fmap alignB argPortsUncentred
    argPorts = centerX $ hsep portSeparationSize argPortsCentred
    argsDiagram = inFrame argPorts borderColor boxWidth (height argPorts)

    resultDiagram =  makeResultDiagram name

    transformedName = makeInputDiagram iconInfo di (pure maybeFunText) name

    functionDiagramInBox = inFrame transformedName borderColor boxWidth (height transformedName)

    argsAndFunctionDiagram = case flavor of 
      ApplyFlavor -> [argsDiagram,functionDiagramInBox]
      _ -> [functionDiagramInBox, argsDiagram]

    finalDia = vcat $ argsAndFunctionDiagram ++ [resultDiagram]

listCompDiagram :: SpecialBackend b
  => IconInfo
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> [Maybe NamedIcon]
  -> TransformableDia b
listCompDiagram
  iconInfo
  maybeItem
  genIcons
  qualIcons
  di
  = finalDia where 
    colorStyle = diColorStyle di
    name = diName di 
    boxWidth =  width argPorts

    qualDiagrams = zipWith ( makeAppInnerIcon iconInfo di None) listCompQualPortList (fmap pure qualIcons)
    qualDiagram = hcat $ map (inCaseDecisionFrame colorStyle) qualDiagrams
    qualDiagramWithLine = alignB ( listCompPipe colorStyle (letterHeight + height qualDiagram)) ||| alignB qualDiagram

    argPortsUncentred =  zipWith 
      (makePassthroughIcon iconInfo di None) 
      (zip argPortList valuePortList) 
      (fmap pure genIcons)

    argPortsCentred  = fmap alignB argPortsUncentred
    argPorts = centerX $ hsep portSeparationSize argPortsCentred
    argsDiagram = inFrame argPorts (listC colorStyle) boxWidth (height argPorts)

    resultDiagram =  makeResultDiagram name

    itemDiagram = makeInputDiagram iconInfo di (pure maybeItem) name

    listCompItemDiagram = inItemFrame colorStyle itemDiagram

    finalDia = vcat [argsDiagram, qualDiagramWithLine, listCompItemDiagram, resultDiagram]
    

listLitDiagram :: SpecialBackend b
  => IconInfo
  -> ListLitFlavor
  -> [Maybe NamedIcon]
  -> Delimiters
  -> TransformableDia b
listLitDiagram iconInfo flavor literals delimiters drawingInfo = finalDia where
  colorStyle = diColorStyle drawingInfo
  name = diName drawingInfo
  asPatternPort = makeAppInnerIcon iconInfo drawingInfo None PatternUnpackingPort (pure Nothing)
  literalDias = map alignB $ zipWith ( makeAppInnerIcon iconInfo drawingInfo None) argPortList (fmap pure literals)
  delimitersDias = map (listLitDelimiterDia colorStyle flavor) delimiters
  litDiagram = concat . transpose $ [delimitersDias,literalDias]
  listDia = centerX $ hcat litDiagram
  resultDia = centerX $ makeResultDiagram name
  finalDia = vcat [asPatternPort, listDia, resultDia]

-- | generalNestedCaseDia port layout:
-- 0 -> top
-- 1 -> bottom
-- odds -> left
-- evens -> right
caseDiagram ::forall b. SpecialBackend b
  => IconInfo
  -> Maybe NamedIcon
  -> [(Maybe NamedIcon,Maybe NamedIcon)]
  -> CaseFlavor
  -> TransformableDia b
caseDiagram iconInfo input condsAndVals flavor di
  = finalDia where
    colorStyle = diColorStyle di
    name = diName di
    symbolColor = caseRhsC colorStyle

    resultDia = makeResultDiagram name

    iFVarAndConstIcons = zipWith iconMapper casePortPairList condsAndVals

    iconMapper (condPort,valPort) (cond, val) = decisionDiagram where
      condDia = alignB $ vcat [inCaseDecisionFrame colorStyle condIcon, spaceForBigLine]
      valDia = alignB $ vcat [valIcon, caseValSymbol symbolColor]
      condIcon = makeAppInnerIcon iconInfo di InCase condPort (Labeled "" cond)
      valIcon = makeAppInnerIcon iconInfo di None valPort (Labeled "" val)
      spaceForBigLine = strutY defaultLineWidth
      decisionDiagram = hcat [condDia, valDia]

    multiIfDia = centerX $ hsep portSeparationSize  iFVarAndConstIcons
    decisionLine = lwG defaultLineWidth $ lc symbolColor $ hrule (width multiIfDia)
    allCases = multiIfDia <> decisionLine

    inputLambdaLine = alignT inputLine <> inputDiagram
    inputLine = lwG defaultLineWidth $ lc symbolColor $ vrule (height allCases)
    inputDiagram
      | flavor == MultiIfFlavor = alignB $ coloredTextBox (textBoxTextC colorStyle) ifConditionConst
      | otherwise = centerX $ alignB $ makeInputDiagram iconInfo di (pure input) name

    allCasesAtRight = inputLambdaLine <> alignTL allCases
    finalDia = vcat [allCasesAtRight, resultDia]

functionArgDia :: SpecialBackend b
  => [String]
  -> TransformableDia b
functionArgDia argumentNames drawingInfo
  = named name finalDiagram where
  colorStyle = diColorStyle drawingInfo
  name = diName drawingInfo
    -- argumetPort = zipWith (makePassthroughPorts name) (zip argPortList valuePortList) argumentNames
  argumetPort = zipWith (makeLabelledPort None colorStyle name) valuePortList argumentNames
  combinedArgumetPort = hsep portSeparationSize argumetPort
  -- argumetsInBox = inFrame combinedArgumetPort (lambdaC colorScheme) (width combinedArgumetPort) (height combinedArgumetPort)
  finalDiagram = combinedArgumetPort

functionDefDia :: SpecialBackend b
  => IconInfo
  -> String
  -> Maybe NamedIcon 
  -> TransformableDia b
functionDefDia iconInfo functionName input drawingInfo = finalDiagram where
  colorStyle = diColorStyle drawingInfo
  name = diName drawingInfo
  lambdaSymbol = alignBR (lambdaBodySymbol functionName colorStyle ||| memptyWithPosition)
  inputDiagram = makeInputDiagram' FunDefValuePort iconInfo drawingInfo (pure input) name
  
  resultDiagram =  makeResultDiagram name
  finalDiagram = lambdaSymbol <> (inputDiagram === resultDiagram)

-- | The ports of flatLambdaIcon are:
-- 0: Result icon
-- 1: The lambda function value
-- 2,3.. : The parameters
lambdaRegionToDiagram :: SpecialBackend b
  => ColorStyle
  -> [SpecialDiagram b]
  -> NodeName
  -> Int
  -> SpecialDiagram b
lambdaRegionToDiagram colorStyle enclosedDiagarms (NodeName nameInt) level
  = regionSymbol
  where
    additionalPadding = defaultLineWidth * fromIntegral level
    paddingSizeX = lambdaRegionPaddingX + 1.5 * additionalPadding
    paddingSizeY = lambdaRegionPaddingY + 1.5 * additionalPadding
    diagramBoxes = map  (boundingRect . extrudeLeft paddingSizeX . extrudeRight paddingSizeX . extrudeBottom paddingSizeY . extrudeTop paddingSizeY ) enclosedDiagarms
    boxesPath = mconcat diagramBoxes
    contentsRect = strokePath $ B.union Winding boxesPath

    edgeColors = edgeListC colorStyle
    namePortHash = mod nameInt (length edgeColors)
    regionLineColor = edgeColors !! namePortHash

    line =  lwG defaultLineWidth contentsRect
    regionSymbol = setDashing regionLineColor [dashingLineLen, dashingSpaceLen] line

dashingLineLen :: NumericType
dashingLineLen = 0.15 * letterHeight
dashingSpaceLen :: NumericType
dashingSpaceLen = 0.35 * letterHeight