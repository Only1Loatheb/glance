{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icons
    (
    TransformParams(..),
    TransformableDia,
    getPortAngles,
    iconToDiagram,
    inputPort,
    resultPort,
    argumentPorts,
    caseRhsPorts,
    casePatternPorts,
    multiIfRhsPorts,
    multiIfBoolPorts,
    textBox,
    multilineComment,
    defaultLineWidth,
    ColorStyle(..),
    colorScheme,
    coloredTextBox,
    sizeUnit,
    findIconFromName
    ) where

import Diagrams.Prelude hiding ((&), (#), Name)

import qualified Control.Arrow as Arrow
import Data.Either(partitionEithers)
import qualified Data.IntMap as IM
import Data.List(find)
import Data.Maybe(listToMaybe, isJust, fromJust, mapMaybe)
import Data.Typeable(Typeable)

import Constants(pattern InputPortConst, pattern ResultPortConst)
import DrawingColors(colorScheme, ColorStyle(..))
import Types(Icon(..), SpecialQDiagram, SpecialBackend, SpecialNum
            , NodeName(..), Port(..), LikeApplyFlavor(..),
            SyntaxNode(..), NamedIcon, Labeled(..), IconInfo
            , Named(..))

{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Unnecessary hiding" #-}

-- TYPES --

data TransformParams n = TransformParams {
  tpName :: NodeName  -- The icon's name
  , tpNestingDepth :: Int  -- The icon's nesting depth
  , tpIsReflected :: Bool  -- If the icon will be reflected
  , tpAngle :: Angle n  -- By what angle will the icon be rotated
  }

-- | A TransformableDia is a function that returns a diagram for an icon when
-- given the icon's name, its nesting depth, whether it will be reflected, and
-- by what angle it will be rotated.
type TransformableDia b n = TransformParams n -> SpecialQDiagram b n


-- CONSTANTS --
defaultLineWidth :: (Fractional a) => a
defaultLineWidth = 0.15

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
  = fc col $ lw none $ rotateBy (-1/12) $ eqTriangle (2 * sizeUnit)

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

resultSymbol :: SpecialBackend b n =>
  SpecialQDiagram b n
resultSymbol
  = lwG defaultLineWidth
    $ lc (regionPerimC colorScheme)
    $ fc (regionPerimC colorScheme) $ circle sizeUnit

inIfConstBox :: SpecialBackend b n
  => Colour Double
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
inIfConstBox borderColor diagram  =
  finalDiagram where
    line = hrule (width diagram)
    coloredLine = lwG defaultLineWidth $  lc borderColor line
    triangleToRight = centerY $ fromOffsets triangleOfssets
    halfHeight = (height diagram) / 2
    triangleOfssets = [halfHeight *^ (unitY+unitX), halfHeight *^ (unitY-unitX)]
    coloredTriangleToRight =  lwG defaultLineWidth $  lc borderColor $ triangleToRight
    diagramWithLines = centerY $ vcat [coloredLine, diagram, coloredLine]
    finalDiagram = centerX $ hcat [reflectX coloredTriangleToRight, diagramWithLines, coloredTriangleToRight]


-- BEGIN Exported icon functions --

findIconFromName :: IconInfo -> NodeName -> NamedIcon
findIconFromName icons name@(NodeName nameInt)
  = Named name $ IM.findWithDefault
    (error $ "findIconFromName: icon not found.\nicons="
      <> show icons <> "\nname=" <> show name)
    nameInt
    icons

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

-- BEGIN getPortAngles line goes out in that direction--

applyPortAngles :: Floating n => Port -> [Angle n]
applyPortAngles (Port x) = fmap (@@ turn) $ case x of
  0 -> [3/8, 1/2, 5/8] -- TODO Don't use angle of 1/2 for nested icons here
  1 -> [3/4] -- line goes out with value to label
  _ -> [1/4, 1/4] -- [idk, side fromline comes with value like in lambda]

lambdaPortAngles :: Floating n => Bool -> Port -> [Angle n]
lambdaPortAngles embedded (Port x) = fmap (@@ turn) $ case x of
  -- 0 == lambda return value Icon
  0 -> if embedded
       then [3/4, 3/4]
       else [1/4, 1/4, 1/4]
  -- 1 == value port
  --1 -> [1/8, 7/8, 0]
  1 -> [3/4]
  _ -> [3/4, 3/4]

pAppPortAngles :: Floating n => Port -> [Angle n]
pAppPortAngles (Port x) = fmap (@@ turn) $ case x of
  0 -> [1/4]
  1 -> [0]
  _ -> [1/2]

multiIfPortAngles :: Floating n => Port -> [Angle n]
multiIfPortAngles (Port port) = case port of
  0 -> [1/4 @@ turn]
  1 -> [3/4 @@ turn]
  _ -> otherAngles where otherAngles
                           | even port = [0 @@ turn]
                           | otherwise = [1/2 @@ turn]

findNestedIcon :: IconInfo -> NodeName -> Icon -> Maybe Icon
findNestedIcon iconInfo name icon = case icon of
  NestedApply _ headIcon args
    -> snd
       <$> findIcon
       iconInfo
       name
       ((fmap . fmap) (findIconFromName iconInfo) (headIcon : args))
  NestedPApp constructor args ->
    snd <$> findIcon iconInfo name (fmap laValue (constructor:args))
  _ -> Nothing

findIcon :: IconInfo -> NodeName -> [Maybe NamedIcon] -> Maybe (Int, Icon)
findIcon iconInfo name args = icon where
  numberedArgs = zip ([0,1..] :: [Int]) args
  filteredArgs = Arrow.second fromJust <$> filter (isJust . snd) numberedArgs
  nameMatches (_, Named n _) = n == name
  icon = case find nameMatches filteredArgs of
    Nothing -> listToMaybe $ mapMaybe findSubSubIcon filteredArgs
    Just (argNum, Named _ finalIcon) -> Just (argNum, finalIcon)
    where
      findSubSubIcon (argNum, Named _ subIcon)
        = case findNestedIcon iconInfo name subIcon of
            Nothing -> Nothing
            Just x -> Just (argNum, x)

generalNestedPortAngles :: SpecialNum n
  => IconInfo
  -> (Port -> [Angle n])
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> Port -> Maybe NodeName -> [Angle n]
generalNestedPortAngles iconInfo defaultAngles headIcon args port maybeNodeName =
  case maybeNodeName of
    Nothing -> defaultAngles port
    Just name -> case findIcon iconInfo name (headIcon : args) of
      Nothing -> []
      Just (_, icon) -> getPortAnglesHelper True iconInfo icon port Nothing

reflectXAngle :: SpecialNum n => Angle n -> Angle n
reflectXAngle x = reflectedAngle where
  normalizedAngle = normalizeAngle x
  reflectedAngle = (-) <$> halfTurn <*> normalizedAngle

-- TODO reflect the angles for the right side sub-icons
nestedMultiIfPortAngles :: SpecialNum n
  => IconInfo
  -> [Maybe NamedIcon]
  -> Port
  -> Maybe NodeName
  -> [Angle n]
nestedMultiIfPortAngles iconInfo args port maybeNodeName = case maybeNodeName of
  Nothing -> multiIfPortAngles port
  Just name -> case findIcon iconInfo name args of
    Nothing -> []
    -- TODO Don't use hardcoded numbers
    -- The arguments correspond to ports [0, 2, 3, 4 ...]
    Just (argNum, icon) -> if odd argNum && argNum >= 1
      -- The icon will be reflected
      then fmap reflectXAngle subAngles
      else subAngles
      where
        subAngles = getPortAnglesHelper True iconInfo icon port Nothing

getPortAngles :: SpecialNum n
  => IconInfo -> Icon -> Port -> Maybe NodeName -> [Angle n]
getPortAngles = getPortAnglesHelper False

getPortAnglesHelper :: SpecialNum n
  => Bool -> IconInfo -> Icon -> Port -> Maybe NodeName -> [Angle n]
getPortAnglesHelper embedded iconInfo icon port maybeNodeName = case icon of
  TextBoxIcon _ -> []
  BindTextBoxIcon _ -> []
  MultiIfIcon _ -> multiIfPortAngles port
  CaseIcon _ -> multiIfPortAngles port
  CaseResultIcon -> []
  LambdaIcon _ _ _ -> lambdaPortAngles embedded port
  NestedApply _ headIcon args
    -> generalNestedPortAngles
       iconInfo
       applyPortAngles
       -- TODO Refactor with iconToDiagram
       (fmap (findIconFromName iconInfo) headIcon)
       ((fmap . fmap) (findIconFromName iconInfo) args)
       port
       maybeNodeName
  NestedPApp headIcon args
    -> generalNestedPortAngles
       iconInfo
       pAppPortAngles
       (laValue headIcon)
       (fmap laValue args)
       port
       maybeNodeName
  NestedCaseIcon args
    -> nestedMultiIfPortAngles
       iconInfo
       ((fmap . fmap) (findIconFromName iconInfo) args)
       port
       maybeNodeName
  NestedMultiIfIcon args
    -> nestedMultiIfPortAngles
       iconInfo
       ((fmap . fmap) (findIconFromName iconInfo) args)
       port
       maybeNodeName

-- END getPortAngles --

-- BEGIN Port numbers

argPortsConst :: [Port]
argPortsConst = fmap Port [2,3..]

-- TODO It's a bit strange that the parameter is a SyntaxNode, not an Icon.
inputPort :: SyntaxNode -> Port
inputPort = const InputPortConst

resultPort :: SyntaxNode -> Port
resultPort = const ResultPortConst

caseRhsPorts :: [Port]
caseRhsPorts = fmap Port [3,5..]

casePatternPorts :: [Port]
casePatternPorts = fmap Port [2,4..]

multiIfRhsPorts :: [Port]
multiIfRhsPorts = casePatternPorts

multiIfBoolPorts :: [Port]
multiIfBoolPorts = caseRhsPorts

argumentPorts :: SyntaxNode -> [Port]
argumentPorts n = case n of
  (ApplyNode _ _) -> defaultPorts
  PatternApplyNode _ _-> defaultPorts
  (FunctionDefNode _ _) -> defaultPorts
  CaseOrMultiIfNode _ _ -> defaultPorts
  NameNode _ -> []
  BindNameNode _ -> []
  LiteralNode _ -> []
  CaseResultNode -> []
  where
    defaultPorts = argPortsConst
-- END Port numbers

-- END Exported icon functions --

-- BEGIN Diagram helper functions --

-- | Make an identity TransformableDia
identDiaFunc :: SpecialNum n => SpecialQDiagram b n -> TransformableDia b n
identDiaFunc dia transformParams = nameDiagram (tpName transformParams) dia

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
  (_:_:_) -> portAndCircle ||| label
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

makeTransformedText :: SpecialBackend b n
  => IconInfo
  -> TransformParams n
  -> Labeled (Maybe NamedIcon)
  -> SpecialQDiagram b n
makeTransformedText iconInfo tp maybeFunText = case laValue maybeFunText of
  Just _ ->
    makeAppInnerIcon iconInfo tp True InputPortConst maybeFunText
  Nothing -> mempty

appArgBox :: (HasStyle a, Typeable (N a)
             , TrailLike a, RealFloat (N a), V a ~ V2)
          => Colour Double -> N a -> N a -> a
appArgBox borderColor topAndBottomWidth portHeight
  = coloredArgBox where
    coloredArgBox = lwG defaultLineWidth $ lcA (withOpacity borderColor defaultOpacity) argBox
    argBox = rect topAndBottomWidth (portHeight + verticalSeparation)
    verticalSeparation = sizeUnit

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


-- | Like beside, but it puts the second dia atop the first dia
beside' :: (Semigroup a, Juxtaposable a) => V a (N a) -> a -> a -> a
beside' dir dia1 dia2 = juxtapose dir dia1 dia2 <> dia1


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

-- BEGIN Text boxes and icons --

-- Text constants --
textBoxFontSize :: (Num a) => a
textBoxFontSize = 1

monoLetterWidthToHeightFraction :: (Fractional a) => a
monoLetterWidthToHeightFraction = 0.61

textBoxHeightFactor :: (Fractional a) => a
textBoxHeightFactor = 1.4

textFont :: String
textFont = "monospace"

-- BEGIN Text helper functions --

-- This may be a faster implementation of normalizeAngle
--Get the decimal part of a float
-- reduceAngleRange :: SpecialNum a => a -> a
-- reduceAngleRange x = x - fromInteger (floor x)

-- | Given the number of letters in a textbox string, make a rectangle that will
-- enclose the text box. Since the normal SVG text has no size, some hackery is
-- needed to determine the size of the text's bounding box.
rectForText :: (InSpace V2 n t, TrailLike t) => Int -> t
rectForText n = rect rectangleWidth (textBoxFontSize * textBoxHeightFactor)
  where
    rectangleWidth
      = (fromIntegral n * textBoxFontSize * monoLetterWidthToHeightFraction)
        + (textBoxFontSize * 0.3)

-- END Text helper functions

commentTextArea :: SpecialBackend b n =>
  Colour Double -> String -> SpecialQDiagram b n
commentTextArea textColor t =
  alignL $ fontSize
  (local textBoxFontSize)
  (font textFont $ fc textColor $ topLeftText t)
  <>  alignTL (lw none $ rectForText (length t))

multilineComment :: SpecialBackend b n =>
  Colour Double
  -> AlphaColour Double -> String -> SpecialQDiagram b n
multilineComment textColor _boxColor t = lwG (0.6 * defaultLineWidth) textDia
  where
    textLines = lines t
    textAreas = map (commentTextArea textColor) textLines
    textDia = vcat textAreas

coloredTextBox :: SpecialBackend b n =>
  Colour Double
  -> AlphaColour Double -> String -> SpecialQDiagram b n
coloredTextBox textColor boxColor t
  = boxAroundText <> textLabel  where
    textLabel =
      fontSize
      (local textBoxFontSize)
      (font textFont $ fillColor textColor $ text t)
    boxAroundText =
      lwG -- A convenient synonym for 'lineWidth (global w)'.
      (0.6 * defaultLineWidth)
      (lcA boxColor -- A synonym for lineColor, specialized to AlphaColour Double (i.e. colors with transparency)
        $ fcA (withOpacity (backgroundC colorScheme) 0) -- last param is radius of circular rounded corners 
        $ rectForText (length t))

transformCorrectedTextBox :: SpecialBackend b n =>
  String
  -> Colour Double
  -> Colour Double
  -> Bool
  -> Angle n
  -> SpecialQDiagram b n
transformCorrectedTextBox str textColor borderColor reflect angle =
  rotateBy
  textBoxRotation
  (reflectIfTrue reflect (coloredTextBox textColor (opaque borderColor) str))
  where
    -- If normalizeAngle is slow, the commented out function reduceAngleRange
    -- might be faster.
    reducedAngle = normalizeAngle angle ^. turn
    textBoxRotation
      = if (reducedAngle > (1/4)) && (reducedAngle < (3/4)) then 1 / 2 else 0
    reflectIfTrue shouldReflect dia
      = if shouldReflect then reflectX dia else dia

transformableBindTextBox :: SpecialBackend b n =>
  String -> Bool -> Angle n -> SpecialQDiagram b n
transformableBindTextBox str
  = transformCorrectedTextBox
    str
    (bindTextBoxTextC colorScheme)
    (bindTextBoxC colorScheme)

bindTextBox :: SpecialBackend b n =>
  String -> SpecialQDiagram b n
bindTextBox
  = coloredTextBox (bindTextBoxTextC colorScheme)
    $ opaque (bindTextBoxC colorScheme)

textBox :: SpecialBackend b n =>
  String -> TransformableDia b n
textBox t (TransformParams name _ reflect angle)
  = nameDiagram name $ transformCorrectedTextBox
    t (textBoxTextC colorScheme) (textBoxC colorScheme) reflect angle

-- END Text boxes and icons

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

-- TODO Improve design to be more than a circle.
caseResult :: SpecialBackend b n =>
  SpecialQDiagram b n
caseResult = applySymbol caseCColor
  where
    caseCColor = caseRhsC colorScheme

caseC :: SpecialBackend b n =>
  SpecialQDiagram b n -> SpecialQDiagram b n
caseC portDia = caseResult <> portDia

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
nestedLambda :: SpecialBackend b n
  => IconInfo
  -> [String]
  -> Maybe NamedIcon
  -> TransformableDia b n
nestedLambda iconInfo paramNames mBodyExp (TransformParams name level reflect angle)
  -- = centerXY $ lambdaBodyDiagram ||| centerY (named name inputOutputDiagram)
  = centerXY (named name inputsResultAndBodyDia)
  -- centerY (named name inputOutputDiagram)
  where
  inputsResultAndBodyDia = (alignB inputsAndBodyDia) <> (alignT resultDiagram)
  inputsAndBodyDia = (alignB inputDiagram) <> (alignT lambdaBodyDiagram)

  inputDiagram =  hsep sizeUnit portIcons
  portIcons = zipWith (makeLabelledPort name reflect angle) paramNames argPortsConst

  resultDiagram = makeQualifiedPort name ResultPortConst <> resultSymbol
  -- inputIcons
  --   = (makeQualifiedPort name InputPortConst <> inputSymbol)
  --     :
  --     (portIcons
  --       ++ [makeQualifiedPort name ResultPortConst <> alignR resultSymbol])

  lambdaBodyDiagram = case mBodyExp of
    Nothing -> mempty
    Just (Named bodyNodeName bodyIcon)
      -> iconToDiagram
         iconInfo
         bodyIcon
         (TransformParams bodyNodeName level reflect angle)


-- END Main icons
-- END Icons
