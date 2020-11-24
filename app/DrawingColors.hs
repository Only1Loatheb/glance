module DrawingColors (
  ColorStyle(..)
  , ColorStyleType(..)
  , getColorStyle
  , dummyColorStyle
  ) where

import Diagrams.Prelude hiding ((&), (#))
data ColorStyleType = ColorsOnBlack | ColorsOnWhite | WhiteOnBlack | BlackOnWhite
  deriving (Show, Eq, Ord)

getColorStyle :: (Floating a, Ord a) => ColorStyleType -> ColorStyle a
getColorStyle colorStyleType = case colorStyleType of
  ColorsOnBlack -> colorsOnBlackScheme
  ColorsOnWhite -> colorsOnWhiteScheme
  WhiteOnBlack -> whiteOnBlackScheme
  BlackOnWhite -> blackOnWhiteScheme

-- COLO(U)RS --
dummyColorStyle :: ColorStyle Double
dummyColorStyle = colorsOnBlackScheme

data ColorStyle a = ColorStyle {
  backgroundC :: Colour a,
  textBoxTextC :: Colour a,
  applyCompositionC :: [Colour a],
  boolC :: Colour a,
  lambdaC :: Colour a,
  caseRhsC :: Colour a,
  patternC :: Colour a,
  bindTextBoxTextC :: Colour a,
  edgeListC :: [Colour a],
  nestingC :: [Colour a],
  listC :: Colour a,
  tupleC :: Colour a
}

colorsOnBlackScheme :: (Floating a, Ord a) => ColorStyle a
colorsOnBlackScheme = ColorStyle {
  backgroundC = black,
  textBoxTextC = white,
  applyCompositionC = cycle [cyan,lightSlightlyPurpleBlue],
  boolC = slightlyGreenYellow,
  lambdaC = lime,
  caseRhsC = orange,
  patternC = lightMagenta,
  bindTextBoxTextC = lightGreen,
  edgeListC = [white, red, reddishOrange, lightPurple, yellow, lightBlue, cyan, coral,maroon,lightpink, olive, green],
  nestingC = cycle [red, reddishOrange, yellow],
  listC = lightBlue,
  tupleC = lightPurple
}

slightlyGreenYellow :: (Floating a, Ord a) => Colour a
slightlyGreenYellow = sRGB24 212 255 0
lightMagenta :: (Floating a, Ord a) => Colour a
lightMagenta = sRGB24 255 94 255
lightSlightlyPurpleBlue :: (Floating a, Ord a) => Colour a
lightSlightlyPurpleBlue = sRGB24 109 87 255
reddishOrange :: (Floating a, Ord a) => Colour a
reddishOrange = sRGB24 255 119 0
-- :: (Floating a, Ord a) => Colour a
--lightBlue = sRGB24 126 127 255
lightBlue :: (Floating a, Ord a) => Colour a
lightBlue = sRGB24 35 156 255
lightPurple :: (Floating a, Ord a) => Colour a
lightPurple = sRGB24 208 137 255
lightGreen :: (Floating a, Ord a) => Colour a
lightGreen = sRGB24 180 255 145


colorsOnWhiteScheme :: (Floating a, Ord a) => ColorStyle a
colorsOnWhiteScheme = ColorStyle {
  backgroundC = white,
  textBoxTextC = black,
  applyCompositionC = cycle [cyan,lightSlightlyPurpleBlue],
  boolC = slightlyGreenYellow,
  lambdaC = lime,
  caseRhsC = orange,
  patternC = lightMagenta,
  bindTextBoxTextC = lightGreen,
  edgeListC = [black, red, reddishOrange, lightPurple, yellow, lightBlue, cyan, coral,maroon,lightpink, olive, green],
  nestingC = cycle [red, reddishOrange, yellow],
  listC = lightBlue,
  tupleC = lightPurple
}

whiteOnBlackScheme :: (Floating a, Ord a) => ColorStyle a
whiteOnBlackScheme = ColorStyle {
  backgroundC = black,
  textBoxTextC = white,
  applyCompositionC = repeat white,
  boolC = white,
  lambdaC = white,
  caseRhsC = white,
  patternC = white,
  bindTextBoxTextC = white,
  edgeListC = [white],
  nestingC = repeat white,
  listC = white,
  tupleC = white
}

blackOnWhiteScheme :: (Floating a, Ord a) => ColorStyle a
blackOnWhiteScheme = ColorStyle {
  backgroundC = white,
  textBoxTextC = black,
  applyCompositionC = repeat black,
  boolC = black,
  lambdaC = black,
  caseRhsC = black,
  patternC = black,
  bindTextBoxTextC = black,
  edgeListC = [black],
  nestingC = repeat black,
  listC = black,
  tupleC = black
}