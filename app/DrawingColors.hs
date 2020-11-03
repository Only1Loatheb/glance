module DrawingColors (
  ColorStyle(..)
  , colorScheme
  , colorOnBlackScheme
  , whiteOnBlackScheme
  , randomColorScheme
) where

import Diagrams.Prelude hiding ((&), (#))

{-# ANN module "HLint: ignore Unnecessary hiding" #-}

-- COLO(U)RS --
colorScheme :: ColorStyle Double
colorScheme = colorOnBlackScheme

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
  listC :: Colour a
}

colorOnBlackScheme :: (Floating a, Ord a) => ColorStyle a
colorOnBlackScheme = ColorStyle {
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
  listC = lightBlue
}
  where
    slightlyGreenYellow = sRGB24 212 255 0
    lightMagenta = sRGB24 255 94 255
    lightSlightlyPurpleBlue = sRGB24 109 87 255
    reddishOrange = sRGB24 255 119 0
    --lightBlue = sRGB24 126 127 255
    lightBlue = sRGB24 35 156 255
    lightPurple = sRGB24 208 137 255
    lightGreen = sRGB24 180 255 145


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
  listC = white
}

-- Use this to test that all of the colors use the colorScheme
randomColorScheme :: (Floating a, Ord a) => ColorStyle a
randomColorScheme = ColorStyle {
  backgroundC = darkorchid,
  textBoxTextC = blue,
  applyCompositionC = repeat green,
  boolC = lightpink,
  lambdaC = cyan,
  caseRhsC = red,
  patternC = olive,
  bindTextBoxTextC = lime,
  edgeListC = [wheat],
  nestingC = cycle [red, yellow, purple, pink, lightblue, magenta],
  listC = lightblue
}
