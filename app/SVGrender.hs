module SVGrender (
  customRenderSVG
  , customRenderSVG'
  )
where

import Diagrams.Backend.SVG(renderSVG', Options(..), SVG)
import Graphics.Svg.Attributes(bindAttr, AttrTag(..))
import Data.Text as T(pack, filter, Text)
import Data.Char(isAlpha)
import System.FilePath(takeBaseName)
import Data.Typeable(Typeable)
import qualified Diagrams.Prelude as Dia
import IconToDiagram(colorScheme, ColorStyle(..))

import           Types (
  Edge(..)
  , Icon(..)
  , NamedIcon(..)
  , NameAndPort(..)
  , Connection
  , NodeName(..)
  , Port
  , Named(..)
  , EdgeOption(..)
  , DiaQuery
  , NodeQueryValue(..)
  , SrcRef
  )


customRenderSVG' :: (Typeable n, Show n, RealFloat n) =>
  FilePath
  -> Dia.SizeSpec Dia.V2 n
  -> Dia.QDiagram SVG Dia.V2 n DiaQuery
  -> IO ()
customRenderSVG' outputFilename size qDiagram = customRenderSVG outputFilename size diagram where
  diagram = Dia.clearValue qDiagram

customRenderSVG :: (Typeable n, Show n, RealFloat n) =>
  FilePath
  -> Dia.SizeSpec Dia.V2 n
  -> Dia.QDiagram SVG Dia.V2 n Dia.Any
  -> IO ()
customRenderSVG outputFilename size diagram 
  = renderSVG' outputFilename svgOptions diagramWithBackground where
    diagramWithBackground = Dia.bg (backgroundC colorScheme) diagram
    -- This xml:space attribute preserves the whitespace in the svg text.
    attributes = [bindAttr XmlSpace_ (pack "preserve")]
    -- https://github.com/diagrams/diagrams-svg/blob/master/src/Diagrams/Backend/SVG.hs#L367
    mkPrefix :: FilePath -> T.Text
    mkPrefix = T.filter isAlpha . T.pack . takeBaseName
    svgOptions = SVGOptions size Nothing (mkPrefix outputFilename) attributes True
