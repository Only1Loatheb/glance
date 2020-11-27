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

import Types (DiaQuery)

customRenderSVG' :: (Typeable n, Show n, RealFloat n) =>
  FilePath
  -> n
  -> Dia.QDiagram SVG Dia.V2 n DiaQuery
  -> IO ()
customRenderSVG' outputFilename scale qDiagram = customRenderSVG outputFilename scale diagram where
  diagram = Dia.clearValue qDiagram

customRenderSVG :: (Typeable n, Show n, RealFloat n) =>
  FilePath
  -> n
  -> Dia.QDiagram SVG Dia.V2 n Dia.Any
  -> IO ()
customRenderSVG outputFilename scale diagram
  = renderSVG' outputFilename svgOptions diagram where
    -- This xml:space attribute preserves the whitespace in the svg text.
    attributes = [bindAttr XmlSpace_ (pack "preserve")]
    -- https://github.com/diagrams/diagrams-svg/blob/master/src/Diagrams/Backend/SVG.hs#L367
    mkPrefix :: FilePath -> T.Text
    mkPrefix = T.filter isAlpha . T.pack . takeBaseName
    svgOptions = SVGOptions (Dia.mkWidth $ scale * Dia.width diagram) Nothing (mkPrefix outputFilename) attributes True