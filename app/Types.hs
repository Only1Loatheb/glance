{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module Types (
  Named(..)
  , NamedIcon
  , IconInfo
  , Icon(..)
  , SyntaxNode(..)
  , NodeName(..)
  , Port(..)
  , NameAndPort
  , Connection
  , Edge(..)
  , EdgeOption(..)
  , IDState(..)
  , Drawing
  , QueryableDrawing
  , DrawingBackend()
  , SgNamedNode
  , IngSyntaxGraph
  , ApplyFlavor(..)
  , CaseFlavor(..)
  , Labeled(..)
  , EmbedDirection(..)
  , EmbedInfo(..)
  , AnnotatedGraph
  , NodeInfo(..)
  , Embedder(..)
  , mkEmbedder
  , EmbedderSyntaxNode
  , DrawingInfo(..)
  , TransformableDia
  , DiaQuery
  , NodeQueryValue(..)
  , DeclQueryValue(..)
  , SyntaxNodeCore(..)
  , DiagramIcon(..)
  , SrcRef
  , ModuleGraphs
  , AnnotatedFGR
  , SourceCode
  , ViewGraphs
  , QueryValue(..)
  , View
  , CreateView
  , FuncDefRegionInfo
  , Reference
  , EvalContext
  , SgBind
  , SgSink(..)
  , SyntaxGraph(..)
  , GraphAndRef(..)
  , SgBindMap
  , Delimiters
  , ListLitFlavor(..)
  , NumericType
  , PointType
  , ColorStyle
  , ColorStyle'(..)
  , InCaseOrInApply(..)
  , laLabel
  , laValue
  , naVal
  , naName
  , monoLetterWidthToHeight
  , setDashing
) where

import Diagrams.Prelude(
    Any
  , Bifunctor(bimap)
  , (&)
  , dashingG
  , boundingBox
  , getCorners
  , _LG
  , lGradEnd
  , lGradStart
  , lc
  , lineTexture
  , mkLinearGradient
  , mkStops
  , (.~)
  , makeLenses
  , Colour
  , IsName
  , QDiagram
  , Renderable
  , Coordinates((^&))
  , Path
  , SpreadMethod(GradPad)
  , Point
  , V2
  , unp2
  , p2
  , (.-.)
  , norm
  )
import Diagrams.TwoD.Text(Text)

import qualified Data.Graph.Inductive as ING
import qualified Data.Graph.Inductive.PatriciaTree as FGR
import qualified Data.IntMap as IMap
import qualified Data.Map as SMap
import qualified Data.Set as Set
import Data.Typeable(Typeable)
import qualified Language.Haskell.Exts as Exts
import qualified Diagrams.Backend.Canvas as CV
import qualified Diagrams.Backend.SVG as SVG
import Data.Maybe (fromMaybe)

newtype NodeName = NodeName Int deriving (Typeable, Eq, Ord, Show)
instance IsName NodeName

data Named a = Named {
  _naName :: NodeName
  , _naVal :: a}
  deriving (Show, Eq, Ord, Functor)


type NamedIcon = Named Icon

data Labeled a = Labeled {
    _laLabel :: String  
  , _laValue :: a
  }
  deriving (Show, Eq, Ord)

instance Functor Labeled where
  fmap f (Labeled str value ) = Labeled str (f value)

instance Applicative Labeled where
  pure x = Labeled "" x
  (Labeled fStr f) <*> (Labeled xStr x) = Labeled (fStr <> xStr) (f x)

type FuncDefRegionInfo = (
  Set.Set NodeName  -- Nodes inside the lambda
  , Int) -- lambdaNestingLevel

type IconInfo = IMap.IntMap Icon

type SrcRef = Exts.SrcSpan

type Delimiters = [String] 

-- | A datatype that represents an icon.
-- The TextBoxIcon's data is the text that appears in the text box.
data Icon = Icon DiagramIcon SrcRef
  deriving (Show, Eq, Ord)

data DiagramIcon = 
  TextBoxIcon String
  | FunctionArgIcon
    [String]  -- Parameter labels
    FuncDefRegionInfo
    NodeName -- FunctionDefIcon Name
  | FunctionDefIcon
    String  -- Function name
    (Maybe NodeName) -- embeded body node
  | BindTextBoxIcon String
  | NestedApply
    ApplyFlavor  -- apply or compose
    (Maybe NodeName)  -- The function for apply, or the argument for compose
    [Maybe NodeName]  -- list of arguments or functions
  | NestedPatternApp
    String  -- Data constructor
    [Labeled (Maybe NodeName)]  -- Arguments
    (Maybe NodeName)  -- asigned value
  | NestedCaseIcon 
    CaseFlavor 
    (Maybe NodeName) -- imput node name
    [(Maybe NodeName,Maybe NodeName)]
  | CaseResultIcon
  | ListCompIcon
    (Maybe NodeName) -- item constructor
    [Maybe NodeName] -- generators
    [Maybe NodeName] -- qualifiers
  | ListLitIcon
    ListLitFlavor
    [Maybe NodeName]
    Delimiters
  | Concentrator
  deriving (Show, Eq, Ord)

data ApplyFlavor = ApplyFlavor | ComposeFlavor deriving (Show, Eq, Ord)

data CaseFlavor = CaseFlavor | MultiIfFlavor deriving (Show, Eq, Ord)
data ListLitFlavor = ListFlavor | TupleFlavor deriving (Show, Eq, Ord)

-- TODO The full edge does not need to be included, just the port.
data Embedder a = Embedder {
  emEmbedded :: Set.Set (NodeName, Edge)  -- ^ Set of embedded nodes
  , emNode :: a
  }
  deriving (Show, Eq, Ord, Functor)

mkEmbedder :: a -> Embedder a
mkEmbedder = Embedder Set.empty

type EmbedderSyntaxNode = Embedder SyntaxNode

type SgNamedNode = Named EmbedderSyntaxNode

data SyntaxNode = SyntaxNode {
  syntaxNodeCore :: SyntaxNodeCore
  , srcRef :: SrcRef
  } 
  deriving (Show, Eq, Ord)

data SyntaxNodeCore = 
  -- Function application, composition, and applying to a composition
  -- The list of nodes is unordered (replace with a map?)
  ApplyNode ApplyFlavor Int
  | PatternNode String [String]
  | BindNameNode String -- for top level bindings
  | LiteralNode String -- Literal values like the string "Hello World"
  | FunctionArgNode
    [String]  -- Parameter labels
    FuncDefRegionInfo
    NodeName -- FunctionDefNode Name
  | FunctionValueNode  -- Function definition (ie. lambda expression)
    String -- function name
  | CaseResultNode -- if same icon is in condition statment and result value
  | CaseNode CaseFlavor Int -- number of alternatives
  | ListCompNode
    Int -- number of generators
    Int -- number of qualifiers  
  | ListLitNode
    ListLitFlavor
    Int -- number of literals
    Delimiters 
  deriving (Show, Eq, Ord)

newtype Port = Port Int deriving (Typeable, Eq, Ord, Show)
instance IsName Port

type NameAndPort = Named Port -- deriving (Show, Eq, Ord) -- there is NamedPort already

type Connection = (NameAndPort, NameAndPort)

data EdgeOption =
  DrawAndConstraint
  | DrawAndNotConstraint
  | DoNotDrawButConstraint Int -- length in number of ranks
  | DrawAsImportant
  | DrawThrough (NameAndPort, NameAndPort)
  deriving (Show, Eq, Ord)

-- | An Edge has an name of the source icon, and its optional port number,
-- and the name of the destination icon, and its optional port number.
data Edge = Edge { 
  edgeOption :: EdgeOption
  , edgeConnection :: Connection
  }
  deriving (Show, Eq, Ord)

-- | IDState is an Abstract Data Type that is used as a state whose value is a
-- unique id.
newtype IDState = IDState Int deriving (Eq, Show)

type NumericType = Double 

type PointType = Point V2 NumericType

-- Note that DrawingBackend is a constraint synonym, not a type synonym.

class (Renderable (Path V2 NumericType) b, Renderable (Text NumericType) b) => DrawingBackend b where
  monoLetterWidthToHeight :: Drawing b -> NumericType
  setDashing :: Colour NumericType -> [NumericType] -> Drawing b -> Drawing b

instance DrawingBackend SVG.SVG where
  monoLetterWidthToHeight = const 0.62
  setDashing regionLineColor dashingPattern a = lc regionLineColor $ dashingG dashingPattern 0 a

-- | Nasty workaround of lack of dashingG
instance DrawingBackend CV.Canvas where
  monoLetterWidthToHeight = const 0.82
  setDashing regionLineColor [] a = lc regionLineColor a
  setDashing regionLineColor dashingPattern a = lineTexture  gradient a where
    (corner1,corner2) = bimap (p2 . dup . uncurry  min . unp2) (p2 . dup . uncurry  max . unp2) $ fromMaybe ((-0.5) ^& 0,0.5 ^& 0) $ getCorners $ boundingBox a
    gradient = mkLinearGradient stops corner1 corner2 GradPad 
      & _LG . lGradStart        .~ corner1
      & _LG . lGradEnd          .~ corner2
    patternLen = sum dashingPattern
    dashingLens = map (/ largerDimention) $ scanl (+) 0.0 
      $ concat $ replicate (floor $ largerDimention / patternLen)  dashingPattern
    largerDimention = norm $ corner2 .-. corner1
    stops = mkStops $ zipWith (\locationKey lineOpacity -> (regionLineColor, locationKey, lineOpacity)) dashingLens (cycle [1.0,0.0])

dup :: a -> (a, a)
dup a = (a,a)

type Drawing b = QDiagram b V2 NumericType Any 
-- class DrawingBackend b => SpecialDiagramC b where

type QueryableDrawing b = QDiagram b V2 NumericType DiaQuery

type IngSyntaxGraph gr = gr SgNamedNode Edge

data EmbedDirection =
  EdEmbedFrom -- The tail
  | EdEmbedTo -- The head
  deriving (Show, Eq)

-- A Nothing eiEmbedDir means the edge is not embedded.
data EmbedInfo a = EmbedInfo {
  eiEmbedDir :: Maybe EmbedDirection
  , eiVal :: a
  }
  deriving (Show, Eq, Functor)

type AnnotatedGraph gr = gr (NodeInfo SgNamedNode) (EmbedInfo Edge)

data NodeInfo a = NodeInfo {
  niParent :: Maybe ING.Node
  , niVal :: a
  }
  deriving (Show, Eq, Functor, Ord)

data DrawingInfo = DrawingInfo {
  diName :: NodeName  -- The icon's name
  , diNestingLevel :: Int  -- The icon's nesting depth
  , diIsIn :: InCaseOrInApply  -- is inside case condition diagram for drawing upside down
  , diColorStyle :: ColorStyle
  }

data InCaseOrInApply = InApply | InCase | None deriving (Show, Eq)
-- | A TransformableDia is a function that returns a diagram for an icon when
-- given the icon's name, its nesting depth, whether it will be reflected, and
-- by what angle it will be rotated.
type TransformableDia b = DrawingInfo -> Drawing b

data NodeQueryValue = NodeQueryValue {
  srcRefNQV :: SrcRef
  , nodeNameNQV :: NodeName
  } deriving (Show, Eq, Ord)

data DeclQueryValue = DeclQueryValue {
  srcRefDQV :: SrcRef
  , graphDQV :: SyntaxGraph
  , commentsBeforeDQV :: [Exts.Comment]
  , commentsAfterDQV :: [Exts.Comment]
  } deriving (Show)

data QueryValue = 
  NodeQv NodeQueryValue
  | DeclQv DeclQueryValue 
  deriving (Show)

type DiaQuery = [QueryValue]

type AnnotatedFGR = AnnotatedGraph FGR.Gr

type SourceCode = String

type View = (Maybe DeclQueryValue, Maybe NodeQueryValue)
type CreateView = DiaQuery -> View -> View

type ModuleGraphs = ([(SrcRef, SyntaxGraph)],[Exts.Comment])
type ViewGraphs = ([(SrcRef, (AnnotatedFGR, AnnotatedFGR))],[Exts.Comment])

-- SYNTAX GRAPH
type Reference = Either String NameAndPort

type EvalContext = Set.Set String

type SgBind = (String, Reference)
type SgBindMap = SMap.Map String Reference

data SgSink = SgSink String NameAndPort deriving (Eq, Ord, Show)

-- | A SyntaxGraph is an abstract representation of Haskell syntax. SyntaxGraphs
-- are generated from the Haskell syntax tree and are used to generate Drawings.
data SyntaxGraph = SyntaxGraph {
  sgNodes :: Set.Set SgNamedNode,
  sgEdges :: Set.Set Edge,
  sgSinks :: Set.Set SgSink, -- values that havent been used (unused bindings)
  sgBinds :: SgBindMap, -- name form upper leval -> reference -- Reference -> Reference 
  -- sgEmbedMap keeps track of nodes embedded in other nodes. If (child, parent)
  -- is in the Map, then child is embedded inside parent.
  sgEmbedMap :: IMap.IntMap NodeName -- NodeName -> NodeName
  } deriving (Show, Eq)

instance Semigroup SyntaxGraph where
  (<>)
    (SyntaxGraph icons1 edges1 sinks1 sources1 map1)
    (SyntaxGraph icons2 edges2 sinks2 sources2 map2)
    = SyntaxGraph
      (Set.union icons1 icons2)
      (Set.union edges1 edges2)
      (Set.union sinks1 sinks2)
      (SMap.union sources1 sources2)
      (IMap.union map1 map2)

instance Monoid SyntaxGraph where
  mempty = SyntaxGraph Set.empty Set.empty Set.empty SMap.empty mempty
  mappend = (<>)

data GraphAndRef = GraphAndRef {
  grGraph :: SyntaxGraph
  , grRef :: Reference
  }

type ColorStyle = ColorStyle' NumericType
data ColorStyle' a = ColorStyle {
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

$(makeLenses ''Labeled)
$(makeLenses ''Named)