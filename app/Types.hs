{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Types (
  Named(..)
  , NamedIcon
  , IconInfo
  , Icon(..)
  , SyntaxNode(..)
  , NodeName(..)
  , Port(..)
  , NameAndPort(..)
  , Connection
  , Edge(..)
  , EdgeOption(..)
  , Drawing(..)
  , IDState(..)
  , SpecialDiagram
  , SpecialQDiagram
  , SpecialBackend
  
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
  , TransformParams(..)
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
) where

import Diagrams.Prelude(QDiagram, V2, Any, Renderable, Path, IsName, Point)
import Diagrams.TwoD.Text(Text)
import Control.Applicative(Applicative(..))
import qualified Data.Graph.Inductive as ING
import qualified Data.Graph.Inductive.PatriciaTree as FGR
import qualified Data.IntMap as IMap
import qualified Data.Map as SMap
import qualified Data.Set as Set
import Data.Typeable(Typeable)
import qualified Language.Haskell.Exts as Exts


newtype NodeName = NodeName Int deriving (Typeable, Eq, Ord, Show)
instance IsName NodeName

data Named a = Named {
  naName :: NodeName
  , naVal :: a}
  deriving (Show, Eq, Ord, Functor)

type NamedIcon = Named Icon

data Labeled a = Labeled {
  laValue :: a
  , laLabel :: String}
  deriving (Show, Eq, Ord)

instance Functor Labeled where
  fmap f (Labeled value str) = Labeled (f value) str

instance Applicative Labeled where
  pure x = Labeled x ""
  (Labeled f fStr) <*> (Labeled x xStr) = Labeled (f x) (fStr <> xStr)

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
  | FunctionDefIcon
    String  -- Function name
    FuncDefRegionInfo
    (Maybe NodeName) -- embeded body node
  | BindTextBoxIcon String
  | NestedApply
    ApplyFlavor  -- apply or compose
    (Maybe NodeName)  -- The function for apply, or the argument for compose
    [Maybe NodeName]  -- list of arguments or functions
  | NestedPatternApp
    (Labeled (Maybe NamedIcon))  -- Data constructor
    [Labeled (Maybe NamedIcon)]  -- Arguments
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

-- TODO remove Ints from SyntaxNode data constructors.
data SyntaxNode = SyntaxNode {
  syntaxNodeCore :: SyntaxNodeCore
  , srcRef :: SrcRef
  } 
  deriving (Show, Eq, Ord)

data SyntaxNodeCore = 
  -- Function application, composition, and applying to a composition
  -- The list of nodes is unordered (replace with a map?)
  ApplyNode ApplyFlavor Int
  | PatternApplyNode String [Labeled (Maybe SgNamedNode)]
  | BindNameNode String -- for top level bindings --TODO delete this
  | LiteralNode String -- Literal values like the string "Hello World"
  | FunctionArgNode
    [String]  -- Parameter labels
  | FunctionValueNode  -- Function definition (ie. lambda expression)
    String -- function name
    FuncDefRegionInfo
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

data NameAndPort = NameAndPort NodeName Port deriving (Show, Eq, Ord)

type Connection = (NameAndPort, NameAndPort)

-- TODO Consider removing EdgeOption since it's unused.
data EdgeOption =
  DrawAndConstraint
  | DrawAndNotConstraint
  | DoNotDrawButConstraint deriving (Show, Eq, Ord)

-- | An Edge has an name of the source icon, and its optional port number,
-- and the name of the destination icon, and its optional port number.
data Edge = Edge { 
  edgeOption :: EdgeOption
  , edgeConnection :: Connection
  }
  deriving (Show, Eq, Ord)

-- | A drawing is a map from names to Icons, a list of edges,
-- and a map of names to subDrawings
data Drawing = Drawing [NamedIcon] (Set.Set Edge) deriving (Show, Eq)

-- | IDState is an Abstract Data Type that is used as a state whose value is a
-- unique id.
newtype IDState = IDState Int deriving (Eq, Show)

type NumericType = Double 

type PointType = Point V2 NumericType

-- Note that SpecialBackend is a constraint synonym, not a type synonym.
type SpecialBackend b
  = (Renderable (Path V2 NumericType) b, Renderable (Text NumericType) b)

type SpecialDiagram b = QDiagram b V2 NumericType Any

type SpecialQDiagram b = QDiagram b V2 NumericType DiaQuery

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

data TransformParams n = TransformParams {
  tpName :: NodeName  -- The icon's name
  , tpNestingDepth :: Int  -- The icon's nesting depth
  -- , tpIsReflected :: Bool  -- If the icon will be reflected
  -- , tpAngle :: Angle NumericType  -- By what angle will the icon be rotated
  }

-- | A TransformableDia is a function that returns a diagram for an icon when
-- given the icon's name, its nesting depth, whether it will be reflected, and
-- by what angle it will be rotated.
type TransformableDia b n = TransformParams n -> SpecialDiagram b

data NodeQueryValue = NodeQueryValue {
  nodeSrcRef :: SrcRef
  , nodeName :: NodeName
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
  -- TODO change to SMap.StringMap NameAndPort
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
  graph :: SyntaxGraph
  , ref :: Reference
  }
