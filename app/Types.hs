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
  , SpecialNum
  , SgNamedNode
  , IngSyntaxGraph
  , LikeApplyFlavor(..)
  , CaseOrMultiIfTag(..)
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
  , NameQuery(..)
) where

import Diagrams.Prelude(QDiagram, V2, Any, Renderable, Path, IsName)
import Diagrams.TwoD.Text(Text)
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVA
import Control.Applicative(Applicative(..))
import qualified Data.Graph.Inductive as ING
import qualified Data.IntMap as IMap
import qualified Data.Set as Set
import Data.Typeable(Typeable)

newtype NodeName = NodeName Int deriving (Typeable, Eq, Ord, Show)
instance IsName NodeName

data Named a = Named {naName :: NodeName, naVal :: a}
  deriving (Show, Eq, Ord, Functor)

type NamedIcon = Named Icon

data Labeled a = Labeled {laValue :: a, laLabel :: String}
  deriving (Show, Eq, Ord)

instance Functor Labeled where
  fmap f (Labeled value str) = Labeled (f value) str

instance Applicative Labeled where
  pure x = Labeled x ""
  (Labeled f fStr) <*> (Labeled x xStr) = Labeled (f x) (fStr <> xStr)

type IconInfo = IMap.IntMap Icon

-- | A datatype that represents an icon.
-- The TextBoxIcon's data is the text that appears in the text box.
data Icon = TextBoxIcon String
  | MultiIfIcon
    Int  -- Number of alternatives
  | FunctionArgIcon
    [String]  -- Parameter labels
  | FunctionDefIcon
    String  -- Function name
    (Set.Set NodeName)  -- Nodes inside the lambda
    (Maybe NodeName) -- embeded body node
  | CaseIcon Int
  | CaseResultIcon
  | BindTextBoxIcon String
  | NestedApply
    LikeApplyFlavor  -- apply or compose
    (Maybe NodeName)  -- The function for apply, or the argument for compose
    [Maybe NodeName]  -- list of arguments or functions
  | NestedPatternApp
    (Labeled (Maybe NamedIcon))  -- Data constructor
    [Labeled (Maybe NamedIcon)]  -- Arguments
    (Maybe NodeName)  -- asigned value
  | NestedCaseIcon [Maybe NodeName]
  | NestedMultiIfIcon [Maybe NodeName]
  | ListCompIcon
  deriving (Show, Eq, Ord)

data LikeApplyFlavor = ApplyNodeFlavor | ComposeNodeFlavor
  deriving (Show, Eq, Ord)

data CaseOrMultiIfTag = CaseTag | MultiIfTag deriving (Show, Eq, Ord)

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
data SyntaxNode =
  -- Function application, composition, and applying to a composition
  -- The list of nodes is unordered (replace with a map?)
  ApplyNode LikeApplyFlavor Int
  | PatternApplyNode String [Labeled (Maybe SgNamedNode)]
  | NameNode String -- Identifiers or symbols
  | BindNameNode String -- for top level bindings
  | LiteralNode String -- Literal values like the string "Hello World"
  | FunctionArgNode
    [String]  -- Parameter labels
  | FunctionValueNode  -- Function definition (ie. lambda expression)
    String -- function name
    (Set.Set NodeName)  -- Nodes inside the lambda
  | CaseResultNode
  | CaseOrMultiIfNode CaseOrMultiIfTag Int
  | ListCompNode
  deriving (Show, Eq, Ord)

newtype Port = Port Int deriving (Typeable, Eq, Ord, Show)
instance IsName Port

data NameAndPort = NameAndPort NodeName (Maybe Port) deriving (Show, Eq, Ord)

type Connection = (NameAndPort, NameAndPort)

-- TODO Consider removing EdgeOption since it's unused.
data EdgeOption =
  DrawAndConstraint
  | DrawAndNotConstraint
  | DoNotDrawButConstraint deriving (Show, Eq, Ord)

-- | An Edge has an name of the source icon, and its optional port number,
-- and the name of the destination icon, and its optional port number.
data Edge = Edge { edgeOption :: EdgeOption
                 , edgeConnection :: Connection}
  deriving (Show, Eq, Ord)

-- | A drawing is a map from names to Icons, a list of edges,
-- and a map of names to subDrawings
data Drawing = Drawing [NamedIcon] (Set.Set Edge) deriving (Show, Eq)

-- | IDState is an Abstract Data Type that is used as a state whose value is a
-- unique id.
newtype IDState = IDState Int deriving (Eq, Show)

type SpecialNum n
  = (Floating n, RealFrac n, RealFloat n, Typeable n, Show n, Enum n)

-- Note that SpecialBackend is a constraint synonym, not a type synonym.
type SpecialBackend b n
  = (SpecialNum n, Renderable (Path V2 n) b, Renderable (Text n) b)

type SpecialDiagram b n = QDiagram b V2 n Any

type SpecialQDiagram b n = QDiagram b V2 n NameQuery

type IngSyntaxGraph gr = gr SgNamedNode Edge

data EmbedDirection =
  EdEmbedFrom -- The tail
  | EdEmbedTo -- The head
  deriving (Show, Eq)

-- A Nothing eiEmbedDir means the edge is not embedded.
data EmbedInfo a = EmbedInfo {eiEmbedDir :: Maybe EmbedDirection, eiVal :: a}
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
  -- , tpAngle :: Angle n  -- By what angle will the icon be rotated
  }

-- | A TransformableDia is a function that returns a diagram for an icon when
-- given the icon's name, its nesting depth, whether it will be reflected, and
-- by what angle it will be rotated.
type TransformableDia b n = TransformParams n -> SpecialDiagram b n

newtype NameQuery = NameQuery ( Set.Set NodeName ) deriving (Show, Eq)

instance Semigroup NameQuery where
  (<>) (NameQuery l) (NameQuery r) = NameQuery $ Set.union l r

instance Monoid NameQuery where
  mempty = NameQuery Set.empty
  mappend = (<>)