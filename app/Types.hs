{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Types (
  Icon(..),
  NameAndPort(..),
  Connection(..),
  Edge(..),
  EdgeEnd(..),
  Drawing(..),
  IDState,
  initialIdState,
  getId
) where

import Diagrams.Prelude(Name)
import Control.Monad.State(State, state)

-- TYPES --
-- | A datatype that represents an icon.
-- The BranchIcon is used as a branching point for a line.
-- The TextBoxIcon's data is the text that appears in the text box.
-- The LambdaRegionIcon's data is the number of lambda ports, and the name of it's
-- subdrawing.
data Icon = Apply0Icon | ResultIcon | BranchIcon | TextBoxIcon String | GuardIcon Int
  | LambdaRegionIcon Int Name | Apply0NIcon Int
  deriving (Show)

data NameAndPort = NameAndPort Name (Maybe Int) deriving (Show)

type Connection = (NameAndPort, NameAndPort)

-- | An Edge has an name of the source icon, and its optional port number,
-- and the name of the destination icon, and its optional port number.
data Edge = Edge {edgeConnection :: Connection, edgeEnds :: (EdgeEnd, EdgeEnd)}
  deriving (Show)

data EdgeEnd = EndAp1Result | EndAp1Arg | EndNone deriving (Show)

-- | A drawing is a map from names to Icons, a list of edges,
-- and a map of names to subDrawings
data Drawing = Drawing [(Name, Icon)] [Edge] [(Name, Drawing)] deriving (Show)

-- | IDState is an Abstract Data Type that is used as a state whose value is a unique id.
newtype IDState = IDState Int deriving (Eq, Show)

initialIdState :: IDState
initialIdState = IDState 0

getId :: State IDState Int
getId = state (\(IDState x) -> (x, IDState (x + 1)))