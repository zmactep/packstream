{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.PackStream.Structure
( ToStructure (..), FromStructure (..)
, Node (..), Relationship (..), UnboundRelationship (..), Path (..)
, Date (..), Time (..), LocalTime (..), DateTime (..), DateTimeZoneId (..), LocalDateTime (..)
, Duration (..)
, Point2D (..), Point3D (..)
) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Control.Monad.Except (MonadError(..))
import Control.Monad ((>=>))

import Data.PackStream.Internal.Type
import Data.PackStream ( PackStreamValue(..) )

-- * Structure coverters
-- 
-- 'PackStream' protocol provides several built-in 'Structure' types. They
-- are the objects with specific fields and one-byte structure signature.
-- The 'ToStructure' and 'FromStructure' typeclasses help to convert
-- Haskell representation of these objects into and from generic 'Structure'
-- type.

-- |The set of types, that can be presented as 'PackStream' 'Structure's
class ToStructure a where
    -- |Convert object to 'Structure'
    toStructure :: a -> Structure

-- |The set of types, that can be parsed from 'PackStream' 'Structure's
class FromStructure a where
    -- |Convert 'Structure' to the object of selected type
    fromStructure :: Structure -> Either PackStreamError a

-- * Built-in structure types

-- |Snapshot of a node within a graph database
data Node = Node { nodeId    :: Int            -- ^Node identifier
                 , labels    :: [Text]         -- ^List of node labels
                 , nodeProps :: Map Text Value -- ^Dict of node properties
                 }
  deriving (Show, Eq)

instance ToStructure Node where
    toStructure Node{..} = Structure 0x4E [ toValue nodeId
                                          , toValue labels
                                          , toValue nodeProps
                                          ]

instance FromStructure Node where
    fromStructure (Structure 0x4E [I nid, L lbls, D nps]) = Node nid <$> traverse fromValue lbls <*> pure nps
    fromStructure _                                       = throwError $ WrongStructure "Node"

-- |Snapshot of a relationship within a graph database
data Relationship = Relationship { relId       :: Int            -- ^Relationship identifier
                                 , startNodeId :: Int            -- ^Identifier of the start node
                                 , endNodeId   :: Int            -- ^Identifier of the end node
                                 , relType     :: Text           -- ^Relationship type
                                 , relProps    :: Map Text Value -- ^Dict of relationship properties
                                 }
  deriving (Show, Eq)

instance ToStructure Relationship where
    toStructure Relationship{..} = Structure 0x52 [ toValue relId
                                                  , toValue startNodeId
                                                  , toValue endNodeId
                                                  , toValue relType
                                                  , toValue relProps
                                                  ]

instance FromStructure Relationship where
    fromStructure (Structure 0x52 [I rid, I snid, I enid, T rt, D rps]) = pure $ Relationship rid snid enid rt rps
    fromStructure _                                                     = throwError $ WrongStructure "Relationship"

-- |Relationship detail without start or end node information
data UnboundRelationship = UnboundRelationship { urelId    :: Int            -- ^Relationship identifier
                                               , urelType  :: Text           -- ^Relationship type
                                               , urelProps :: Map Text Value -- ^Dict of relationship properties
                                               }
  deriving (Show, Eq)

instance ToStructure UnboundRelationship where
    toStructure UnboundRelationship{..} = Structure 0x72 [ toValue urelId
                                                         , toValue urelType
                                                         , toValue urelProps
                                                         ]

instance FromStructure UnboundRelationship where
    fromStructure (Structure 0x72 [I rid, T rt, D rps]) = pure $ UnboundRelationship rid rt rps
    fromStructure _                                     = throwError $ WrongStructure "UnboundRelationship"

-- |Alternating sequence of nodes and relationships
data Path = Path { nodes :: [Node]                -- ^Chain of 'Node's in path
                 , rels  :: [UnboundRelationship] -- ^Chain of 'UnboundRelationship's in path
                 , ids   :: [Int]                 -- ^The ids is a list of relationship id and node id to represent the path
                 }
  deriving (Show, Eq)

instance ToStructure Path where
    toStructure Path{..} = Structure 0x50 [ toValue $ fmap toStructure nodes
                                          , toValue $ fmap toStructure rels
                                          , toValue ids
                                          ]

instance FromStructure Path where
    fromStructure (Structure 0x50 [L nds, L rls, L is]) = Path <$> traverse (fromValue >=> fromStructure) nds 
                                                               <*> traverse (fromValue >=> fromStructure) rls 
                                                               <*> traverse fromValue is

-- |The days are days since the Unix epoch
newtype Date = Date { days :: Int -- ^The days are days since the Unix epoch
                    }
  deriving (Show, Eq)

instance ToStructure Date where
  toStructure Date{..} = Structure 0x44 [toValue days]

instance FromStructure Date where
  fromStructure (Structure 0x44 [I ds]) = pure $ Date ds
  fromStructure _                       = throwError $ WrongStructure "Date"

data Time
data LocalTime
data DateTime
data DateTimeZoneId
data LocalDateTime
data Duration
data Point2D
data Point3D