{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Play.Types where

import Control.Lens (makeFields, makeLenses)

-----------
-- Types --
-----------

data Point = Point { _x :: Int, _y :: Int } deriving (Show, Read, Eq, Ord)
data Size  = Size  { _w :: Int, _h :: Int } deriving (Show, Read, Eq, Ord)

data PositionComponent = PositionComponent
  { _pos  :: Point
  , _size :: Size
  } deriving (Show, Read, Eq, Ord)

data MovementComponent = MovementComponent
  { _direction :: Point
  , _speed :: Int
  } deriving (Show, Read, Eq, Ord)

data CollisionComponent = CollisionComponent
  { _collided :: Maybe Point
  } deriving (Show, Read, Eq, Ord)
  
data GameObj = GameObj
  { gameObjPosition  :: PositionComponent
  , gameObjMovement  :: MovementComponent
  , gameObjCollision :: CollisionComponent
  } deriving (Show, Read, Eq, Ord)

data Screen = Screen
  { screenPosition  :: PositionComponent
  , screenCollision :: CollisionComponent
  } deriving (Show, Read, Eq, Ord)

makeLenses ''PositionComponent
makeLenses ''MovementComponent
makeLenses ''CollisionComponent
makeLenses ''Point
makeLenses ''Size
makeFields ''GameObj
makeFields ''Screen

pointToTuple :: Point -> (Int, Int)
pointToTuple (Point x_ y_) = (x_, y_)
sizeToTuple :: Size -> (Int, Int)
sizeToTuple (Size w_ h_) = (w_, h_)

