{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Play.Types where

import qualified Control.Lens     as Lens
import           Control.Lens (makeLenses, ALens', (^.), (^?))

-----------
-- Types --
-----------

data Point = Point { _x :: Int, _y :: Int } deriving (Show, Read, Eq, Ord)
data Size  = Size { _w :: Int, _h :: Int } deriving (Show, Read, Eq, Ord)

data GameObj s =
  GameObj
    { _position  :: PositionComponent s
    , _movement  :: MovementComponent s
    , _collision :: CollisionComponent s
    , _state :: s
    }

data PositionComponent s =
  PositionComponent
    { _pos  :: ALens' s Point
    , _size :: ALens' s Size
    }

data MovementComponent s =
  MovementComponent
    { _direction :: ALens' s Point
    , _speed :: ALens' s Int
    }

data CollisionComponent s =
  CollisionComponent
    { _collided :: ALens' s (Maybe Point)
    }

makeLenses ''PositionComponent
makeLenses ''MovementComponent
makeLenses ''CollisionComponent
makeLenses ''GameObj
makeLenses ''Point
makeLenses ''Size


sizeOf :: GameObj s -> Lens.Lens' (GameObj s) Size
sizeOf obj =
  state . Lens.cloneLens (obj ^. position . size)

posOf :: GameObj s -> Lens.Lens' (GameObj s) Point
posOf obj  = state . Lens.cloneLens (obj ^. position . pos)

speedOf :: GameObj s -> Lens.Lens' (GameObj s) Int
speedOf obj = state . Lens.cloneLens (obj ^. movement . speed)

directionOf :: GameObj s -> Lens.Lens' (GameObj s) Point
directionOf obj = state . Lens.cloneLens (obj ^. movement . direction)

collidedOf :: GameObj s -> Lens.Lens' (GameObj s) (Maybe Point)
collidedOf obj = state . Lens.cloneLens (obj ^. collision . collided)

pointToTuple :: Point -> (Int, Int)
pointToTuple (Point x_ y_) = (x_, y_)
sizeToTuple :: Size -> (Int, Int)
sizeToTuple (Size w_ h_) = (w_, h_)

