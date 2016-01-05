{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Play.Types where

import qualified Control.Lens     as Lens
import           Control.Lens (makeLenses, ALens', (^.))

-----------
-- Types --
-----------

data Point = Point { _x :: Int, _y :: Int } deriving (Show, Read, Eq, Ord)
data Size  = Size { _w :: Int, _h :: Int } deriving (Show, Read, Eq, Ord)

data GameObj s =
  GameObj
    { _movement  :: MovementComponent s
    , _collision :: CollisionComponent s
    , _state :: s
    }

data MovementComponent s =
  MovementComponent
    { _position :: ALens' s Point
    , _size :: ALens' s Size
    }

data CollisionComponent s =
  CollisionComponent
    { _collided :: ALens' s (Maybe Point)
    }

makeLenses ''MovementComponent
makeLenses ''CollisionComponent
makeLenses ''GameObj
makeLenses ''Point
makeLenses ''Size

sizeOf :: GameObj s -> Lens.Lens' (GameObj s) Size
sizeOf obj = state . Lens.cloneLens (obj ^. movement . size)

posOf :: GameObj s -> Lens.Lens' (GameObj s) Point
posOf obj = state . Lens.cloneLens (obj ^. movement . position)

collidedOf :: GameObj s -> Lens.Lens' (GameObj s) (Maybe Point)
collidedOf obj = state . Lens.cloneLens (obj ^. collision . collided)

pointToTuple :: Point -> (Int, Int)
pointToTuple (Point x_ y_) = (x_, y_)
sizeToTuple :: Size -> (Int, Int)
sizeToTuple (Size w_ h_) = (w_, h_)

