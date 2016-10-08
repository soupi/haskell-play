{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Play.Movement where

import qualified Control.Lens as Lens
import           Control.Lens ((^.))

import Play.Types
import Play.Utils
import Play.Collisions

-----------
-- Types --
-----------

move
  :: (HasPosition a PositionComponent, HasMovement a MovementComponent, HasCollision a CollisionComponent)
  => a -> a
move obj =
  case obj ^. collision . collided of
    Nothing ->
      let spd = obj ^. movement . speed
          dir = (obj ^. movement . direction) `mulPoint` Point spd spd
      in Lens.over (position . pos) (`addPoint` dir) obj
    Just _ ->
      undoCollision obj

checkScreenBounds :: Screen -> GameObj -> (Bool, GameObj)
checkScreenBounds closing obj =
  let (Point x_ y_) = obj ^. position . pos
      (Size  w_ h_) = obj ^. position . size
      (Point bx by) = closing ^. position . pos
      (Size  bw bh) = closing ^. position . size
  in
  if | x_ < bx -> (,) True $ Lens.set (position . pos . x) bx obj
     | x_ + w_ > bx + bw -> (,) True $ Lens.set (position . pos . x) (bw - w_) obj
     | y_ < by -> (,) True $ Lens.set (position . pos . y) by obj
     | y_ + h_ > by + bh -> (,) True $ Lens.set (position . pos . y) (bh - h_) obj
     | otherwise -> (False, obj)
