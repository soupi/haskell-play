{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}

module Play.Collisions where

import qualified Control.Lens as Lens
import           Control.Lens ((^.))

import Play.Types
import Play.Utils

----------------
-- Collisions
----------------

posAndColl
  :: (HasCollision a CollisionComponent, HasPosition a PositionComponent)
  => a -> (PositionComponent, CollisionComponent)
posAndColl obj = (obj ^. position, obj ^. collision)

testCollisionWith
  :: (HasCollision a CollisionComponent, HasPosition a PositionComponent)
  => [a] -> [(PositionComponent, CollisionComponent)] -> [a]
testCollisionWith objs1 objs2 =
  (\o -> Lens.set (collision . collided) (foldl addCollisions Nothing $ fmap (collisionDetection o) objs2) o) <$> objs1

collisionDetection
  :: (HasPosition a PositionComponent)
  => a -> (PositionComponent, CollisionComponent) -> Maybe Point
collisionDetection a (bData,_) =
  let aData = a ^. position
  in
    collisionDirection aData bData

testCollision
  :: PositionComponent
  -> PositionComponent
  -> Bool
testCollision (PositionComponent (Point ax ay) (Size aw ah)) (PositionComponent (Point bx by) (Size bw bh)) =
  not $
     ax >= bx +  bw
  || ay >= by +  bh
  || ax +  aw <= bx
  || ay +  ah <= by

cornerRects :: PositionComponent -> [PositionComponent]
cornerRects (PositionComponent (Point ox oy) (Size ow oh)) =
  let size' = Size (ow `div` 2) (oh `div` 2)
  in
    [ PositionComponent (Point ox oy) size'
    , PositionComponent (Point (ox + ow `div` 2) oy) size'
    , PositionComponent (Point ox (oy + oh `div` 2)) size'
    , PositionComponent (Point (ox + oh `div` 2) (oy + oh `div` 2)) size'
    ]

corners :: (Point, Size) -> [Point]
corners (Point ox oy, Size ow oh) =
  [ Point ox oy
  , Point (ox + ow) oy
  , Point ox (oy + oh)
  , Point (ox + ow) (oy + oh)
  ]

pointInRect
  :: Point
  -> (Point, Size)
  -> Bool
pointInRect (Point px py) (Point ox oy, Size ow oh) =
     (ox <= px && px <= ox + ow)
  && (oy <= py && py <= oy + oh)


collisionDirection :: PositionComponent -> PositionComponent -> Maybe Point
collisionDirection aData bData =
  foldl addCollisions Nothing $
    zipWith (\result test -> if test then Just result else Nothing)
            [Point (-1) (-1), Point 1 (-1), Point (-1) 1, Point 1 1]
            (map (testCollision bData) (cornerRects aData))

addCollisions :: Maybe Point -> Maybe Point -> Maybe Point
addCollisions Nothing a = a
addCollisions a Nothing = a
addCollisions (Just (Point ax ay)) (Just (Point bx by)) =
  let x_ = if | ax == 0   -> 0
              | ax == bx  -> ax
              | otherwise -> ax + bx
      y_ = if | ay == 0   -> 0
              | ay == by  -> ay
              | otherwise -> ay + by
  in
      Just $ Point x_ y_


undoCollision
  :: (HasPosition a PositionComponent, HasMovement a MovementComponent, HasCollision a CollisionComponent)
  => a -> a
undoCollision obj =
  case obj^.collision . collided of
    Nothing ->
      obj
    Just dir ->
      let spd  = obj ^. movement . speed
          dir' = dir `mulPoint` Point (-spd) (-spd)
      in Lens.over (position . pos) (`addPoint` dir') obj
