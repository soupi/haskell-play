{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}

module Play.Collisions where

import qualified Lens.Micro as Lens
import           Lens.Micro ((^.))

import           Play.Types

----------------
-- Collisions
----------------

testCollisionWith :: (HasCollision a CollisionComponent, HasPosition a PositionComponent
                     ,HasCollision b CollisionComponent, HasPosition b PositionComponent)
                  => [a] -> [b] -> [a]
testCollisionWith objs1 objs2 =
  (\o -> Lens.set (collision . collided) (foldl addCollisions Nothing $ fmap (collisionDetection o) objs2) o) <$> objs1

collisionDetection :: (HasCollision a CollisionComponent, HasPosition a PositionComponent
                      ,HasCollision b CollisionComponent, HasPosition b PositionComponent)
                    => a -> b -> Maybe Point
collisionDetection a b =
  let bData = (b ^. position . pos, b ^. position . size)
      aData = (a ^. position . pos, a ^. position . size)
  in
    if testCollision aData bData then
       collisionDirection a b
    else Nothing

testCollision :: (Point, Size)
              -> (Point, Size)
              -> Bool
testCollision (Point ax ay, Size aw ah) (Point bx by, Size bw bh) =
  not $
     ax >= bx +  bw
  || ay >= by +  bh
  || ax +  aw <= bx
  || ay +  ah <= by

cornerRects :: (Point, Size) -> [(Point, Size)]
cornerRects (Point ox oy, Size ow oh) =
  let size' = Size (ow `div` 2) (oh `div` 2)
  in
    [ (Point ox oy, size')
    , (Point (ox + ow `div` 2) oy, size')
    , (Point ox (oy + oh `div` 2), size')
    , (Point (ox + oh `div` 2) (oy + oh `div` 2), size')
    ]

corners :: (Point, Size) -> [Point]
corners (Point ox oy, Size ow oh) =
  [ Point ox oy
  , Point (ox + ow) oy
  , Point ox (oy + oh)
  , Point (ox + ow) (oy + oh)
  ]

pointInRect :: Point
            -> (Point, Size)
            -> Bool
pointInRect (Point px py) (Point ox oy, Size ow oh) =
     (ox <= px && px <= ox + ow)
  && (oy <= py && py <= oy + oh)


collisionDirection :: (HasCollision a CollisionComponent, HasPosition a PositionComponent
                      ,HasCollision b CollisionComponent, HasPosition b PositionComponent)
                    => a -> b -> Maybe Point
collisionDirection a b =
  let bData = (b ^. position . pos, b ^. position . size)
      aData = (a ^. position . pos, a ^. position . size)
  in
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

