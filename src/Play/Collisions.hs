{-# LANGUAGE MultiWayIf #-}

module Play.Collisions where

import qualified Control.Lens     as Lens
import           Control.Lens ((^.))

import           Play.Types

----------------
-- Collisions
----------------


testCollisionWith :: [GameObj a]
                  -> [GameObj b]
                  -> [GameObj a]
testCollisionWith objs1 objs2 =
  fmap (\o -> Lens.set (collidedOf o) (foldl addCollisions Nothing $ fmap (collisionDetection o) objs2) o) objs1

collisionDetection :: GameObj a
          -> GameObj b
          -> Maybe Point
collisionDetection a b =
  if testCollision a (b ^. posOf b, b ^. sizeOf b) then collisionDirection a b else Nothing

testCollision :: GameObj a
              -> (Point, Size)
              -> Bool
testCollision a b =
  not $
      a ^. posOf a . x >= b ^. Lens._1  . x +  b ^. Lens._2 . w
   || a ^. posOf a . y >= b ^. Lens._1  . y +  b ^. Lens._2 . h
   || a ^. posOf a . x +  a ^. sizeOf a . w <= b ^. Lens._1 . x
   || a ^. posOf a . y +  a ^. sizeOf a . h <= b ^. Lens._1 . y

cornerRects :: GameObj a -> [(Point, Size)]
cornerRects obj =
  let size' = Size (obj ^. sizeOf obj . w `div` 2) (obj ^. sizeOf obj . h `div` 2)
  in
    [ (Point (obj^.posOf obj.x) (obj^.posOf obj.y), size')
    , (Point (obj^.posOf obj.x + obj^.sizeOf obj.w `div` 2) (obj^.posOf obj.y), size')
    , (Point (obj^.posOf obj.x) (obj^.posOf obj.y + obj^.sizeOf obj.h `div` 2), size')
    , (Point (obj^.posOf obj.x + obj^.sizeOf obj.h `div` 2) (obj^.posOf obj.y + obj^.sizeOf obj.h `div` 2), size')
    ]

corners :: GameObj a -> [Point]
corners obj =
  [ Point (obj^.posOf obj.x) (obj^.posOf obj.y)
  , Point (obj^.posOf obj.x + obj^.sizeOf obj.w) (obj^.posOf obj.y)
  , Point (obj^.posOf obj.x) (obj^.posOf obj.y + obj^.sizeOf obj.h)
  , Point (obj^.posOf obj.x + obj^.sizeOf obj.w) (obj^.posOf obj.y + obj^.sizeOf obj.h)
  ]

pointInRect :: Point
            -> GameObj a
            -> Bool
pointInRect (Point px py) obj =
     (obj^.posOf obj.x <= px && px <= obj^.posOf obj.x + obj^.sizeOf obj.w)
  && (obj^.posOf obj.y <= py && py <= obj^.posOf obj.y + obj^.sizeOf obj.h)


collisionDirection :: GameObj a
                   -> GameObj b
                   -> Maybe Point
collisionDirection a b =
  foldl addCollisions Nothing $
  zipWith (\result test -> if test then Just result else Nothing)
          [Point (-1) (-1), Point 1 (-1), Point (-1) 1, Point 1 1]
          (map (testCollision b) (cornerRects a))

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

