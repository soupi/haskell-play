{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Play.Run where

import           Data.Maybe (fromMaybe, isJust)
import           Control.Monad.IO.Class (MonadIO)
import qualified SDL
import qualified Data.Vector.Storable as VS
import qualified Foreign.C.Types as C (CInt)
import qualified Linear
import qualified Linear.Affine as Linear
import qualified Lens.Micro as Lens
import           Lens.Micro ((&), (^.))
import           Lens.Micro.TH (makeLenses, makeFields)

import qualified MySDL.MySDL as MySDL
import qualified MySDL.Run   as MySDL
import           Play.Types
import           Play.Collisions


-----------
-- Types --
-----------


data GameState =
  GameState
    { _screen  :: Screen
    , _playerA :: GameObj
    , _playerB :: GameObj
    , _ball :: Ball
    }

type Ball = GameObj

data Screen =
  Screen
    { screenPosition  :: PositionComponent
    , screenCollision :: CollisionComponent
    } deriving (Show, Read, Eq, Ord)


makeFields ''Screen
makeLenses ''GameState

type Keys = [(Key, Bool)]

data Key =
  KeyUp | KeyDown | KeyLeft | KeyRight
  deriving (Show, Read, Eq)

keyMapping :: [(Key, SDL.Scancode)]
keyMapping =
  [(KeyUp, SDL.ScancodeW)
  ,(KeyLeft, SDL.ScancodeA)
  ,(KeyDown, SDL.ScancodeS)
  ,(KeyRight, SDL.ScancodeD)
  ]

----------
-- Glue --
----------

main :: IO ()
main = MySDL.main initWorld

initWorld :: Monad m => m (MySDL.WorldInterface Keys GameState, GameState)
initWorld = pure (worldInterface, initGame)

initGame :: GameState
initGame =
  GameState
    { _screen  = initScreen
    , _playerA = initPlayerA
    , _playerB = initPlayerB
    , _ball    = initBall
    }

initScreen :: Screen
initScreen =
  Screen
    (PositionComponent (Point 0 0) (Size 800 600))
    (CollisionComponent Nothing)


initBall :: Ball
initBall =
  GameObj
    (PositionComponent (Point 390 290) (Size 20 20))
    (MovementComponent (Point 0 1) 3)
    (CollisionComponent Nothing)

initPlayerA :: GameObj
initPlayerA =
  GameObj
    (PositionComponent (Point 350 50) (Size 100 30))
    (MovementComponent (Point 0 0) 3)
    (CollisionComponent Nothing)

initPlayerB :: GameObj
initPlayerB =
  GameObj
    (PositionComponent (Point 350 520) (Size 100 30))
    (MovementComponent (Point 0 0) 3)
    (CollisionComponent Nothing)

worldInterface :: MySDL.WorldInterface Keys GameState
worldInterface =
  MySDL.WorldInterface makeEvents update render

---------------
-- Interface --
---------------

makeEvents :: [SDL.EventPayload] -> (SDL.Scancode -> Bool) -> Keys
makeEvents _ keyPressed =
  fmap (fmap keyPressed) keyMapping

testKey :: Key -> Keys -> Bool
testKey key = fromMaybe False . lookup key

keysToMovement :: Keys -> Point
keysToMovement keys =
  let
      singleMove k1 k2
        | testKey k1 keys && not (testKey k2 keys) = -1
        | testKey k2 keys && not (testKey k1 keys) =  1
        | otherwise = 0
      hori = singleMove KeyUp KeyDown
      vert = singleMove KeyLeft KeyRight
  in Point vert hori

update :: MonadIO m => Keys -> MySDL.World GameState ->  m (Either (Maybe String) (MySDL.World GameState))
update keys (sets, state) =
  let mov = keysToMovement keys
      nState =
        collisionLayers state
          & Lens.over (playerA . position . pos) (`addPoint` mov)
          & \s -> Lens.over ball (updateBall s) s
          & collisionLayers
          & undoCollisions
  in  (pure . pure) (sets, nState)

addPoint :: Point -> Point -> Point
addPoint = apToPoint (+)

mulPoint :: Point -> Point -> Point
mulPoint = apToPoint (*)

apToPoint :: (Int -> Int -> Int) -> Point -> Point -> Point
apToPoint f p1 p2 =
  p1 & Lens.over x (f (p2 ^. x))
     & Lens.over y (f (p2 ^. y))

collisionLayers :: GameState -> GameState
collisionLayers s =
  s & \state -> Lens.over playerA (head . (`testCollisionWith` [posAndColl $ state ^. playerB]) . (:[])) state
    & \state -> Lens.over playerB (head . (`testCollisionWith` [posAndColl $ state ^. playerA]) . (:[])) state
    & \state -> Lens.over ball    (head . (`testCollisionWith` [posAndColl $ state ^. playerA, posAndColl $ state ^. playerB]) . (:[])) state

undoCollisions :: GameState -> GameState
undoCollisions state =
  state & Lens.over playerA undoCollision
        & Lens.over playerB undoCollision
        & Lens.over ball    undoCollision

move :: (HasPosition a PositionComponent, HasMovement a MovementComponent, HasCollision a CollisionComponent)
     => a -> a
move obj =
  case obj^.collision . collided of
    Nothing ->
      let spd = obj ^. movement . speed
          dir = (obj ^. movement . direction) `mulPoint` Point spd spd
      in Lens.over (position . pos) (`addPoint` dir) obj
    Just _ ->
      undoCollision obj

undoCollision :: (HasPosition a PositionComponent, HasMovement a MovementComponent, HasCollision a CollisionComponent)
              => a -> a
undoCollision obj =
  case obj^.collision . collided of
    Nothing ->
      obj
    Just dir ->
      let spd  = obj ^. movement . speed
          dir' = dir `mulPoint` Point (-spd) (-spd)
      in Lens.over (position . pos) (`addPoint` dir') obj

updateBall :: GameState -> Ball -> Ball
updateBall state obj =
  case checkScreenBounds (state ^. screen) $ move obj of
    (True, ball') ->
      Lens.over (movement . direction . y) (* (-1)) ball'
    (False, ball') ->
      case testCollisionWith [ball'] [state ^. playerA & posAndColl, state ^. playerB & posAndColl] of
        [ball''] ->
          if isJust (ball'' ^. collision . collided) then
            Lens.over (movement . direction . y) (* (-1)) ball'
          else
            ball''
        _ -> error "unexpected logic error when testing collision of ball"

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

-- render

render :: MonadIO m => SDL.Renderer -> MySDL.World GameState -> m ()
render renderer (_, GameState _ pA pB b) = do
  let rects = VS.fromList $ fmap toRect [pA, pB] ++ [toRect b]
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  SDL.rendererDrawColor renderer SDL.$= Linear.V4 255 255 255 255
  SDL.drawRects renderer rects
  --SDL.fillRects renderer rects

toRect :: GameObj -> SDL.Rectangle C.CInt
toRect obj =
  SDL.Rectangle
    (Linear.P . uncurry Linear.V2 . Lens.over Lens.both fromIntegral . pointToTuple $ obj ^. position . pos)
    (uncurry Linear.V2 . Lens.over Lens.both fromIntegral . sizeToTuple $ obj ^. position . size)
