{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Play.Run where

import           Data.Maybe (fromMaybe, isJust)
import           Control.Monad.IO.Class (MonadIO)
import qualified SDL
import qualified Data.Vector.Storable as VS
import qualified Foreign.C.Types as C (CInt)
import qualified Linear
import qualified Linear.Affine as Linear
import qualified Control.Lens     as Lens
import           Control.Lens (makeLenses, (&), (^.))

import qualified MySDL.MySDL as MySDL
import qualified MySDL.Run   as MySDL
import           Play.Types
import           Play.Collisions


-----------
-- Types --
-----------

data Game =
  Game
    { _screen  :: GameObj (Point, Size, Point, Int, Maybe Point)
    , _playerA :: GameObj (Point, Size, Point, Int, Maybe Point)
    , _playerB :: GameObj (Point, Size, Point, Int, Maybe Point)
    , _ball :: Ball
    }

type Ball = GameObj (Point, Size, Point, Int, Maybe Point)

makeLenses ''Game

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

initWorld :: Monad m => m (MySDL.WorldInterface Keys Game, Game)
initWorld = pure (worldInterface, initGame)

initGame :: Game
initGame =
  Game
    { _screen  = initScreen
    , _playerA = initPlayerA
    , _playerB = initPlayerB
    , _ball    = initBall
    }

initScreen =
  GameObj
    (PositionComponent Lens._1 Lens._2)
    (MovementComponent Lens._3 Lens._4)
    (CollisionComponent Lens._5)
    (Point 0 0, Size 800 600, Point 0 0, 0, Nothing)


initBall :: Ball
initBall =
  GameObj
    (PositionComponent Lens._1 Lens._2)
    (MovementComponent Lens._3 Lens._4)
    (CollisionComponent Lens._5)
    (Point 390 290, Size 20 20, Point 0 1, 3, Nothing)

initPlayerA =
  GameObj
    (PositionComponent Lens._1 Lens._2)
    (MovementComponent Lens._3 Lens._4)
    (CollisionComponent Lens._5)
    (Point 350 50, Size 100 30, Point 0 0, 2, Nothing)

initPlayerB =
  GameObj
    (PositionComponent Lens._1 Lens._2)
    (MovementComponent Lens._3 Lens._4)
    (CollisionComponent Lens._5)
    (Point 350 520, Size 100 30, Point 0 0, 2, Nothing)



worldInterface :: MySDL.WorldInterface Keys Game
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
  let speed = 1
      singleMove k1 k2
        | testKey k1 keys && not (testKey k2 keys) = -1
        | testKey k2 keys && not (testKey k1 keys) =  1
        | otherwise = 0
      hori = singleMove KeyUp KeyDown
      vert = singleMove KeyLeft KeyRight
  in Point (vert * speed) (hori * speed)

update :: MonadIO m => Keys -> MySDL.World Game ->  m (Either (Maybe String) (MySDL.World Game))
update keys (sets, game) =
  let mov = keysToMovement keys
      nGame = game & Lens.over (playerA . posOf (game ^. playerA)) (`addPoint` mov)
                   & Lens.over ball (updateBall game)
  in  (pure . pure) (sets, nGame)

addPoint :: Point -> Point -> Point
addPoint = apToPoint (+)

mulPoint :: Point -> Point -> Point
mulPoint = apToPoint (*)

apToPoint :: (Int -> Int -> Int) -> Point -> Point -> Point
apToPoint f p1 p2 =
  p1 & Lens.over x (f (p2 ^. x))
     & Lens.over y (f (p2 ^. y))

moveObj :: GameObj s -> GameObj s
moveObj obj =
  let spd = obj ^. speedOf obj
      dir = (obj ^. directionOf obj) `mulPoint` Point spd spd
  in Lens.over (posOf obj) (`addPoint` dir) obj

updateBall :: Game -> Ball -> Ball
updateBall game obj =
  case checkScreenBounds (game ^. screen) $ moveObj obj of
    (True, ball') ->
      Lens.over (directionOf ball' . y) (* (-1)) ball'
    (False, ball') ->
      case testCollisionWith [ball'] [game ^. playerA, game ^. playerB] of
        [ball''] ->
          if isJust (ball'' ^. collidedOf ball'') then
            Lens.over (directionOf ball' . y) (* (-1)) ball'
          else
            ball''

checkScreenBounds :: GameObj a -> GameObj s -> (Bool, GameObj s)
checkScreenBounds closing obj =
  let (Point x_ y_) = obj ^. posOf obj
      (Size  w_ h_) = obj ^. sizeOf obj
      (Point bx by) = closing ^. posOf closing
      (Size  bw bh) = closing ^. sizeOf closing
  in
  if | x_ < bx -> (,) True $ Lens.set (posOf obj . x) bx obj
     | x_ + w_ > bx + bw -> (,) True $ Lens.set (posOf obj . x) (bw - w_) obj
     | y_ < by -> (,) True $ Lens.set (posOf obj . y) by obj
     | y_ + h_ > by + bh -> (,) True $ Lens.set (posOf obj . y) (bh - h_) obj
     | otherwise -> (False, obj)

-- render

render :: MonadIO m => SDL.Renderer -> MySDL.World Game -> m ()
render renderer (_, Game _ pA pB b) = do
  let rects = VS.fromList $ fmap toRect [pA, pB] ++ [toRect b]
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  SDL.rendererDrawColor renderer SDL.$= Linear.V4 255 255 255 255
  SDL.drawRects renderer rects
  --SDL.fillRects renderer rects

toRect :: GameObj s -> SDL.Rectangle C.CInt
toRect obj =
  SDL.Rectangle
    (Linear.P . uncurry Linear.V2 . Lens.over Lens.both fromIntegral . pointToTuple $ obj ^. posOf obj)
    (uncurry Linear.V2 . Lens.over Lens.both fromIntegral . sizeToTuple $ obj ^. sizeOf obj)
