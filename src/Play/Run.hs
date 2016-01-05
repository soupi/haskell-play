{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Play.Run where

import           Data.Maybe (fromMaybe)
import           Control.Monad.IO.Class (MonadIO)
import qualified SDL
import qualified Data.Vector.Storable as VS
import qualified Foreign.C.Types as C (CInt)
import qualified Linear
import qualified Linear.Affine as Linear
import qualified Control.Lens     as Lens
import           Control.Lens (makeLenses, ALens', (&), (^.))

import qualified MySDL.MySDL as MySDL
import qualified MySDL.Run   as MySDL

-----------
-- Types --
-----------

data Point = Point { _x :: Int, _y :: Int }
data Size  = Size { _w :: Int, _h :: Int }

pointToTuple :: Point -> (Int, Int)
pointToTuple (Point x_ y_) = (x_, y_)
sizeToTuple :: Size -> (Int, Int)
sizeToTuple (Size w_ h_) = (w_, h_)

data GameObj s =
  GameObj
    { _movement :: MovementComponent s
    , _state :: s
    }

data MovementComponent s =
  MovementComponent
    { _position :: ALens' s Point
    , _size :: ALens' s Size
    }

data Game =
  Game
    { _screen :: GameObj (Point, Size)
    , _playerA :: GameObj (Point, Size)
    , _playerB :: GameObj (Point, Size)
    , _ball :: Ball
    }

type Ball = GameObj (Point, Size, Int)

makeLenses ''MovementComponent
makeLenses ''GameObj
makeLenses ''Point
makeLenses ''Size
makeLenses ''Game

sizeOf :: GameObj s -> Lens.Lens' (GameObj s) Size
sizeOf obj = state . Lens.cloneLens (obj ^. movement . size)

posOf :: GameObj s -> Lens.Lens' (GameObj s) Point
posOf obj = state . Lens.cloneLens (obj ^. movement . position)

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
    { _screen  = GameObj (MovementComponent Lens._1 Lens._2) (Point 0 0, Size 800 600)
    , _playerA = GameObj (MovementComponent Lens._1 Lens._2) (Point 350 50, Size 100 30)
    , _playerB = GameObj (MovementComponent Lens._1 Lens._2) (Point 350 520, Size 100 30)
    , _ball    = GameObj (MovementComponent Lens._1 Lens._2) (Point 390 290, Size 20 20, 3)
    }

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
                   & Lens.over ball updateBall
  in  (pure . pure) (sets, nGame)

addPoint :: Point -> Point -> Point
addPoint p1 p2 =
  p1 & Lens.over x (+ (p2 ^. x))
     & Lens.over y (+ (p2 ^. y))

updateBall :: Ball -> Ball
updateBall obj =
  case checkScreenBounds $ Lens.over (posOf obj) (`addPoint` Point 0 (obj ^. state . Lens._3)) obj of
    (True, ball') ->
      Lens.over (state . Lens._3) (* (-1)) ball'
    (False, ball') ->
      ball'

checkScreenBounds :: GameObj s -> (Bool, GameObj s)
checkScreenBounds obj =
  let (Point x_ y_) = obj ^. posOf obj
      (Size  w_ h_) = obj ^. sizeOf obj
  in
  if | x_ < 0 -> (,) True $ Lens.set (posOf obj . x) 0 obj
     | x_ + w_ > 800 -> (,) True $ Lens.set (posOf obj . x) (800 - w_) obj
     | y_ < 0 -> (,) True $ Lens.set (posOf obj . y) 0 obj
     | y_ + h_ > 600 -> (,) True $ Lens.set (posOf obj . y) (600 - h_) obj
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
