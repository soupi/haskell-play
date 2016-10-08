{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module MySDL.Run where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad          ((>=>))
import qualified SDL
import qualified Linear

import qualified MySDL.MySDL as MySDL

-----------
-- Types --
-----------

type World a = (Settings, a)

data WorldInterface e a =
  WorldInterface
    { eventsMaker :: [SDL.EventPayload] -> (SDL.Scancode -> Bool) -> e
    , updateWorld :: forall m. MonadIO m => e -> World a -> m (Either (Maybe String) (World a))
    , renderWorld :: forall m. MonadIO m => SDL.Renderer -> World a -> m ()
    }

data Settings =
  Settings

defaultSettings :: Settings
defaultSettings =
  Settings

-----------
-- Logic --
-----------

main :: IO (WorldInterface e a, a) -> IO ()
main initWorld = do
  settings <- pure defaultSettings
  w <- initWorld
  runGame $ fmap ((,) settings) w

runGame :: (WorldInterface e a, World a) -> IO ()
runGame w = do
  putStrLn "Hello Game!"
  _ <- run w
  putStrLn "Goodbye."

run :: (WorldInterface e a, World a) -> IO (World a)
run (interface, world) =
  MySDL.withWindow "Game" (MySDL.myWindowConfig (Linear.V2 800 600)) $
    flip MySDL.withRenderer (setBGColorBlack >=> MySDL.apploop world (update interface) . render interface)

update :: MonadIO m
  => WorldInterface e a
  -> [SDL.EventPayload]
  -> (SDL.Scancode -> Bool)
  -> World a
  -> m (Either (Maybe String) (World a))
update interface ev hasKey =
  updateWorld interface (eventsMaker interface ev hasKey)


setKeys :: [SDL.Scancode] -> (SDL.Scancode -> Bool) -> [(SDL.Scancode, Bool)]
setKeys keyMapping isKeyPressed =
   zip keyMapping $ map isKeyPressed keyMapping

render :: MonadIO m => WorldInterface e a -> (SDL.Window, SDL.Renderer) -> World a -> m ()
render interface (_, renderer) world = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
--  drawRects (Lens.view CPU.gfx cpu) renderer
  renderWorld interface renderer world
  SDL.present renderer

setBGColorBlack :: MonadIO m => (SDL.Window, SDL.Renderer) -> m (SDL.Window, SDL.Renderer)
setBGColorBlack sdlStuff@(_, renderer) = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  pure sdlStuff

