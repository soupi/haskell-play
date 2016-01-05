{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module MySDL.Run where

import           Data.Maybe (fromMaybe)
import qualified Data.Char as Char (toLower)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad          ((>=>))
import qualified SDL
import qualified Linear
import qualified Options.Applicative as Opt
import Options.Applicative ((<>))

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
    { setShowInstructions :: Bool
    , setPlaySounds       :: Bool
    , setSpeed            :: Speed
    }

data Speed = SlowSpeed | NormalSpeed | FastSpeed

defaultSettings :: Settings
defaultSettings =
  Settings
    { setShowInstructions = False
    , setPlaySounds       = False
    , setSpeed            = NormalSpeed
    }

speed2InstPerFrame :: Speed -> Int
speed2InstPerFrame speed =
  case speed of
    SlowSpeed   -> 2
    NormalSpeed -> 10
    FastSpeed   -> 20

-------------------
-- Option Parser --
-------------------

argsParser :: Opt.Parser (FilePath, Settings)
argsParser =
  (,) <$> filepathParser
      <*> settingsParser

filepathParser :: Opt.Parser FilePath
filepathParser =
  Opt.argument Opt.str $
       Opt.metavar "FILE"
    <> Opt.help "Path to the game to load"


settingsParser :: Opt.Parser Settings
settingsParser =
  Settings <$>
      Opt.switch
        (  Opt.long  "instructions"
        <> Opt.short 'i'
        <> Opt.help  "Trace encountered instructions")

     <*> Opt.switch
        (  Opt.long  "no-sound"
        <> Opt.short 'n'
        <> Opt.help  "Run game without sound")

     <*>
       fmap (fromMaybe (setSpeed defaultSettings))
       (Opt.optional
        (Opt.option
          (Opt.str >>= parseSpeed)
          (  Opt.long "speed"
          <> Opt.short 's'
          <> Opt.metavar "SPEED"
          <> Opt.help "Set emulation speed to (slow/normal/fast)")))

parseSpeed :: String -> Opt.ReadM Speed
parseSpeed str =
  case map Char.toLower str of
    "slow"   -> pure SlowSpeed
    "normal" -> pure NormalSpeed
    "fast"   -> pure FastSpeed
    _        -> Opt.readerError $ "'" ++ str ++ "' is not a valid option"

argsParserInfo :: Opt.ParserInfo Settings
argsParserInfo =
  Opt.info (Opt.helper <*> settingsParser) $
     Opt.fullDesc
  <> Opt.progDesc "A purely functional game written in Haskell"
  <> Opt.header   "GaME"

-----------
-- Logic --
-----------

main :: IO (WorldInterface e a, a) -> IO ()
main initWorld = do
  settings <- Opt.execParser argsParserInfo
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

update :: MonadIO m => WorldInterface e a
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

{-
squareSize :: C.CInt
squareSize =  8

convertGFX :: V.Vector Bool -> V.Vector (SDL.Rectangle C.CInt)
convertGFX gfx = V.map f $ V.filter snd $ V.indexed gfx
    where f (index, _)  = SDL.Rectangle (Linear.P $ uncurry Linear.V2 (determinePos $ fromIntegral index)) (Linear.V2 squareSize squareSize)
          determinePos i = map2 (* squareSize) (i `mod` 64, i `div` 64)

drawRects :: MonadIO m => V.Vector Bool -> SDL.Renderer -> m ()
drawRects gfx renderer = do
  SDL.rendererDrawColor renderer SDL.$= Linear.V4 255 255 255 255
  let newGfx = convertGFX gfx
      rects  = VS.generate (V.length newGfx) (newGfx V.!)
  SDL.drawRects renderer rects
  SDL.fillRects renderer rects
-}
