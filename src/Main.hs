{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PackageImports #-}

module Main where

import           Control.Applicative
import           Control.Cond
import           Control.Lens
import           Control.Monad
import           Control.Monad.Writer
import           Core
import           Criterion.Measurement       (initializeTime)
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy        as BS
import qualified Data.Foldable               as F
import qualified Data.Map                    as M hiding (size)
import qualified Data.Traversable            as T
import           FRP.Elerea.Simple
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Rendering
import           "GLFW-b" Graphics.UI.GLFW            as GLFW
import           Logic
import qualified Data.ByteString.Char8       as C

type Keymap = M.Map Key (Game ())

defaultKeymap :: Keymap
defaultKeymap = M.fromList
  [ (Key'Right, moveUnits DRight)
  , (Key'Left , moveUnits DLeft )
  , (Key'Down , moveUnits DDown )
  , (Key'Up   , moveUnits DUp   )
  ]

defaultConf :: Config
defaultConf = Config
  { _height    = 600
  , _width     = 600
  , _title     = "test"
  }

main :: IO ()
main = liftM eitherDecode (BS.readFile "ressources/world.json")
  >>= \case
  (Right world) ->
    runGame defaultConf world
      (withWindow
      $ \w -> do
        -- Signal network initialization
        !(netw, snk) <- io $ do
          (smp, snk) <- external M.empty
          netw <- start $ return smp
          return (netw, snk)

        -- glossState initialization
        !glossState <- io initState

        -- units initialization
        !initUnits <- use units >>= T.traverse (\u ->
          case u^.sprites of
            (Right _) -> fail "Error: sprites already loaded"
            (Left sp) -> liftM (flip (set sprites) u)
              $ Right <$> io (initSprites sp))
        units .= initUnits

        -- begin of the loop
        io initializeTime
        loop w (netw, snk) glossState) >>= displayLog
  (Left e) -> putStrLn e

  where
    loop window (netw, snk) glossState = do
      -- Read signals, sample them and sequence them, then process output
      io $ readInput window snk
      !inputs <- io netw

      !(_, t) <- execTime $ process window glossState inputs
      tell $ Log t 1
      ifM (io $ keyIsPressed window Key'Escape)
          (return ())
          (loop window (netw, snk) glossState)

    readInput :: GLFW.Window -> (Keymap -> IO ()) -> IO ()
    readInput window snk = do
      pollEvents
      -- todo: dedicated optimized function
      keysPressed <- filterM (keyIsPressed window) . M.keys $ defaultKeymap
      snk . M.filterWithKey (\k _ -> F.elem k keysPressed)
        $ defaultKeymap

    process window glossState inputs = do
      F.sequence_ inputs
      renderFrame window glossState

    renderFrame window glossState = do
      w <- view width
      h <- view height
      u <- use units
      io . displayPicture (w, h) black glossState 1.0 . Pictures
         $ T.fmapDefault displayUnit u
      io $ swapBuffers window

    displayUnit u = Color red
      $ translate (u^.x) (u^.y)
      $ circle (u^.size)

    initSprites :: SpritesPaths -> IO LoadedSprites
    initSprites = traverse (loadBMP . C.unpack)

displayLog :: Log -> IO ()
displayLog (Log t f) = do
  print $ "frames displayed: " ++ show f
  print $ "time beetween frames: " ++ show t
  print $ "average fps: " ++ show (1/(t/fromIntegral f))

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress <$> GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False
