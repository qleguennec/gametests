{-# LANGUAGE PackageImports #-}

module Main where

import           Control.Concurrent          (threadDelay)
import           Control.Lens
import           Control.Monad
import           Core
import           Data.Foldable               as F
import           Data.Map                    as M
import           FRP.Elerea.Simple
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Rendering
import           "GLFW-b" Graphics.UI.GLFW            as GLFW
import           Logic
import           System.Exit                 (exitSuccess)

type Keymap = Map Key (Game ())

defaultKeymap :: Keymap
defaultKeymap = M.fromList
  [ (Key'Right, movePlayer DRight)
  , (Key'Left , movePlayer DLeft)
  , (Key'Down , movePlayer DDown)
  , (Key'Up   , movePlayer DUp)
  ]

defaultConf :: Config
defaultConf = Config
  { _height    = 600
  , _width     = 600
  , _increment = 20.0
  , _title     = "test"
  }

defaultPlayer :: Unit
defaultPlayer = Unit
  { _x      = 0
  , _y      = 0
  , _center = 27
  }

defaultWorld :: World
defaultWorld = World
  { _player = defaultPlayer
  }

main :: IO ()
main = do
  _ <- runGame defaultConf defaultWorld
    $ withWindow
    $ \w -> do
      -- Signal network initialization
      (netw, snk) <- io $ do
        (smp, snk) <- external M.empty
        netw <- start (return smp)
        return (netw, snk)

      -- glossState initialization
      glossState <- io initState

      -- begin of the loop
      loop w (netw, snk) glossState
  exitSuccess

  where
    loop window (netw, snk) glossState = forever $ do
      io $ threadDelay 10

      -- Read signals, sample them and sequence them
      io $ readInput window snk
      inputs <- io netw
      F.sequence_ inputs

      -- render and display frame
      renderFrame window glossState

    readInput :: GLFW.Window -> (Map Key (Game ()) -> IO ()) -> IO ()
    readInput window snk = do
      pollEvents

      -- todo: dedicated optimized function
      keysPressed <- filterM (keyIsPressed window) . keys $ defaultKeymap
      snk . M.filterWithKey (\k _ -> F.elem k keysPressed)
        $ defaultKeymap

    renderFrame window glossState = do
      w <- view width
      h <- view height
      p <- use player
      io $ displayPicture (w, h) black glossState 1.0
          $ Pictures [Color red $ translate (p^.x) (p^.y)
            $ circle (fromIntegral $ p^.center)]
      io $ swapBuffers window

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False
