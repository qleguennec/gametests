{-# LANGUAGE PackageImports #-}

module Main where

import           Control.Applicative
import           Control.Concurrent          (threadDelay)
import           Control.Lens
import           Control.Monad
import           Core
import           Data.Foldable               as F
import           Data.Map                    as M hiding (size)
import           FRP.Elerea.Simple
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Rendering
import           "GLFW-b" Graphics.UI.GLFW            as GLFW
import           Logic
import           System.Exit                 (exitSuccess)
import Data.Traversable as T

type Keymap = Map Key (Game ())

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

defaultWorld :: World
defaultWorld = World
  { _units =
      [ Unit 0 (-100) 10 4 blue
      , Unit 0 100 30 3.5 white
      , Unit 50 50 20 15 green
      ]
  }

main :: IO ()
main = do
  _ <- runGame defaultConf defaultWorld
    $ withWindow
    $ \w -> do
      -- Signal network initialization
      (netw, snk) <- io $ do
        (smp, snk) <- external M.empty
        netw <- start $ return smp
        return (netw, snk)

      -- glossState initialization
      glossState <- io initState

      -- begin of the loop
      loop w (netw, snk) glossState
  exitSuccess

  where
    loop window (netw, snk) glossState = forever $ do
      io $ threadDelay 1000

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
      u <- use units
      io . displayPicture (w, h) black glossState 1.0 . Pictures
         $ T.fmapDefault displayUnit u
      io $ swapBuffers window

    displayUnit u = Color (u^.ucolor)
      $ translate (u^.x) (u^.y)
      $ circle (u^.size)

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress <$> GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False
