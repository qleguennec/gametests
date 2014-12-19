{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE PackageImports #-}

module Main where

import           Control.Applicative
import           Control.Cond
import           Control.Lens
import           Control.Monad
import           Control.Monad.Writer
import           Core
import           Criterion.Measurement       (initializeTime)
import           Data.Foldable               as F
import           Data.Map                    as M hiding (size)
import           Data.Traversable            as T
import           FRP.Elerea.Simple
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Rendering
import           "GLFW-b" Graphics.UI.GLFW            as GLFW
import           Logic

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
main = displayLog =<<
  runGame defaultConf defaultWorld
    (withWindow
    $ \w -> do
      -- Signal network initialization
      !(netw, snk) <- io $ do
        (smp, snk) <- external M.empty
        netw <- start $ return smp
        return (netw, snk)

      -- glossState initialization
      !glossState <- io initState

      -- begin of the loop
      io initializeTime
      loop w (netw, snk) glossState
  )

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

    readInput :: GLFW.Window -> (Map Key (Game ()) -> IO ()) -> IO ()
    readInput window snk = do
      pollEvents
      -- todo: dedicated optimized function
      keysPressed <- filterM (keyIsPressed window) . keys $ defaultKeymap
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

    displayUnit u = Color (u^.ucolor)
      $ translate (u^.x) (u^.y)
      $ circle (u^.size)

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
