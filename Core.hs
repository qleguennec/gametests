{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE TemplateHaskell            #-}

module Core where

import           Control.Applicative
import           Control.Cond
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           "GLFW-b" Graphics.UI.GLFW     as GLFW

data Config = Config
  { _height    :: Int
  , _width     :: Int
  , _increment :: Float
  , _title     :: String
  }
makeLenses ''Config

data Unit = Unit
  { _x      :: Float
  , _y      :: Float
  , _center :: Int
  }
makeLenses ''Unit

data World = World
  { _player :: Unit
  }
makeLenses ''World

newtype Game a = Game (ReaderT Config (StateT World IO) a)
  deriving (MonadIO, Functor, Applicative, Monad, MonadState World, MonadReader Config)

runGame :: Config -> World -> Game a -> IO (a, World)
runGame c w (Game a) = runStateT (runReaderT a c) w

withWindow :: (GLFW.Window -> Game ()) -> Game ()
withWindow f = whenM (io GLFW.init) $ do
    w <- view width
    h <- view height
    t <- view title
    m <- io $ GLFW.createWindow w h t Nothing Nothing
    case m of
      (Just window) -> do
        io $ GLFW.makeContextCurrent m
        f window
        io $ GLFW.setErrorCallback $ Just simpleErrorCallback
        io $ GLFW.destroyWindow window
      Nothing -> return ()
    io GLFW.terminate
    where
      simpleErrorCallback e s = putStrLn . unwords $ [show e, show s]

-- Util

-- alias for liftIO
io :: IO a -> Game a
io = liftIO
