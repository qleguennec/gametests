{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE TemplateHaskell            #-}

module Core where

import           Control.Applicative
import           Control.Cond
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Criterion.Measurement       as Bench (getTime)
import           Data.Aeson
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as C
import qualified Data.Map                    as M
import           Data.Traversable            as T
import           Data.Vector                 as V (toList)
import           Graphics.Gloss.Data.Picture
import qualified "GLFW-b" Graphics.UI.GLFW            as GLFW

data Config = Config
  { _height :: Int
  , _width  :: Int
  , _title  :: String
  } deriving (Eq, Show, Read)
makeLenses ''Config

data UnitType = Player | Ennemy
  deriving (Eq, Show, Read)

-- Literal sprites are non-loaded sprites
-- (second member containing path of the picture)
data SpriteType = SFace | SBack | SRight | SLeft
  deriving (Eq, Show, Read, Ord)
type SpritesPaths  = M.Map SpriteType BS.ByteString
type LoadedSprites = M.Map SpriteType Picture

data Unit = Unit
  { _type    :: UnitType
  , _x       :: Float
  , _y       :: Float
  , _size    :: Float
  , _speed   :: Float
  , _sprites :: Either SpritesPaths LoadedSprites
  } deriving (Eq, Show)
makeLenses ''Unit

data World = World
  { _units :: [Unit]
  } deriving (Eq, Show)
makeLenses ''World

data Log = Log
  { bfTime :: Double
  , frames :: Int
  } deriving (Eq, Show, Read)

instance Monoid Log where
  mempty = Log 0 0
  (Log t f) `mappend` (Log t' f') = Log (t+t') (f+f')


newtype Game a =
  Game (ReaderT Config (StateT World (WriterT Log IO)) a) deriving
      ( MonadIO, Functor, Applicative, Monad
      , MonadState World, MonadReader Config, MonadWriter Log
      )

io :: IO a -> Game a
io = liftIO

execTime :: Game a -> Game (a, Double)
execTime g = do
  !t <- io Bench.getTime
  !a <- g
  !t' <- io Bench.getTime
  return (a, t'-t)

runGame :: Config -> World -> Game a -> IO Log
runGame c w (Game a) = execWriterT $ runStateT (runReaderT a c) w

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

-- Data parsing

instance FromJSON World where
  parseJSON (Object v) = World <$> v .: "units"
  parseJSON _ = fail "World parsing failed"

instance FromJSON Unit where
  parseJSON (Object v) =
    Unit <$> v .: "type"
         <*> v .: "x"
         <*> v .: "y"
         <*> v .: "size"
         <*> v .: "speed"
         <*> (Left <$> v .: "sprites")
  parseJSON _ = fail "Unit parsing failed"

instance FromJSON SpritesPaths where
  parseJSON (Array a) = M.fromList . V.toList <$> T.for a
    (\case
      (Object v) ->
        (,) <$> v .: "type" <*> (C.pack <$> v .: "src")
      _ -> fail "Sprites parsing failed"
    )
  parseJSON _ = fail "Sprites parsing failed"

instance FromJSON UnitType where
  parseJSON (String s) = return . read . show $ s
  parseJSON _ = fail "UnitType parsing failed"

instance FromJSON SpriteType where
  parseJSON (String s) = return . read . show $ s
  parseJSON _ = fail "SpriteType parsing failed"
