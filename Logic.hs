module Logic where

import           Control.Applicative
import           Control.Cond
import           Control.Lens
import           Core
import           Data.Traversable    as T
import           Util

data Direction = DLeft | DRight | DDown | DUp

trans :: Direction -> Float -> (Float, Float)
trans dir i = case dir of
  DLeft  -> (-i, 0)
  DRight -> (i, 0)
  DDown  -> (0, -i)
  DUp    -> (0, i)

inBounds :: (Float, Float) -> Unit -> Game Bool
inBounds (a, b) u = do
  w <- bound (u^.size) <$> view width
  h <- bound (u^.size) <$> view height
  return $ (a < w)
        && (b < h)
        && (a > (-w))
        && (b > (-h))
  where
    bound :: Float -> Int -> Float
    bound c d = (fromIntegral d/2) - c

moveUnit :: Direction -> Unit -> Game Unit
moveUnit dir u = do
    let (a, b) = tupleAp (+) (u^.x, u^.y) (trans dir (u^.speed))
    ifM (inBounds (a, b) u)
        (return . set y b . set x a $ u)
        (return u)

moveUnits :: Direction -> Game ()
moveUnits dir = use units >>= T.traverse (moveUnit dir) >>= assign units
