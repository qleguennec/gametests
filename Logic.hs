module Logic where

import           Control.Applicative
import           Control.Cond
import           Control.Lens
import           Core
import           Util

data Direction = DLeft | DRight | DDown | DUp

trans :: Direction -> Float -> (Float, Float)
trans dir i = case dir of
  DLeft  -> (-i, 0)
  DRight -> (i, 0)
  DDown  -> (0, -i)
  DUp    -> (0, i)

inBounds :: (Float, Float) -> Game Bool
inBounds (a, b) = do
  p <- use player
  w <- bound (p^.center) <$> view width
  h <- bound (p^.center) <$> view height
  return $ (a < w) && (b < h) && (a > (-w)) && (b > (-h))
  where
    bound :: Int -> Int -> Float
    bound c d = (fromIntegral d/2) - fromIntegral c

movePlayer :: Direction -> Game ()
movePlayer dir =  do
    p <- use player
    i <- view increment
    let (a, b) = tupleAp (+) (p^.x, p^.y) (trans dir i)
    whenM (inBounds (a, b))
          (player .= Unit a b (p^.center))
