module Lib.Animation (buffer, renderFrame, height, width) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr)
import Lib (Image (MkImage), RGBA (MkRGBA), V2 (V2), blend, renderToBuffer)
import System.IO.Unsafe (unsafePerformIO)

height, width :: Int
height = 200
width = 200

radius :: Double
radius = 10.0

buffer :: Ptr Word8
{-# NOINLINE buffer #-}
buffer = unsafePerformIO (mallocBytes (height * width * 4))

newtype Position = MkPosition (V2 Double)

newtype Velocity = MkVelocity (V2 Double)

getFrame :: Position -> Image
getFrame (MkPosition (V2 xb yb)) =
  MkImage
    height
    width
    ( \(V2 x y) ->
        let dx = fromIntegral x - xb
            dy = fromIntegral y - yb
            dr = sqrt (dx ** 2 + dy ** 2)
         in blend foreground background (s1 (dr - radius))
    )
  where
    s1 x
      | x <= 0 = 0.0
      | 0 <= x && x <= 1 = 3 * x ** 2 - 2 * x ** 3
      | otherwise = 1.0

    foreground = MkRGBA maxBound 0 0 maxBound
    background = MkRGBA 0 0 0 maxBound

data BoxState = MkBoxState Position Velocity

boxState :: IORef BoxState
{-# NOINLINE boxState #-}
boxState =
  unsafePerformIO
    ( newIORef
        ( MkBoxState
            (MkPosition (V2 radius radius))
            (MkVelocity (V2 50 67))
        )
    )

renderFrame :: Double -> IO ()
renderFrame dt = do
  s@(MkBoxState pos _) <- readIORef boxState
  let s' = updateBoxState s
  let frame = getFrame pos
  renderToBuffer frame buffer
  writeIORef boxState s'
  where
    updateBoxState :: BoxState -> BoxState
    updateBoxState (MkBoxState (MkPosition (V2 x y)) vel@(MkVelocity (V2 vx vy)))
      | x' < radius || x' >= fromIntegral width - radius =
          MkBoxState
            (MkPosition (V2 x y'))
            (MkVelocity (V2 (-vx) vy))
      | y' < radius || y' >= fromIntegral width - radius =
          MkBoxState
            (MkPosition (V2 x' y))
            (MkVelocity (V2 vx (-vy)))
      | otherwise = MkBoxState (MkPosition (V2 x' y')) vel
      where
        x' = x + vx * dt
        y' = y + vy * dt
