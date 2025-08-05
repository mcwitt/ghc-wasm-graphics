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
radius = 10

buffer :: Ptr Word8
{-# NOINLINE buffer #-}
buffer = unsafePerformIO (mallocBytes (height * width * 4))

newtype Position = MkPosition (V2 Double)

newtype Velocity = MkVelocity (V2 Double)

getFrame :: World -> Image
getFrame (MkParticle (MkPosition (V2 xp yp)) _) =
  MkImage
    height
    width
    ( \(V2 x y) ->
        let dx = fromIntegral x - xp
            dy = fromIntegral y - yp
            dr = sqrt (dx ** 2 + dy ** 2)
         in blend fg bg (s1 (dr - radius))
    )
  where
    s1 x
      | x <= 0 = 0.0
      | 0 <= x && x <= 1 = 3 * x ** 2 - 2 * x ** 3
      | otherwise = 1.0

    fg = MkRGBA maxBound 0 0 maxBound
    bg = MkRGBA 0 0 0 maxBound

data Particle = MkParticle !Position !Velocity

type World = Particle

world :: IORef World
{-# NOINLINE world #-}
world =
  unsafePerformIO
    ( newIORef
        ( MkParticle
            (MkPosition (V2 radius radius))
            (MkVelocity (V2 50 67))
        )
    )

renderFrame :: Double -> IO ()
renderFrame dt = do
  w <- readIORef world
  let w' = updateParticle w
  let frame = getFrame w
  renderToBuffer frame buffer
  writeIORef world w'
  where
    updateParticle (MkParticle (MkPosition (V2 x y)) vel@(MkVelocity (V2 vx vy)))
      | vx < 0 && x' < radius = MkParticle (MkPosition (V2 (2 * radius - x') y')) (MkVelocity (V2 (-vx) vy))
      | vy < 0 && y' < radius = MkParticle (MkPosition (V2 x' (2 * radius - y'))) (MkVelocity (V2 vx (-vy)))
      | 0 < vx && fromIntegral width - radius < x' =
          MkParticle
            (MkPosition (V2 (2 * (fromIntegral width - radius) - x') y'))
            (MkVelocity (V2 (-vx) vy))
      | 0 < vy && fromIntegral height - radius < y' =
          MkParticle
            (MkPosition (V2 x' (2 * (fromIntegral height - radius) - y')))
            (MkVelocity (V2 vx (-vy)))
      | otherwise = MkParticle (MkPosition (V2 x' y')) vel
      where
        x' = x + vx * dt
        y' = y + vy * dt
