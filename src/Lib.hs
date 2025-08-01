module Lib
  ( renderToBuffer,
    mandelbrot,
    rectangle,
  )
where

import Data.Complex (Complex ((:+)), magnitude)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeElemOff)

data RGBA = MkRGBA
  { red :: !Word8,
    green :: !Word8,
    blue :: !Word8,
    alpha :: !Word8
  }
  deriving (Show)

data Image = MkImage Int Int ((Int, Int) -> RGBA)

renderToBuffer :: Image -> IO (Ptr Word8)
renderToBuffer (MkImage height width pixelAt) = do
  ptr :: Ptr Word8 <- mallocBytes (height * width * 4)
  let setPixel ix = do
        let (MkRGBA r g b a) = pixelAt (coords ix)
        pokeElemOff ptr (4 * ix + 0) r
        pokeElemOff ptr (4 * ix + 1) g
        pokeElemOff ptr (4 * ix + 2) b
        pokeElemOff ptr (4 * ix + 3) a
  mapM_ setPixel [0 .. height * width]
  pure ptr
  where
    coords = (`divMod` width)

rectangle :: Int -> Int -> Image
rectangle height width = MkImage height width (\_ -> MkRGBA maxBound 0 0 maxBound)

mandelbrot :: Int -> Int -> Image
mandelbrot height width = MkImage height width pixelAt
  where
    pixelAt :: (Int, Int) -> RGBA
    pixelAt (ir, ic) =
      let z0 :: Complex Double = 0.0 :+ 0.0
       in go z0 0
      where
        x = fromIntegral ic / fromIntegral width
        y = fromIntegral ir / fromIntegral height

        re = 3.0 * (x - 2 / 3)
        im = 3.0 * (y - 0.5)
        c = re :+ im

        maxIters = 50
        maxMagnitude = 50.0

        go :: Complex Double -> Int -> RGBA
        go z n
          | n > maxIters = mkColor maxBound
          | magnitude z > maxMagnitude = mkColor (ratioAsW8 n maxIters)
          | otherwise = go (z ** 2 + c) (n + 1)

        ratioAsW8 :: (Integral a, Integral b) => a -> b -> Word8
        ratioAsW8 a b =
          let scale :: Double = fromIntegral $ maxBound @Word8
           in round (scale * fromIntegral a / fromIntegral b)

        mkColor alpha = MkRGBA 0 0 maxBound alpha
