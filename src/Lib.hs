module Lib
  ( Image (MkImage),
    RGBA (MkRGBA),
    V2 (V2),
    blend,
    mandelbrot,
    rectangle,
    renderToBuffer,
    renderToNewBuffer,
    setPixel,
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

blend :: RGBA -> RGBA -> Double -> RGBA
blend (MkRGBA r1 g1 b1 a1) (MkRGBA r2 g2 b2 a2) s =
  MkRGBA
    (lerpW r1 r2)
    (lerpW g1 g2)
    (lerpW b1 b2)
    (lerpW a1 a2)
  where
    lerp x1 x2 = (1.0 - s) * x1 + s * x2
    lerpW w1 w2 = floor $ lerp (fromIntegral w1) (fromIntegral w2)

data V2 a = V2 !a !a deriving (Show)

data Image = MkImage Int Int (V2 Int -> RGBA)

setPixel :: Int -> Ptr Word8 -> V2 Int -> RGBA -> IO ()
setPixel width ptr (V2 x y) (MkRGBA r g b a) = do
  let ix = x + width * y
  pokeElemOff ptr (4 * ix + 0) r
  pokeElemOff ptr (4 * ix + 1) g
  pokeElemOff ptr (4 * ix + 2) b
  pokeElemOff ptr (4 * ix + 3) a

renderToNewBuffer :: Image -> IO (Ptr Word8)
renderToNewBuffer im@(MkImage height width _) = do
  ptr :: Ptr Word8 <- mallocBytes (height * width * 4)
  renderToBuffer im ptr
  pure ptr

renderToBuffer :: Image -> Ptr Word8 -> IO ()
renderToBuffer (MkImage height width rgbaAt) ptr = do
  mapM_
    (\pos -> setPixel width ptr pos $ rgbaAt pos)
    [ V2 x y
      | x <- [0 .. width - 1],
        y <- [0 .. height - 1]
    ]

rectangle :: Int -> Int -> Image
rectangle height width = MkImage height width (\_ -> MkRGBA maxBound 0 0 maxBound)

mandelbrot :: Int -> Int -> Image
mandelbrot height width = MkImage height width rgbaAt
  where
    rgbaAt :: V2 Int -> RGBA
    rgbaAt (V2 x y) =
      let z0 :: Complex Double = 0.0 :+ 0.0
       in go z0 0
      where
        xn = fromIntegral x / fromIntegral width
        yn = fromIntegral y / fromIntegral height

        re = 3.0 * (xn - 2 / 3)
        im = 3.0 * (yn - 0.5)
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
