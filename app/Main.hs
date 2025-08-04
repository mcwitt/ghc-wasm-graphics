module Mandelbrot where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import GHC.Wasm.Prim (JSString (JSString), toJSString)
import Lib (mandelbrot, renderToNewBuffer)
import Lib.Animation (buffer, height, renderFrame, width)

foreign export javascript "get_description sync"
  get_description :: JSString

foreign export javascript "render_image" -- default is asynchronous
  render_image :: Int -> Int -> IO (Ptr Word8)

foreign import javascript unsafe "console.log($1)"
  console_log :: JSString -> IO ()

foreign export javascript "get_buffer sync"
  buffer :: Ptr Word8

foreign export javascript "render_frame sync"
  renderFrame :: Double -> IO ()

foreign export javascript "get_height sync"
  height :: Int

foreign export javascript "get_width sync"
  width :: Int

get_description :: JSString
get_description = toJSString "Rendering images and animations using the GHC WebAssembly backend"

render_image :: Int -> Int -> IO (Ptr Word8)
render_image height width = do
  consoleLog ("height: " <> show height <> "; width: " <> show width)
  putStrLn "Hello from GHC/wasm!"
  renderToNewBuffer (mandelbrot height width)

consoleLog :: String -> IO ()
consoleLog = console_log . toJSString
