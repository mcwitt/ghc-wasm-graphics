module Mandelbrot where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import GHC.Wasm.Prim (JSString (JSString), toJSString)
import Lib (mandelbrot, renderToBuffer)

foreign export javascript "get_description sync"
  get_description :: JSString

foreign export javascript "render_image" -- default is asynchronous
  render_image :: Int -> Int -> IO (Ptr Word8)

foreign import javascript unsafe "console.log($1)"
  console_log :: JSString -> IO ()

get_description :: JSString
get_description = toJSString "Rendering an image of the Mandelbrot set using Haskell compiled to WebAssembly"

consoleLog :: String -> IO ()
consoleLog = console_log . toJSString

render_image :: Int -> Int -> IO (Ptr Word8)
render_image height width = do
  consoleLog ("height: " <> show height <> "; width: " <> show width)
  putStrLn "Hello from GHC/wasm!"
  renderToBuffer (mandelbrot height width)
