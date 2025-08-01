# ghc-wasm-graphics

Simple toy example using the GHC WebAssembly backend to render images to a canvas.

Follows the approach used in [Making really tiny WebAssembly graphics demos][bare-metal-wasm], i.e.

- WebAssembly module writes rasterized image data to a buffer in wasm memory
- JS code copies wasm buffer to the JS heap and displays the image on a canvas using `putImageData`.

## Usage

### Nix

Run `nix develop` to enter a shell with the development dependencies, then run `./build.sh` to compile the wasm module. To run, start a development server in the repo root, e.g. with

```
python -m http.server 8000
```

and visit `localhost:8000` using a modern browser.

### Non-nix

You will need to manually set up an environment with the GHC wasm backend available (note this not part of the standard GHC distribution as of August 2025). See [https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta#getting-started-without-nix](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta#getting-started-without-nix).

## References
- [Making really tiny WebAssembly graphics demos][bare-metal-wasm]
- [Compiling Haskell to WebAssembly (ZuriHac 2025 talk by Cheng Shao)](https://youtu.be/9zv08bh2fhs?si=XBjzuaskKC7H-iuu)
- [GitHub repo accompanying Cheng Shao's ZuriHac 2025 talk](https://github.com/haskell-wasm/zurihac-2025)
- [GHC WebAssembly Backend (GHC User's Guide)](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)

[bare-metal-wasm]: https://cliffle.com/blog/bare-metal-wasm/

