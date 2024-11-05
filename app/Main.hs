{-# LANGUAGE CPP #-}

#ifdef wasi_HOST_OS

module MyMain (main) where

import App
import GHC.Wasm.Prim
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm

foreign export javascript "hs_start" main :: JSString -> IO ()

main :: JSString -> IO ()
main e = JSaddle.Wasm.run $ start e

#else

module Main (main) where

import App
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Warp
import System.Environment

main :: IO ()
main = getArgs >>= \case
    -- Note that `debug` works with `cabal repl` but not `cabal run`.
    -- The best workflow is to run `ghcid -c "cabal repl ghc-wasm-miso-examples" -W -T ':main primer'`.
    [arg] -> debug 8000 $ start $ toJSString arg
    _ -> fail "bad args: specify an example, e.g. 2048"

#endif
