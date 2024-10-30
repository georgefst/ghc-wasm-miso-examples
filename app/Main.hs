{-# LANGUAGE CPP #-}

#ifdef wasi_HOST_OS

module MyMain (main) where

import App
import GHC.Wasm.Prim
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm

foreign export javascript "hs_start" main :: JSString -> IO ()

main :: JSString -> IO ()
main e = JSaddle.Wasm.run $ start Opts{primerUseSavedState = False} e

#else

module Main (main) where

import App
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Warp
import System.Environment
-- import Language.Javascript.JSaddle.WebKitGTK qualified as GTK

-- TODO building on top of this repo is a bit annoying
-- get Drew to incorporate this in to Primer with proper Nix setup?
-- at least get my base branch merged: https://github.com/tweag/ghc-wasm-miso-examples/pull/23

-- ghcid -c "cabal repl ghc-wasm-miso-examples" -W -T ':main primer'
main :: IO ()
main = getArgs >>= \case
    -- oh, wow, `debug` does actually work beautifully with GHCID
    -- (but look in to new, simpler alternative anyway, was it from Mercury?)
    -- I don't know why I assumed it was complicated
    -- ah, AFAICT `debug` completely breaks `cabal run` - i.e. only works in REPL
    [arg] -> debug 8000 $ start Opts{primerUseSavedState = True} $ toJSString arg
    -- TODO is there a good GHCID live-reload workflow for this?
    -- otherwise does work and it'd be nice to avoid Chromium
    -- perhaps use `GTK.runInWebView` with a web view that's part of an existing persistent application?
    -- [arg] -> GTK.run $ start Opts{primerUseSavedState = True} $ toJSString arg
    _ -> fail "bad args: specify an example, e.g. 2048"

#endif
