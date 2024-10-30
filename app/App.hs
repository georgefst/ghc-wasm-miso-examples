{-# LANGUAGE CPP #-}

module App (start, Opts (..)) where

#ifdef wasi_HOST_OS
import GHC.Wasm.Prim
import Language.Javascript.JSaddle (JSM)
#else
import Language.Javascript.JSaddle
#endif

import Primer qualified
import SimpleCounter qualified
import Snake qualified
import TodoMVC qualified
import TwoZeroFourEight qualified
import XHR qualified

data Opts = Opts {primerUseSavedState :: Bool}

start :: Opts -> JSString -> JSM ()
start Opts{..} e =
  case fromJSString e :: String of
    "simplecounter" -> SimpleCounter.start
    "snake" -> Snake.start
    "primer" -> Primer.start primerUseSavedState
    "todomvc" -> TodoMVC.start
    "xhr" -> XHR.start
    "2048" -> TwoZeroFourEight.start
    _ -> fail "unknown example"
