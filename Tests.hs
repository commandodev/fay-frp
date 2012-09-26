{-# LANGUAGE NoImplicitPrelude #-}

module Tests where

import           Language.Fay.FFI
import           Language.Fay.FRP
import           Language.Fay.JQuery
import           Language.Fay.Prelude

main :: Fay ()

main = ready $ do
  log "Hi"

log :: Foreign a => a -> Fay ()
log = ffi "console.log(%1)"

logS :: String -> Fay ()
logS = ffi "console.log(%1)"
