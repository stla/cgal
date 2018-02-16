{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Lib
  where
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "testintersection" c_testintersection :: IO ()
