{-# LANGUAGE CPP #-}

module CompatibilityShims (
#if __GLASGOW_HASKELL__ < 710
    (<$>)
#endif
)
where

#if __GLASGOW_HASKELL__ < 710
import Data.Functor ((<$>))
#endif
