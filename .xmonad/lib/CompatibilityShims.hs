{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE TypeFamilies #-}
#endif

module CompatibilityShims (
#if __GLASGOW_HASKELL__ < 710
    def,
    (<$>)
#endif
)
where

#if __GLASGOW_HASKELL__ < 710
import Data.Functor ((<$>))

-- Needed for defaults:
import XMonad (XConfig)
import XMonad.Actions.GridSelect (HasColorizer(..), GSConfig(..), defaultGSConfig)
import XMonad.Config (defaultConfig)
import XMonad.Hooks.DynamicLog (PP(..), defaultPP)
import XMonad.Layout (Choose, Full, Mirror, Tall)
import XMonad.Layout.Decoration (Theme(..), defaultTheme)
import XMonad.Prompt (XPConfig(..), defaultXPConfig)

class Default a where
  def :: a
instance Default XPConfig where def = defaultXPConfig
instance (HasColorizer a) => Default (GSConfig a) where def = defaultGSConfig
instance Default Theme where def = defaultTheme
instance Default PP where def = defaultPP
instance (a ~ Choose Tall (Choose (Mirror Tall) Full)) => Default (XConfig a)
  where def = defaultConfig
#endif
