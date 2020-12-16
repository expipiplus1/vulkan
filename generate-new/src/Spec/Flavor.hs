{-# LANGUAGE AllowAmbiguousTypes #-}
module Spec.Flavor where

import           Relude

data SpecFlavor
  = SpecVk
  | SpecXr

data SSpecFlavor t where
  SSpecVk ::SSpecFlavor SpecVk
  SSpecXr ::SSpecFlavor SpecXr

class KnownSpecFlavor (t :: SpecFlavor) where
  sSpecFlavor :: SSpecFlavor t
instance KnownSpecFlavor SpecVk where
  sSpecFlavor = SSpecVk
instance KnownSpecFlavor SpecXr where
  sSpecFlavor = SSpecXr

specFlavor :: forall t . KnownSpecFlavor t => SpecFlavor
specFlavor = case sSpecFlavor @t of
  SSpecVk -> SpecVk
  SSpecXr -> SpecXr

flavorPrefixCaps :: forall t . KnownSpecFlavor t => ByteString
flavorPrefixCaps = case sSpecFlavor @t of
  SSpecVk -> "VK"
  SSpecXr -> "XR"
