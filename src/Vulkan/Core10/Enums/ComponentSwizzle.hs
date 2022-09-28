{-# language CPP #-}
-- No documentation found for Chapter "ComponentSwizzle"
module Vulkan.Core10.Enums.ComponentSwizzle  (ComponentSwizzle( COMPONENT_SWIZZLE_IDENTITY
                                                              , COMPONENT_SWIZZLE_ZERO
                                                              , COMPONENT_SWIZZLE_ONE
                                                              , COMPONENT_SWIZZLE_R
                                                              , COMPONENT_SWIZZLE_G
                                                              , COMPONENT_SWIZZLE_B
                                                              , COMPONENT_SWIZZLE_A
                                                              , ..
                                                              )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkComponentSwizzle - Specify how a component is swizzled
--
-- = Description
--
-- Setting the identity swizzle on a component is equivalent to setting the
-- identity mapping on that component. That is:
--
-- +-----------------------------------+-----------------------------------+
-- | Component                         | Identity Mapping                  |
-- +===================================+===================================+
-- | @components.r@                    | 'COMPONENT_SWIZZLE_R'             |
-- +-----------------------------------+-----------------------------------+
-- | @components.g@                    | 'COMPONENT_SWIZZLE_G'             |
-- +-----------------------------------+-----------------------------------+
-- | @components.b@                    | 'COMPONENT_SWIZZLE_B'             |
-- +-----------------------------------+-----------------------------------+
-- | @components.a@                    | 'COMPONENT_SWIZZLE_A'             |
-- +-----------------------------------+-----------------------------------+
--
-- Component Mappings Equivalent To 'COMPONENT_SWIZZLE_IDENTITY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.ImageView.ComponentMapping'
newtype ComponentSwizzle = ComponentSwizzle Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COMPONENT_SWIZZLE_IDENTITY' specifies that the component is set to the
-- identity swizzle.
pattern COMPONENT_SWIZZLE_IDENTITY = ComponentSwizzle 0

-- | 'COMPONENT_SWIZZLE_ZERO' specifies that the component is set to zero.
pattern COMPONENT_SWIZZLE_ZERO = ComponentSwizzle 1

-- | 'COMPONENT_SWIZZLE_ONE' specifies that the component is set to either 1
-- or 1.0, depending on whether the type of the image view format is
-- integer or floating-point respectively, as determined by the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-definition Format Definition>
-- section for each 'Vulkan.Core10.Enums.Format.Format'.
pattern COMPONENT_SWIZZLE_ONE = ComponentSwizzle 2

-- | 'COMPONENT_SWIZZLE_R' specifies that the component is set to the value
-- of the R component of the image.
pattern COMPONENT_SWIZZLE_R = ComponentSwizzle 3

-- | 'COMPONENT_SWIZZLE_G' specifies that the component is set to the value
-- of the G component of the image.
pattern COMPONENT_SWIZZLE_G = ComponentSwizzle 4

-- | 'COMPONENT_SWIZZLE_B' specifies that the component is set to the value
-- of the B component of the image.
pattern COMPONENT_SWIZZLE_B = ComponentSwizzle 5

-- | 'COMPONENT_SWIZZLE_A' specifies that the component is set to the value
-- of the A component of the image.
pattern COMPONENT_SWIZZLE_A = ComponentSwizzle 6

{-# COMPLETE
  COMPONENT_SWIZZLE_IDENTITY
  , COMPONENT_SWIZZLE_ZERO
  , COMPONENT_SWIZZLE_ONE
  , COMPONENT_SWIZZLE_R
  , COMPONENT_SWIZZLE_G
  , COMPONENT_SWIZZLE_B
  , COMPONENT_SWIZZLE_A ::
    ComponentSwizzle
  #-}

conNameComponentSwizzle :: String
conNameComponentSwizzle = "ComponentSwizzle"

enumPrefixComponentSwizzle :: String
enumPrefixComponentSwizzle = "COMPONENT_SWIZZLE_"

showTableComponentSwizzle :: [(ComponentSwizzle, String)]
showTableComponentSwizzle =
  [ (COMPONENT_SWIZZLE_IDENTITY, "IDENTITY")
  , (COMPONENT_SWIZZLE_ZERO, "ZERO")
  , (COMPONENT_SWIZZLE_ONE, "ONE")
  , (COMPONENT_SWIZZLE_R, "R")
  , (COMPONENT_SWIZZLE_G, "G")
  , (COMPONENT_SWIZZLE_B, "B")
  , (COMPONENT_SWIZZLE_A, "A")
  ]

instance Show ComponentSwizzle where
  showsPrec =
    enumShowsPrec
      enumPrefixComponentSwizzle
      showTableComponentSwizzle
      conNameComponentSwizzle
      (\(ComponentSwizzle x) -> x)
      (showsPrec 11)

instance Read ComponentSwizzle where
  readPrec =
    enumReadPrec
      enumPrefixComponentSwizzle
      showTableComponentSwizzle
      conNameComponentSwizzle
      ComponentSwizzle
