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
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkComponentSwizzle"
newtype ComponentSwizzle = ComponentSwizzle Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_IDENTITY"
pattern COMPONENT_SWIZZLE_IDENTITY = ComponentSwizzle 0
-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_ZERO"
pattern COMPONENT_SWIZZLE_ZERO     = ComponentSwizzle 1
-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_ONE"
pattern COMPONENT_SWIZZLE_ONE      = ComponentSwizzle 2
-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_R"
pattern COMPONENT_SWIZZLE_R        = ComponentSwizzle 3
-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_G"
pattern COMPONENT_SWIZZLE_G        = ComponentSwizzle 4
-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_B"
pattern COMPONENT_SWIZZLE_B        = ComponentSwizzle 5
-- No documentation found for Nested "VkComponentSwizzle" "VK_COMPONENT_SWIZZLE_A"
pattern COMPONENT_SWIZZLE_A        = ComponentSwizzle 6
{-# complete COMPONENT_SWIZZLE_IDENTITY,
             COMPONENT_SWIZZLE_ZERO,
             COMPONENT_SWIZZLE_ONE,
             COMPONENT_SWIZZLE_R,
             COMPONENT_SWIZZLE_G,
             COMPONENT_SWIZZLE_B,
             COMPONENT_SWIZZLE_A :: ComponentSwizzle #-}

conNameComponentSwizzle :: String
conNameComponentSwizzle = "ComponentSwizzle"

enumPrefixComponentSwizzle :: String
enumPrefixComponentSwizzle = "COMPONENT_SWIZZLE_"

showTableComponentSwizzle :: [(ComponentSwizzle, String)]
showTableComponentSwizzle =
  [ (COMPONENT_SWIZZLE_IDENTITY, "IDENTITY")
  , (COMPONENT_SWIZZLE_ZERO    , "ZERO")
  , (COMPONENT_SWIZZLE_ONE     , "ONE")
  , (COMPONENT_SWIZZLE_R       , "R")
  , (COMPONENT_SWIZZLE_G       , "G")
  , (COMPONENT_SWIZZLE_B       , "B")
  , (COMPONENT_SWIZZLE_A       , "A")
  ]


instance Show ComponentSwizzle where
showsPrec = enumShowsPrec enumPrefixComponentSwizzle
                          showTableComponentSwizzle
                          conNameComponentSwizzle
                          (\(ComponentSwizzle x) -> x)
                          (showsPrec 11)


instance Read ComponentSwizzle where
  readPrec = enumReadPrec enumPrefixComponentSwizzle showTableComponentSwizzle conNameComponentSwizzle ComponentSwizzle

