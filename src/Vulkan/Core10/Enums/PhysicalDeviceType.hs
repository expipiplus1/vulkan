{-# language CPP #-}
-- No documentation found for Chapter "PhysicalDeviceType"
module Vulkan.Core10.Enums.PhysicalDeviceType  (PhysicalDeviceType( PHYSICAL_DEVICE_TYPE_OTHER
                                                                  , PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
                                                                  , PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
                                                                  , PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
                                                                  , PHYSICAL_DEVICE_TYPE_CPU
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
-- No documentation found for TopLevel "VkPhysicalDeviceType"
newtype PhysicalDeviceType = PhysicalDeviceType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPhysicalDeviceType" "VK_PHYSICAL_DEVICE_TYPE_OTHER"
pattern PHYSICAL_DEVICE_TYPE_OTHER          = PhysicalDeviceType 0
-- No documentation found for Nested "VkPhysicalDeviceType" "VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU"
pattern PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = PhysicalDeviceType 1
-- No documentation found for Nested "VkPhysicalDeviceType" "VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU"
pattern PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   = PhysicalDeviceType 2
-- No documentation found for Nested "VkPhysicalDeviceType" "VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU"
pattern PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU    = PhysicalDeviceType 3
-- No documentation found for Nested "VkPhysicalDeviceType" "VK_PHYSICAL_DEVICE_TYPE_CPU"
pattern PHYSICAL_DEVICE_TYPE_CPU            = PhysicalDeviceType 4
{-# complete PHYSICAL_DEVICE_TYPE_OTHER,
             PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU,
             PHYSICAL_DEVICE_TYPE_DISCRETE_GPU,
             PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU,
             PHYSICAL_DEVICE_TYPE_CPU :: PhysicalDeviceType #-}

conNamePhysicalDeviceType :: String
conNamePhysicalDeviceType = "PhysicalDeviceType"

enumPrefixPhysicalDeviceType :: String
enumPrefixPhysicalDeviceType = "PHYSICAL_DEVICE_TYPE_"

showTablePhysicalDeviceType :: [(PhysicalDeviceType, String)]
showTablePhysicalDeviceType =
  [ (PHYSICAL_DEVICE_TYPE_OTHER         , "OTHER")
  , (PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU, "INTEGRATED_GPU")
  , (PHYSICAL_DEVICE_TYPE_DISCRETE_GPU  , "DISCRETE_GPU")
  , (PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU   , "VIRTUAL_GPU")
  , (PHYSICAL_DEVICE_TYPE_CPU           , "CPU")
  ]


instance Show PhysicalDeviceType where
showsPrec = enumShowsPrec enumPrefixPhysicalDeviceType
                          showTablePhysicalDeviceType
                          conNamePhysicalDeviceType
                          (\(PhysicalDeviceType x) -> x)
                          (showsPrec 11)


instance Read PhysicalDeviceType where
  readPrec =
    enumReadPrec enumPrefixPhysicalDeviceType showTablePhysicalDeviceType conNamePhysicalDeviceType PhysicalDeviceType

