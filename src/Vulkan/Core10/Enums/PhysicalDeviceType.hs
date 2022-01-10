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
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkPhysicalDeviceType - Supported physical device types
--
-- = Description
--
-- The physical device type is advertised for informational purposes only,
-- and does not directly affect the operation of the system. However, the
-- device type /may/ correlate with other advertised properties or
-- capabilities of the system, such as how many memory heaps there are.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'
newtype PhysicalDeviceType = PhysicalDeviceType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PHYSICAL_DEVICE_TYPE_OTHER' - the device does not match any other
-- available types.
pattern PHYSICAL_DEVICE_TYPE_OTHER          = PhysicalDeviceType 0
-- | 'PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU' - the device is typically one
-- embedded in or tightly coupled with the host.
pattern PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = PhysicalDeviceType 1
-- | 'PHYSICAL_DEVICE_TYPE_DISCRETE_GPU' - the device is typically a separate
-- processor connected to the host via an interlink.
pattern PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   = PhysicalDeviceType 2
-- | 'PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU' - the device is typically a virtual
-- node in a virtualization environment.
pattern PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU    = PhysicalDeviceType 3
-- | 'PHYSICAL_DEVICE_TYPE_CPU' - the device is typically running on the same
-- processors as the host.
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

