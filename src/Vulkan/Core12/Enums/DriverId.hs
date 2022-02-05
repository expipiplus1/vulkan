{-# language CPP #-}
-- No documentation found for Chapter "DriverId"
module Vulkan.Core12.Enums.DriverId  (DriverId( DRIVER_ID_AMD_PROPRIETARY
                                              , DRIVER_ID_AMD_OPEN_SOURCE
                                              , DRIVER_ID_MESA_RADV
                                              , DRIVER_ID_NVIDIA_PROPRIETARY
                                              , DRIVER_ID_INTEL_PROPRIETARY_WINDOWS
                                              , DRIVER_ID_INTEL_OPEN_SOURCE_MESA
                                              , DRIVER_ID_IMAGINATION_PROPRIETARY
                                              , DRIVER_ID_QUALCOMM_PROPRIETARY
                                              , DRIVER_ID_ARM_PROPRIETARY
                                              , DRIVER_ID_GOOGLE_SWIFTSHADER
                                              , DRIVER_ID_GGP_PROPRIETARY
                                              , DRIVER_ID_BROADCOM_PROPRIETARY
                                              , DRIVER_ID_MESA_LLVMPIPE
                                              , DRIVER_ID_MOLTENVK
                                              , DRIVER_ID_COREAVI_PROPRIETARY
                                              , DRIVER_ID_JUICE_PROPRIETARY
                                              , DRIVER_ID_VERISILICON_PROPRIETARY
                                              , DRIVER_ID_MESA_TURNIP
                                              , DRIVER_ID_MESA_V3DV
                                              , DRIVER_ID_MESA_PANVK
                                              , DRIVER_ID_SAMSUNG_PROPRIETARY
                                              , DRIVER_ID_MESA_VENUS
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

-- | VkDriverId - Khronos driver IDs
--
-- = Description
--
-- Note
--
-- Khronos driver IDs may be allocated by vendors at any time. There may be
-- multiple driver IDs for the same vendor, representing different drivers
-- (for e.g. different platforms, proprietary or open source, etc.). Only
-- the latest canonical versions of this Specification, of the
-- corresponding @vk.xml@ API Registry, and of the corresponding
-- @vulkan_core.h@ header file /must/ contain all reserved Khronos driver
-- IDs.
--
-- Only driver IDs registered with Khronos are given symbolic names. There
-- /may/ be unregistered driver IDs returned.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_driver_properties VK_KHR_driver_properties>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core12.Promoted_From_VK_KHR_driver_properties.PhysicalDeviceDriverProperties',
-- 'Vulkan.Core12.PhysicalDeviceVulkan12Properties'
newtype DriverId = DriverId Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_AMD_PROPRIETARY"
pattern DRIVER_ID_AMD_PROPRIETARY           = DriverId 1
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_AMD_OPEN_SOURCE"
pattern DRIVER_ID_AMD_OPEN_SOURCE           = DriverId 2
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_MESA_RADV"
pattern DRIVER_ID_MESA_RADV                 = DriverId 3
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_NVIDIA_PROPRIETARY"
pattern DRIVER_ID_NVIDIA_PROPRIETARY        = DriverId 4
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS"
pattern DRIVER_ID_INTEL_PROPRIETARY_WINDOWS = DriverId 5
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA"
pattern DRIVER_ID_INTEL_OPEN_SOURCE_MESA    = DriverId 6
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_IMAGINATION_PROPRIETARY"
pattern DRIVER_ID_IMAGINATION_PROPRIETARY   = DriverId 7
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_QUALCOMM_PROPRIETARY"
pattern DRIVER_ID_QUALCOMM_PROPRIETARY      = DriverId 8
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_ARM_PROPRIETARY"
pattern DRIVER_ID_ARM_PROPRIETARY           = DriverId 9
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_GOOGLE_SWIFTSHADER"
pattern DRIVER_ID_GOOGLE_SWIFTSHADER        = DriverId 10
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_GGP_PROPRIETARY"
pattern DRIVER_ID_GGP_PROPRIETARY           = DriverId 11
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_BROADCOM_PROPRIETARY"
pattern DRIVER_ID_BROADCOM_PROPRIETARY      = DriverId 12
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_MESA_LLVMPIPE"
pattern DRIVER_ID_MESA_LLVMPIPE             = DriverId 13
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_MOLTENVK"
pattern DRIVER_ID_MOLTENVK                  = DriverId 14
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_COREAVI_PROPRIETARY"
pattern DRIVER_ID_COREAVI_PROPRIETARY       = DriverId 15
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_JUICE_PROPRIETARY"
pattern DRIVER_ID_JUICE_PROPRIETARY         = DriverId 16
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_VERISILICON_PROPRIETARY"
pattern DRIVER_ID_VERISILICON_PROPRIETARY   = DriverId 17
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_MESA_TURNIP"
pattern DRIVER_ID_MESA_TURNIP               = DriverId 18
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_MESA_V3DV"
pattern DRIVER_ID_MESA_V3DV                 = DriverId 19
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_MESA_PANVK"
pattern DRIVER_ID_MESA_PANVK                = DriverId 20
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_SAMSUNG_PROPRIETARY"
pattern DRIVER_ID_SAMSUNG_PROPRIETARY       = DriverId 21
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_MESA_VENUS"
pattern DRIVER_ID_MESA_VENUS                = DriverId 22
{-# complete DRIVER_ID_AMD_PROPRIETARY,
             DRIVER_ID_AMD_OPEN_SOURCE,
             DRIVER_ID_MESA_RADV,
             DRIVER_ID_NVIDIA_PROPRIETARY,
             DRIVER_ID_INTEL_PROPRIETARY_WINDOWS,
             DRIVER_ID_INTEL_OPEN_SOURCE_MESA,
             DRIVER_ID_IMAGINATION_PROPRIETARY,
             DRIVER_ID_QUALCOMM_PROPRIETARY,
             DRIVER_ID_ARM_PROPRIETARY,
             DRIVER_ID_GOOGLE_SWIFTSHADER,
             DRIVER_ID_GGP_PROPRIETARY,
             DRIVER_ID_BROADCOM_PROPRIETARY,
             DRIVER_ID_MESA_LLVMPIPE,
             DRIVER_ID_MOLTENVK,
             DRIVER_ID_COREAVI_PROPRIETARY,
             DRIVER_ID_JUICE_PROPRIETARY,
             DRIVER_ID_VERISILICON_PROPRIETARY,
             DRIVER_ID_MESA_TURNIP,
             DRIVER_ID_MESA_V3DV,
             DRIVER_ID_MESA_PANVK,
             DRIVER_ID_SAMSUNG_PROPRIETARY,
             DRIVER_ID_MESA_VENUS :: DriverId #-}

conNameDriverId :: String
conNameDriverId = "DriverId"

enumPrefixDriverId :: String
enumPrefixDriverId = "DRIVER_ID_"

showTableDriverId :: [(DriverId, String)]
showTableDriverId =
  [ (DRIVER_ID_AMD_PROPRIETARY          , "AMD_PROPRIETARY")
  , (DRIVER_ID_AMD_OPEN_SOURCE          , "AMD_OPEN_SOURCE")
  , (DRIVER_ID_MESA_RADV                , "MESA_RADV")
  , (DRIVER_ID_NVIDIA_PROPRIETARY       , "NVIDIA_PROPRIETARY")
  , (DRIVER_ID_INTEL_PROPRIETARY_WINDOWS, "INTEL_PROPRIETARY_WINDOWS")
  , (DRIVER_ID_INTEL_OPEN_SOURCE_MESA   , "INTEL_OPEN_SOURCE_MESA")
  , (DRIVER_ID_IMAGINATION_PROPRIETARY  , "IMAGINATION_PROPRIETARY")
  , (DRIVER_ID_QUALCOMM_PROPRIETARY     , "QUALCOMM_PROPRIETARY")
  , (DRIVER_ID_ARM_PROPRIETARY          , "ARM_PROPRIETARY")
  , (DRIVER_ID_GOOGLE_SWIFTSHADER       , "GOOGLE_SWIFTSHADER")
  , (DRIVER_ID_GGP_PROPRIETARY          , "GGP_PROPRIETARY")
  , (DRIVER_ID_BROADCOM_PROPRIETARY     , "BROADCOM_PROPRIETARY")
  , (DRIVER_ID_MESA_LLVMPIPE            , "MESA_LLVMPIPE")
  , (DRIVER_ID_MOLTENVK                 , "MOLTENVK")
  , (DRIVER_ID_COREAVI_PROPRIETARY      , "COREAVI_PROPRIETARY")
  , (DRIVER_ID_JUICE_PROPRIETARY        , "JUICE_PROPRIETARY")
  , (DRIVER_ID_VERISILICON_PROPRIETARY  , "VERISILICON_PROPRIETARY")
  , (DRIVER_ID_MESA_TURNIP              , "MESA_TURNIP")
  , (DRIVER_ID_MESA_V3DV                , "MESA_V3DV")
  , (DRIVER_ID_MESA_PANVK               , "MESA_PANVK")
  , (DRIVER_ID_SAMSUNG_PROPRIETARY      , "SAMSUNG_PROPRIETARY")
  , (DRIVER_ID_MESA_VENUS               , "MESA_VENUS")
  ]

instance Show DriverId where
  showsPrec = enumShowsPrec enumPrefixDriverId showTableDriverId conNameDriverId (\(DriverId x) -> x) (showsPrec 11)

instance Read DriverId where
  readPrec = enumReadPrec enumPrefixDriverId showTableDriverId conNameDriverId DriverId

