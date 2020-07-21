{-# language CPP #-}
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
                                              , ..
                                              )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
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
-- 'Vulkan.Core12.Promoted_From_VK_KHR_driver_properties.PhysicalDeviceDriverProperties',
-- 'Vulkan.Core12.PhysicalDeviceVulkan12Properties'
newtype DriverId = DriverId Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_AMD_PROPRIETARY"
pattern DRIVER_ID_AMD_PROPRIETARY = DriverId 1
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_AMD_OPEN_SOURCE"
pattern DRIVER_ID_AMD_OPEN_SOURCE = DriverId 2
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_MESA_RADV"
pattern DRIVER_ID_MESA_RADV = DriverId 3
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_NVIDIA_PROPRIETARY"
pattern DRIVER_ID_NVIDIA_PROPRIETARY = DriverId 4
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS"
pattern DRIVER_ID_INTEL_PROPRIETARY_WINDOWS = DriverId 5
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA"
pattern DRIVER_ID_INTEL_OPEN_SOURCE_MESA = DriverId 6
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_IMAGINATION_PROPRIETARY"
pattern DRIVER_ID_IMAGINATION_PROPRIETARY = DriverId 7
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_QUALCOMM_PROPRIETARY"
pattern DRIVER_ID_QUALCOMM_PROPRIETARY = DriverId 8
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_ARM_PROPRIETARY"
pattern DRIVER_ID_ARM_PROPRIETARY = DriverId 9
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_GOOGLE_SWIFTSHADER"
pattern DRIVER_ID_GOOGLE_SWIFTSHADER = DriverId 10
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_GGP_PROPRIETARY"
pattern DRIVER_ID_GGP_PROPRIETARY = DriverId 11
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_BROADCOM_PROPRIETARY"
pattern DRIVER_ID_BROADCOM_PROPRIETARY = DriverId 12
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_MESA_LLVMPIPE"
pattern DRIVER_ID_MESA_LLVMPIPE = DriverId 13
-- No documentation found for Nested "VkDriverId" "VK_DRIVER_ID_MOLTENVK"
pattern DRIVER_ID_MOLTENVK = DriverId 14
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
             DRIVER_ID_MOLTENVK :: DriverId #-}

instance Show DriverId where
  showsPrec p = \case
    DRIVER_ID_AMD_PROPRIETARY -> showString "DRIVER_ID_AMD_PROPRIETARY"
    DRIVER_ID_AMD_OPEN_SOURCE -> showString "DRIVER_ID_AMD_OPEN_SOURCE"
    DRIVER_ID_MESA_RADV -> showString "DRIVER_ID_MESA_RADV"
    DRIVER_ID_NVIDIA_PROPRIETARY -> showString "DRIVER_ID_NVIDIA_PROPRIETARY"
    DRIVER_ID_INTEL_PROPRIETARY_WINDOWS -> showString "DRIVER_ID_INTEL_PROPRIETARY_WINDOWS"
    DRIVER_ID_INTEL_OPEN_SOURCE_MESA -> showString "DRIVER_ID_INTEL_OPEN_SOURCE_MESA"
    DRIVER_ID_IMAGINATION_PROPRIETARY -> showString "DRIVER_ID_IMAGINATION_PROPRIETARY"
    DRIVER_ID_QUALCOMM_PROPRIETARY -> showString "DRIVER_ID_QUALCOMM_PROPRIETARY"
    DRIVER_ID_ARM_PROPRIETARY -> showString "DRIVER_ID_ARM_PROPRIETARY"
    DRIVER_ID_GOOGLE_SWIFTSHADER -> showString "DRIVER_ID_GOOGLE_SWIFTSHADER"
    DRIVER_ID_GGP_PROPRIETARY -> showString "DRIVER_ID_GGP_PROPRIETARY"
    DRIVER_ID_BROADCOM_PROPRIETARY -> showString "DRIVER_ID_BROADCOM_PROPRIETARY"
    DRIVER_ID_MESA_LLVMPIPE -> showString "DRIVER_ID_MESA_LLVMPIPE"
    DRIVER_ID_MOLTENVK -> showString "DRIVER_ID_MOLTENVK"
    DriverId x -> showParen (p >= 11) (showString "DriverId " . showsPrec 11 x)

instance Read DriverId where
  readPrec = parens (choose [("DRIVER_ID_AMD_PROPRIETARY", pure DRIVER_ID_AMD_PROPRIETARY)
                            , ("DRIVER_ID_AMD_OPEN_SOURCE", pure DRIVER_ID_AMD_OPEN_SOURCE)
                            , ("DRIVER_ID_MESA_RADV", pure DRIVER_ID_MESA_RADV)
                            , ("DRIVER_ID_NVIDIA_PROPRIETARY", pure DRIVER_ID_NVIDIA_PROPRIETARY)
                            , ("DRIVER_ID_INTEL_PROPRIETARY_WINDOWS", pure DRIVER_ID_INTEL_PROPRIETARY_WINDOWS)
                            , ("DRIVER_ID_INTEL_OPEN_SOURCE_MESA", pure DRIVER_ID_INTEL_OPEN_SOURCE_MESA)
                            , ("DRIVER_ID_IMAGINATION_PROPRIETARY", pure DRIVER_ID_IMAGINATION_PROPRIETARY)
                            , ("DRIVER_ID_QUALCOMM_PROPRIETARY", pure DRIVER_ID_QUALCOMM_PROPRIETARY)
                            , ("DRIVER_ID_ARM_PROPRIETARY", pure DRIVER_ID_ARM_PROPRIETARY)
                            , ("DRIVER_ID_GOOGLE_SWIFTSHADER", pure DRIVER_ID_GOOGLE_SWIFTSHADER)
                            , ("DRIVER_ID_GGP_PROPRIETARY", pure DRIVER_ID_GGP_PROPRIETARY)
                            , ("DRIVER_ID_BROADCOM_PROPRIETARY", pure DRIVER_ID_BROADCOM_PROPRIETARY)
                            , ("DRIVER_ID_MESA_LLVMPIPE", pure DRIVER_ID_MESA_LLVMPIPE)
                            , ("DRIVER_ID_MOLTENVK", pure DRIVER_ID_MOLTENVK)]
                     +++
                     prec 10 (do
                       expectP (Ident "DriverId")
                       v <- step readPrec
                       pure (DriverId v)))

