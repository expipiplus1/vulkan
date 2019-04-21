{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_shader_float_controls
  ( VkPhysicalDeviceFloatControlsPropertiesKHR(..)
  , pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
  , pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )


-- | VkPhysicalDeviceFloatControlsPropertiesKHR - Structure describing
-- properties supported by VK_KHR_shader_float_controls
--
-- = Description
--
-- If the 'VkPhysicalDeviceFloatControlsPropertiesKHR' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in VkPhysicalDeviceFloatControlsPropertiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceFloatControlsPropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceFloatControlsPropertiesKHR = VkPhysicalDeviceFloatControlsPropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "pNext"
  vkPNext :: Ptr ()
  , -- | @separateDenormSettings@ is a boolean value indicating whether the
  -- implementation supports separate settings for 16-bit and 64-bit
  -- denormals.
  vkSeparateDenormSettings :: VkBool32
  , -- | @separateRoundingModeSettings@ is a boolean value indicating whether the
  -- implementation supports separate rounding modes for 16-bit and 64-bit
  -- floating point instructions.
  vkSeparateRoundingModeSettings :: VkBool32
  , -- | @shaderSignedZeroInfNanPreserveFloat16@ is a boolean value indicating
  -- whether sign of a zero, Nans and \(\pm\infty\) /can/ be preserved in
  -- 16-bit floating-point computations. It also indicates whether the
  -- @SignedZeroInfNanPreserve@ execution mode /can/ be used for 16-bit
  -- floating-point types.
  vkShaderSignedZeroInfNanPreserveFloat16 :: VkBool32
  , -- | @shaderSignedZeroInfNanPreserveFloat32@ is a boolean value indicating
  -- whether sign of a zero, Nans and \(\pm\infty\) /can/ be preserved in
  -- 32-bit floating-point computations. It also indicates whether the
  -- @SignedZeroInfNanPreserve@ execution mode /can/ be used for 32-bit
  -- floating-point types.
  vkShaderSignedZeroInfNanPreserveFloat32 :: VkBool32
  , -- | @shaderSignedZeroInfNanPreserveFloat64@ is a boolean value indicating
  -- whether sign of a zero, Nans and \(\pm\infty\) /can/ be preserved in
  -- 64-bit floating-point computations. It also indicates whether the
  -- @SignedZeroInfNanPreserve@ execution mode /can/ be used for 64-bit
  -- floating-point types.
  vkShaderSignedZeroInfNanPreserveFloat64 :: VkBool32
  , -- | @shaderDenormPreserveFloat16@ is a boolean value indicating whether
  -- denormals /can/ be preserved in 16-bit floating-point computations. It
  -- also indicates whether the @DenormPreserve@ execution mode /can/ be used
  -- for 16-bit floating-point types.
  vkShaderDenormPreserveFloat16 :: VkBool32
  , -- | @shaderDenormPreserveFloat32@ is a boolean value indicating whether
  -- denormals /can/ be preserved in 32-bit floating-point computations. It
  -- also indicates whether the @DenormPreserve@ execution mode /can/ be used
  -- for 32-bit floating-point types.
  vkShaderDenormPreserveFloat32 :: VkBool32
  , -- | @shaderDenormPreserveFloat64@ is a boolean value indicating whether
  -- denormals /can/ be preserved in 64-bit floating-point computations. It
  -- also indicates whether the @DenormPreserve@ execution mode /can/ be used
  -- for 64-bit floating-point types.
  vkShaderDenormPreserveFloat64 :: VkBool32
  , -- | @shaderDenormFlushToZeroFloat16@ is a boolean value indicating whether
  -- denormals /can/ be flushed to zero in 16-bit floating-point
  -- computations. It also indicates whether the @DenormFlushToZero@
  -- execution mode /can/ be used for 16-bit floating-point types.
  vkShaderDenormFlushToZeroFloat16 :: VkBool32
  , -- | @shaderDenormFlushToZeroFloat32@ is a boolean value indicating whether
  -- denormals /can/ be flushed to zero in 32-bit floating-point
  -- computations. It also indicates whether the @DenormFlushToZero@
  -- execution mode /can/ be used for 32-bit floating-point types.
  vkShaderDenormFlushToZeroFloat32 :: VkBool32
  , -- | @shaderDenormFlushToZeroFloat64@ is a boolean value indicating whether
  -- denormals /can/ be flushed to zero in 64-bit floating-point
  -- computations. It also indicates whether the @DenormFlushToZero@
  -- execution mode /can/ be used for 64-bit floating-point types.
  vkShaderDenormFlushToZeroFloat64 :: VkBool32
  , -- | @shaderRoundingModeRTEFloat16@ is a boolean value indicating whether an
  -- implementation supports the round-to-nearest-even rounding mode for
  -- 16-bit floating-point arithmetic and conversion instructions. It also
  -- indicates whether the @RoundingModeRTE@ execution mode /can/ be used for
  -- 16-bit floating-point types.
  vkShaderRoundingModeRTEFloat16 :: VkBool32
  , -- | @shaderRoundingModeRTEFloat32@ is a boolean value indicating whether an
  -- implementation supports the round-to-nearest-even rounding mode for
  -- 32-bit floating-point arithmetic and conversion instructions. It also
  -- indicates whether the @RoundingModeRTE@ execution mode /can/ be used for
  -- 32-bit floating-point types.
  vkShaderRoundingModeRTEFloat32 :: VkBool32
  , -- | @shaderRoundingModeRTEFloat64@ is a boolean value indicating whether an
  -- implementation supports the round-to-nearest-even rounding mode for
  -- 64-bit floating-point arithmetic and conversion instructions. It also
  -- indicates whether the @RoundingModeRTE@ execution mode /can/ be used for
  -- 64-bit floating-point types.
  vkShaderRoundingModeRTEFloat64 :: VkBool32
  , -- | @shaderRoundingModeRTZFloat16@ is a boolean value indicating whether an
  -- implementation supports the round-towards-zero rounding mode for 16-bit
  -- floating-point arithmetic and conversion instructions. It also indicates
  -- whether the @RoundingModeRTZ@ execution mode /can/ be used for 16-bit
  -- floating-point types.
  vkShaderRoundingModeRTZFloat16 :: VkBool32
  , -- | @shaderRoundingModeRTZFloat32@ is a boolean value indicating whether an
  -- implementation supports the round-towards-zero rounding mode for 32-bit
  -- floating-point arithmetic and conversion instructions. It also indicates
  -- whether the @RoundingModeRTZ@ execution mode /can/ be used for 32-bit
  -- floating-point types.
  vkShaderRoundingModeRTZFloat32 :: VkBool32
  , -- | @shaderRoundingModeRTZFloat64@ is a boolean value indicating whether an
  -- implementation supports the round-towards-zero rounding mode for 64-bit
  -- floating-point arithmetic and conversion instructions. It also indicates
  -- whether the @RoundingModeRTZ@ execution mode /can/ be used for 64-bit
  -- floating-point types.
  vkShaderRoundingModeRTZFloat64 :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceFloatControlsPropertiesKHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceFloatControlsPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
                                                        <*> peek (ptr `plusPtr` 20)
                                                        <*> peek (ptr `plusPtr` 24)
                                                        <*> peek (ptr `plusPtr` 28)
                                                        <*> peek (ptr `plusPtr` 32)
                                                        <*> peek (ptr `plusPtr` 36)
                                                        <*> peek (ptr `plusPtr` 40)
                                                        <*> peek (ptr `plusPtr` 44)
                                                        <*> peek (ptr `plusPtr` 48)
                                                        <*> peek (ptr `plusPtr` 52)
                                                        <*> peek (ptr `plusPtr` 56)
                                                        <*> peek (ptr `plusPtr` 60)
                                                        <*> peek (ptr `plusPtr` 64)
                                                        <*> peek (ptr `plusPtr` 68)
                                                        <*> peek (ptr `plusPtr` 72)
                                                        <*> peek (ptr `plusPtr` 76)
                                                        <*> peek (ptr `plusPtr` 80)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkSeparateDenormSettings (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 20) (vkSeparateRoundingModeSettings (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 24) (vkShaderSignedZeroInfNanPreserveFloat16 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 28) (vkShaderSignedZeroInfNanPreserveFloat32 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 32) (vkShaderSignedZeroInfNanPreserveFloat64 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 36) (vkShaderDenormPreserveFloat16 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 40) (vkShaderDenormPreserveFloat32 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 44) (vkShaderDenormPreserveFloat64 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 48) (vkShaderDenormFlushToZeroFloat16 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 52) (vkShaderDenormFlushToZeroFloat32 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 56) (vkShaderDenormFlushToZeroFloat64 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 60) (vkShaderRoundingModeRTEFloat16 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 64) (vkShaderRoundingModeRTEFloat32 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 68) (vkShaderRoundingModeRTEFloat64 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 72) (vkShaderRoundingModeRTZFloat16 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 76) (vkShaderRoundingModeRTZFloat32 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 80) (vkShaderRoundingModeRTZFloat64 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))

instance Zero VkPhysicalDeviceFloatControlsPropertiesKHR where
  zero = VkPhysicalDeviceFloatControlsPropertiesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME"
pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME = "VK_KHR_shader_float_controls"

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION"
pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR = VkStructureType 1000197000
