{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_AMD_shader_info
  ( VkShaderInfoTypeAMD(..)
  , pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD
  , pattern VK_SHADER_INFO_TYPE_BINARY_AMD
  , pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD
  , pattern VK_AMD_SHADER_INFO_SPEC_VERSION
  , pattern VK_AMD_SHADER_INFO_EXTENSION_NAME
  , vkGetShaderInfoAMD
  , VkShaderResourceUsageAMD(..)
  , VkShaderStatisticsInfoAMD(..)
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkShaderStageFlagBits(..)
  , VkPipeline
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkShaderStageFlags
  )


-- ** VkShaderInfoTypeAMD

-- No documentation found for TopLevel "VkShaderInfoTypeAMD"
newtype VkShaderInfoTypeAMD = VkShaderInfoTypeAMD Int32
  deriving (Eq, Ord, Storable)

instance Show VkShaderInfoTypeAMD where
  showsPrec _ VK_SHADER_INFO_TYPE_STATISTICS_AMD = showString "VK_SHADER_INFO_TYPE_STATISTICS_AMD"
  showsPrec _ VK_SHADER_INFO_TYPE_BINARY_AMD = showString "VK_SHADER_INFO_TYPE_BINARY_AMD"
  showsPrec _ VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD = showString "VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD"
  showsPrec p (VkShaderInfoTypeAMD x) = showParen (p >= 11) (showString "VkShaderInfoTypeAMD " . showsPrec 11 x)

instance Read VkShaderInfoTypeAMD where
  readPrec = parens ( choose [ ("VK_SHADER_INFO_TYPE_STATISTICS_AMD",  pure VK_SHADER_INFO_TYPE_STATISTICS_AMD)
                             , ("VK_SHADER_INFO_TYPE_BINARY_AMD",      pure VK_SHADER_INFO_TYPE_BINARY_AMD)
                             , ("VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD", pure VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkShaderInfoTypeAMD")
                        v <- step readPrec
                        pure (VkShaderInfoTypeAMD v)
                        )
                    )

-- No documentation found for Nested "VkShaderInfoTypeAMD" "VK_SHADER_INFO_TYPE_STATISTICS_AMD"
pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD :: VkShaderInfoTypeAMD
pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD = VkShaderInfoTypeAMD 0

-- No documentation found for Nested "VkShaderInfoTypeAMD" "VK_SHADER_INFO_TYPE_BINARY_AMD"
pattern VK_SHADER_INFO_TYPE_BINARY_AMD :: VkShaderInfoTypeAMD
pattern VK_SHADER_INFO_TYPE_BINARY_AMD = VkShaderInfoTypeAMD 1

-- No documentation found for Nested "VkShaderInfoTypeAMD" "VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD"
pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD :: VkShaderInfoTypeAMD
pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD = VkShaderInfoTypeAMD 2
-- No documentation found for TopLevel "VK_AMD_SHADER_INFO_SPEC_VERSION"
pattern VK_AMD_SHADER_INFO_SPEC_VERSION :: Integral a => a
pattern VK_AMD_SHADER_INFO_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_AMD_SHADER_INFO_EXTENSION_NAME"
pattern VK_AMD_SHADER_INFO_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_SHADER_INFO_EXTENSION_NAME = "VK_AMD_shader_info"
-- | vkGetShaderInfoAMD - Get information about a shader in a pipeline
--
-- = Parameters
--
-- -   @device@ is the device that created @pipeline@.
--
-- -   @pipeline@ is the target of the query.
--
-- -   @shaderStage@ identifies the particular shader within the pipeline
--     about which information is being queried.
--
-- -   @infoType@ describes what kind of information is being queried.
--
-- -   @pInfoSize@ is a pointer to a value related to the amount of data
--     the query returns, as described below.
--
-- -   @pInfo@ is either NULL or a pointer to a buffer.
--
-- = Description
--
-- If @pInfo@ is @NULL@, then the maximum size of the information that
-- /can/ be retrieved about the shader, in bytes, is returned in
-- @pInfoSize@. Otherwise, @pInfoSize@ /must/ point to a variable set by
-- the user to the size of the buffer, in bytes, pointed to by @pInfo@, and
-- on return the variable is overwritten with the amount of data actually
-- written to @pInfo@.
--
-- If @pInfoSize@ is less than the maximum size that /can/ be retrieved by
-- the pipeline cache, then at most @pInfoSize@ bytes will be written to
-- @pInfo@, and @vkGetShaderInfoAMD@ will return @VK_INCOMPLETE@.
--
-- Not all information is available for every shader and implementations
-- may not support all kinds of information for any shader. When a certain
-- type of information is unavailable, the function returns
-- @VK_ERROR_FEATURE_NOT_PRESENT@.
--
-- If information is successfully and fully queried, the function will
-- return @VK_SUCCESS@.
--
-- For @VK_SHADER_INFO_TYPE_STATISTICS_AMD@, an instance of
-- @VkShaderStatisticsInfoAMD@ will be written to the buffer pointed to by
-- @pInfo@. This structure will be populated with statistics regarding the
-- physical device resources used by that shader along with other
-- miscellaneous information and is described in further detail below.
--
-- For @VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD@, @pInfo@ points to a UTF-8
-- null-terminated string containing human-readable disassembly. The exact
-- formatting and contents of the disassembly string are vendor-specific.
--
-- The formatting and contents of all other types of information, including
-- @VK_SHADER_INFO_TYPE_BINARY_AMD@, are left to the vendor and are not
-- further specified by this extension.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pipeline@ /must/ be a valid @VkPipeline@ handle
--
-- -   @shaderStage@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Pipeline.VkShaderStageFlagBits' value
--
-- -   @infoType@ /must/ be a valid 'VkShaderInfoTypeAMD' value
--
-- -   @pInfoSize@ /must/ be a valid pointer to a @size_t@ value
--
-- -   If the value referenced by @pInfoSize@ is not @0@, and @pInfo@ is
--     not @NULL@, @pInfo@ /must/ be a valid pointer to an array of
--     @pInfoSize@ bytes
--
-- -   @pipeline@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_FEATURE_NOT_PRESENT@
--
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipeline', 'VkShaderInfoTypeAMD',
-- 'Graphics.Vulkan.Core10.Pipeline.VkShaderStageFlagBits'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetShaderInfoAMD" vkGetShaderInfoAMD :: ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult
-- | VkShaderResourceUsageAMD - Resource usage information about a particular
-- shader within a pipeline
--
-- = See Also
--
-- 'VkShaderStatisticsInfoAMD'
data VkShaderResourceUsageAMD = VkShaderResourceUsageAMD
  { -- | @numUsedVgprs@ is the number of vector instruction general-purpose
  -- registers used by this shader.
  vkNumUsedVgprs :: Word32
  , -- | @numUsedSgprs@ is the number of scalar instruction general-purpose
  -- registers used by this shader.
  vkNumUsedSgprs :: Word32
  , -- | @ldsSizePerLocalWorkGroup@ is the maximum local data store size per work
  -- group in bytes.
  vkLdsSizePerLocalWorkGroup :: Word32
  , -- | @ldsUsageSizeInBytes@ is the LDS usage size in bytes per work group by
  -- this shader.
  vkLdsUsageSizeInBytes :: CSize
  , -- | @scratchMemUsageInBytes@ is the scratch memory usage in bytes by this
  -- shader.
  vkScratchMemUsageInBytes :: CSize
  }
  deriving (Eq, Show)

instance Storable VkShaderResourceUsageAMD where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkShaderResourceUsageAMD <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkNumUsedVgprs (poked :: VkShaderResourceUsageAMD))
                *> poke (ptr `plusPtr` 4) (vkNumUsedSgprs (poked :: VkShaderResourceUsageAMD))
                *> poke (ptr `plusPtr` 8) (vkLdsSizePerLocalWorkGroup (poked :: VkShaderResourceUsageAMD))
                *> poke (ptr `plusPtr` 16) (vkLdsUsageSizeInBytes (poked :: VkShaderResourceUsageAMD))
                *> poke (ptr `plusPtr` 24) (vkScratchMemUsageInBytes (poked :: VkShaderResourceUsageAMD))
-- | VkShaderStatisticsInfoAMD - Statistical information about a particular
-- shader within a pipeline
--
-- = Description
--
-- Some implementations may merge multiple logical shader stages together
-- in a single shader. In such cases, @shaderStageMask@ will contain a
-- bitmask of all of the stages that are active within that shader.
-- Consequently, if specifying those stages as input to
-- 'vkGetShaderInfoAMD', the same output information /may/ be returned for
-- all such shader stage queries.
--
-- The number of available VGPRs and SGPRs (@numAvailableVgprs@ and
-- @numAvailableSgprs@ respectively) are the shader-addressable subset of
-- physical registers that is given as a limit to the compiler for register
-- assignment. These values /may/ further be limited by implementations due
-- to performance optimizations where register pressure is a bottleneck.
--
-- = See Also
--
-- 'VkShaderResourceUsageAMD',
-- 'Graphics.Vulkan.Core10.PipelineLayout.VkShaderStageFlags'
data VkShaderStatisticsInfoAMD = VkShaderStatisticsInfoAMD
  { -- | @shaderStageMask@ are the combination of logical shader stages contained
  -- within this shader.
  vkShaderStageMask :: VkShaderStageFlags
  , -- | @resourceUsage@ is an instance of 'VkShaderResourceUsageAMD' describing
  -- internal physical device resources used by this shader.
  vkResourceUsage :: VkShaderResourceUsageAMD
  , -- | @numPhysicalVgprs@ is the maximum number of vector instruction
  -- general-purpose registers (VGPRs) available to the physical device.
  vkNumPhysicalVgprs :: Word32
  , -- | @numPhysicalSgprs@ is the maximum number of scalar instruction
  -- general-purpose registers (SGPRs) available to the physical device.
  vkNumPhysicalSgprs :: Word32
  , -- | @numAvailableVgprs@ is the maximum limit of VGPRs made available to the
  -- shader compiler.
  vkNumAvailableVgprs :: Word32
  , -- | @numAvailableSgprs@ is the maximum limit of SGPRs made available to the
  -- shader compiler.
  vkNumAvailableSgprs :: Word32
  , -- | @computeWorkGroupSize@ is the local workgroup size of this shader in {
  -- X, Y, Z } dimensions.
  vkComputeWorkGroupSize :: Vector 3 Word32
  }
  deriving (Eq, Show)

instance Storable VkShaderStatisticsInfoAMD where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkShaderStatisticsInfoAMD <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 40)
                                       <*> peek (ptr `plusPtr` 44)
                                       <*> peek (ptr `plusPtr` 48)
                                       <*> peek (ptr `plusPtr` 52)
                                       <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkShaderStageMask (poked :: VkShaderStatisticsInfoAMD))
                *> poke (ptr `plusPtr` 8) (vkResourceUsage (poked :: VkShaderStatisticsInfoAMD))
                *> poke (ptr `plusPtr` 40) (vkNumPhysicalVgprs (poked :: VkShaderStatisticsInfoAMD))
                *> poke (ptr `plusPtr` 44) (vkNumPhysicalSgprs (poked :: VkShaderStatisticsInfoAMD))
                *> poke (ptr `plusPtr` 48) (vkNumAvailableVgprs (poked :: VkShaderStatisticsInfoAMD))
                *> poke (ptr `plusPtr` 52) (vkNumAvailableSgprs (poked :: VkShaderStatisticsInfoAMD))
                *> poke (ptr `plusPtr` 56) (vkComputeWorkGroupSize (poked :: VkShaderStatisticsInfoAMD))
