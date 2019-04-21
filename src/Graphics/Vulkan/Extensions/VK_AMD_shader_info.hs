{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_AMD_shader_info
  ( ShaderInfoTypeAMD
  , pattern SHADER_INFO_TYPE_STATISTICS_AMD
  , pattern SHADER_INFO_TYPE_BINARY_AMD
  , pattern SHADER_INFO_TYPE_DISASSEMBLY_AMD
  , withCStructShaderResourceUsageAMD
  , fromCStructShaderResourceUsageAMD
  , ShaderResourceUsageAMD(..)
  , withCStructShaderStatisticsInfoAMD
  , fromCStructShaderStatisticsInfoAMD
  , ShaderStatisticsInfoAMD(..)
  , getNumShaderInfoAMD
  , getShaderInfoAMD
  , getAllShaderInfoAMD
  , pattern VK_AMD_SHADER_INFO_SPEC_VERSION
  , pattern VK_AMD_SHADER_INFO_EXTENSION_NAME
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.ByteString
  ( ByteString
  , packCStringLen
  )
import Data.Vector.Generic.Sized
  ( fromTuple
  )
import qualified Data.Vector.Storable.Sized
  ( unsafeIndex
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_info
  ( VkShaderInfoTypeAMD(..)
  , VkShaderResourceUsageAMD(..)
  , VkShaderStatisticsInfoAMD(..)
  , vkGetShaderInfoAMD
  , pattern VK_SHADER_INFO_TYPE_BINARY_AMD
  , pattern VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD
  , pattern VK_SHADER_INFO_TYPE_STATISTICS_AMD
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Pipeline
  , ShaderStageFlagBits
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( ShaderStageFlags
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_info
  ( pattern VK_AMD_SHADER_INFO_EXTENSION_NAME
  , pattern VK_AMD_SHADER_INFO_SPEC_VERSION
  )


-- | VkShaderInfoTypeAMD - Enum specifying which type of shader info to query
--
-- = See Also
--
-- No cross-references are available
type ShaderInfoTypeAMD = VkShaderInfoTypeAMD


-- | 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VK_SHADER_INFO_TYPE_STATISTICS_AMD'
-- specifies that device resources used by a shader will be queried.
pattern SHADER_INFO_TYPE_STATISTICS_AMD :: (a ~ ShaderInfoTypeAMD) => a
pattern SHADER_INFO_TYPE_STATISTICS_AMD = VK_SHADER_INFO_TYPE_STATISTICS_AMD


-- | 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VK_SHADER_INFO_TYPE_BINARY_AMD'
-- specifies that implementation-specific information will be queried.
pattern SHADER_INFO_TYPE_BINARY_AMD :: (a ~ ShaderInfoTypeAMD) => a
pattern SHADER_INFO_TYPE_BINARY_AMD = VK_SHADER_INFO_TYPE_BINARY_AMD


-- | 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD'
-- specifies that human-readable dissassembly of a shader.
pattern SHADER_INFO_TYPE_DISASSEMBLY_AMD :: (a ~ ShaderInfoTypeAMD) => a
pattern SHADER_INFO_TYPE_DISASSEMBLY_AMD = VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD


-- | VkShaderResourceUsageAMD - Resource usage information about a particular
-- shader within a pipeline
--
-- = Description
--
-- Unresolved directive in VkShaderResourceUsageAMD.txt -
-- include::{generated}\/validity\/structs\/VkShaderResourceUsageAMD.txt[]
--
-- = See Also
--
-- No cross-references are available
data ShaderResourceUsageAMD = ShaderResourceUsageAMD
  { -- No documentation found for Nested "ShaderResourceUsageAMD" "numUsedVgprs"
  numUsedVgprs :: Word32
  , -- No documentation found for Nested "ShaderResourceUsageAMD" "numUsedSgprs"
  numUsedSgprs :: Word32
  , -- No documentation found for Nested "ShaderResourceUsageAMD" "ldsSizePerLocalWorkGroup"
  ldsSizePerLocalWorkGroup :: Word32
  , -- No documentation found for Nested "ShaderResourceUsageAMD" "ldsUsageSizeInBytes"
  ldsUsageSizeInBytes :: CSize
  , -- No documentation found for Nested "ShaderResourceUsageAMD" "scratchMemUsageInBytes"
  scratchMemUsageInBytes :: CSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkShaderResourceUsageAMD' and
-- marshal a 'ShaderResourceUsageAMD' into it. The 'VkShaderResourceUsageAMD' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructShaderResourceUsageAMD :: ShaderResourceUsageAMD -> (VkShaderResourceUsageAMD -> IO a) -> IO a
withCStructShaderResourceUsageAMD marshalled cont = cont (VkShaderResourceUsageAMD (numUsedVgprs (marshalled :: ShaderResourceUsageAMD)) (numUsedSgprs (marshalled :: ShaderResourceUsageAMD)) (ldsSizePerLocalWorkGroup (marshalled :: ShaderResourceUsageAMD)) (ldsUsageSizeInBytes (marshalled :: ShaderResourceUsageAMD)) (scratchMemUsageInBytes (marshalled :: ShaderResourceUsageAMD)))

-- | A function to read a 'VkShaderResourceUsageAMD' and all additional
-- structures in the pointer chain into a 'ShaderResourceUsageAMD'.
fromCStructShaderResourceUsageAMD :: VkShaderResourceUsageAMD -> IO ShaderResourceUsageAMD
fromCStructShaderResourceUsageAMD c = ShaderResourceUsageAMD <$> pure (vkNumUsedVgprs (c :: VkShaderResourceUsageAMD))
                                                             <*> pure (vkNumUsedSgprs (c :: VkShaderResourceUsageAMD))
                                                             <*> pure (vkLdsSizePerLocalWorkGroup (c :: VkShaderResourceUsageAMD))
                                                             <*> pure (vkLdsUsageSizeInBytes (c :: VkShaderResourceUsageAMD))
                                                             <*> pure (vkScratchMemUsageInBytes (c :: VkShaderResourceUsageAMD))

instance Zero ShaderResourceUsageAMD where
  zero = ShaderResourceUsageAMD zero
                                zero
                                zero
                                zero
                                zero



-- | VkShaderStatisticsInfoAMD - Statistical information about a particular
-- shader within a pipeline
--
-- = Description
--
-- Some implementations may merge multiple logical shader stages together
-- in a single shader. In such cases, @shaderStageMask@ will contain a
-- bitmask of all of the stages that are active within that shader.
-- Consequently, if specifying those stages as input to
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.vkGetShaderInfoAMD',
-- the same output information /may/ be returned for all such shader stage
-- queries.
--
-- The number of available VGPRs and SGPRs (@numAvailableVgprs@ and
-- @numAvailableSgprs@ respectively) are the shader-addressable subset of
-- physical registers that is given as a limit to the compiler for register
-- assignment. These values /may/ further be limited by implementations due
-- to performance optimizations where register pressure is a bottleneck.
--
-- Unresolved directive in VkShaderStatisticsInfoAMD.txt -
-- include::{generated}\/validity\/structs\/VkShaderStatisticsInfoAMD.txt[]
--
-- = See Also
--
-- No cross-references are available
data ShaderStatisticsInfoAMD = ShaderStatisticsInfoAMD
  { -- No documentation found for Nested "ShaderStatisticsInfoAMD" "shaderStageMask"
  shaderStageMask :: ShaderStageFlags
  , -- No documentation found for Nested "ShaderStatisticsInfoAMD" "resourceUsage"
  resourceUsage :: ShaderResourceUsageAMD
  , -- No documentation found for Nested "ShaderStatisticsInfoAMD" "numPhysicalVgprs"
  numPhysicalVgprs :: Word32
  , -- No documentation found for Nested "ShaderStatisticsInfoAMD" "numPhysicalSgprs"
  numPhysicalSgprs :: Word32
  , -- No documentation found for Nested "ShaderStatisticsInfoAMD" "numAvailableVgprs"
  numAvailableVgprs :: Word32
  , -- No documentation found for Nested "ShaderStatisticsInfoAMD" "numAvailableSgprs"
  numAvailableSgprs :: Word32
  , -- No documentation found for Nested "ShaderStatisticsInfoAMD" "computeWorkGroupSize"
  computeWorkGroupSize :: (Word32, Word32, Word32)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkShaderStatisticsInfoAMD' and
-- marshal a 'ShaderStatisticsInfoAMD' into it. The 'VkShaderStatisticsInfoAMD' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructShaderStatisticsInfoAMD :: ShaderStatisticsInfoAMD -> (VkShaderStatisticsInfoAMD -> IO a) -> IO a
withCStructShaderStatisticsInfoAMD marshalled cont = withCStructShaderResourceUsageAMD (resourceUsage (marshalled :: ShaderStatisticsInfoAMD)) (\resourceUsage'' -> cont (VkShaderStatisticsInfoAMD (shaderStageMask (marshalled :: ShaderStatisticsInfoAMD)) resourceUsage'' (numPhysicalVgprs (marshalled :: ShaderStatisticsInfoAMD)) (numPhysicalSgprs (marshalled :: ShaderStatisticsInfoAMD)) (numAvailableVgprs (marshalled :: ShaderStatisticsInfoAMD)) (numAvailableSgprs (marshalled :: ShaderStatisticsInfoAMD)) (fromTuple (computeWorkGroupSize (marshalled :: ShaderStatisticsInfoAMD)))))

-- | A function to read a 'VkShaderStatisticsInfoAMD' and all additional
-- structures in the pointer chain into a 'ShaderStatisticsInfoAMD'.
fromCStructShaderStatisticsInfoAMD :: VkShaderStatisticsInfoAMD -> IO ShaderStatisticsInfoAMD
fromCStructShaderStatisticsInfoAMD c = ShaderStatisticsInfoAMD <$> pure (vkShaderStageMask (c :: VkShaderStatisticsInfoAMD))
                                                               <*> (fromCStructShaderResourceUsageAMD (vkResourceUsage (c :: VkShaderStatisticsInfoAMD)))
                                                               <*> pure (vkNumPhysicalVgprs (c :: VkShaderStatisticsInfoAMD))
                                                               <*> pure (vkNumPhysicalSgprs (c :: VkShaderStatisticsInfoAMD))
                                                               <*> pure (vkNumAvailableVgprs (c :: VkShaderStatisticsInfoAMD))
                                                               <*> pure (vkNumAvailableSgprs (c :: VkShaderStatisticsInfoAMD))
                                                               <*> pure (let v = (vkComputeWorkGroupSize (c :: VkShaderStatisticsInfoAMD)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                               , Data.Vector.Storable.Sized.unsafeIndex v 1
                                                               , Data.Vector.Storable.Sized.unsafeIndex v 2 ))

instance Zero ShaderStatisticsInfoAMD where
  zero = ShaderStatisticsInfoAMD zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 (zero, zero, zero)



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
-- @pInfo@, and
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.vkGetShaderInfoAMD'
-- will return 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'.
--
-- Not all information is available for every shader and implementations
-- may not support all kinds of information for any shader. When a certain
-- type of information is unavailable, the function returns
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FEATURE_NOT_PRESENT'.
--
-- If information is successfully and fully queried, the function will
-- return 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'.
--
-- For @infoType@
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VK_SHADER_INFO_TYPE_STATISTICS_AMD',
-- an instance of
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VkShaderStatisticsInfoAMD'
-- will be written to the buffer pointed to by @pInfo@. This structure will
-- be populated with statistics regarding the physical device resources
-- used by that shader along with other miscellaneous information and is
-- described in further detail below.
--
-- For @infoType@
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD',
-- @pInfo@ points to a UTF-8 null-terminated string containing
-- human-readable disassembly. The exact formatting and contents of the
-- disassembly string are vendor-specific.
--
-- The formatting and contents of all other types of information, including
-- @infoType@
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VK_SHADER_INFO_TYPE_BINARY_AMD',
-- are left to the vendor and are not further specified by this extension.
--
-- Unresolved directive in vkGetShaderInfoAMD.txt -
-- include::{generated}\/validity\/protos\/vkGetShaderInfoAMD.txt[]
--
-- = See Also
--
-- No cross-references are available
getNumShaderInfoAMD :: Device ->  Pipeline ->  ShaderStageFlagBits ->  ShaderInfoTypeAMD ->  IO (VkResult, CSize)
getNumShaderInfoAMD = \(Device device' commandTable) -> \pipeline' -> \shaderStage' -> \infoType' -> alloca (\pInfoSize' -> vkGetShaderInfoAMD commandTable device' pipeline' shaderStage' infoType' pInfoSize' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pInfoSize')))

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
-- @pInfo@, and
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.vkGetShaderInfoAMD'
-- will return 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'.
--
-- Not all information is available for every shader and implementations
-- may not support all kinds of information for any shader. When a certain
-- type of information is unavailable, the function returns
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FEATURE_NOT_PRESENT'.
--
-- If information is successfully and fully queried, the function will
-- return 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'.
--
-- For @infoType@
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VK_SHADER_INFO_TYPE_STATISTICS_AMD',
-- an instance of
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VkShaderStatisticsInfoAMD'
-- will be written to the buffer pointed to by @pInfo@. This structure will
-- be populated with statistics regarding the physical device resources
-- used by that shader along with other miscellaneous information and is
-- described in further detail below.
--
-- For @infoType@
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD',
-- @pInfo@ points to a UTF-8 null-terminated string containing
-- human-readable disassembly. The exact formatting and contents of the
-- disassembly string are vendor-specific.
--
-- The formatting and contents of all other types of information, including
-- @infoType@
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VK_SHADER_INFO_TYPE_BINARY_AMD',
-- are left to the vendor and are not further specified by this extension.
--
-- Unresolved directive in vkGetShaderInfoAMD.txt -
-- include::{generated}\/validity\/protos\/vkGetShaderInfoAMD.txt[]
--
-- = See Also
--
-- No cross-references are available
getShaderInfoAMD :: Device ->  Pipeline ->  ShaderStageFlagBits ->  ShaderInfoTypeAMD ->  CSize ->  IO (VkResult, ByteString)
getShaderInfoAMD = \(Device device' commandTable) -> \pipeline' -> \shaderStage' -> \infoType' -> \infoSize' -> allocaArray (fromIntegral infoSize') (\pInfo' -> with infoSize' (\pInfoSize' -> vkGetShaderInfoAMD commandTable device' pipeline' shaderStage' infoType' pInfoSize' (castPtr pInfo') >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(curry packCStringLen pInfo' =<< (fromIntegral <$> (peek pInfoSize')))))))
-- | Returns all the values available from 'getShaderInfoAMD'.
getAllShaderInfoAMD :: Device ->  Pipeline ->  ShaderStageFlagBits ->  ShaderInfoTypeAMD ->  IO (ByteString)
getAllShaderInfoAMD device' pipeline' shaderStage' infoType' =
  snd <$> getNumShaderInfoAMD device' pipeline' shaderStage' infoType'
    >>= \num -> snd <$> getShaderInfoAMD device' pipeline' shaderStage' infoType' num

