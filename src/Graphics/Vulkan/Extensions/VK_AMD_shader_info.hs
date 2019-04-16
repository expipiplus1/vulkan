{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_shader_info
  ( ShaderInfoTypeAMD
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
import qualified Graphics.Vulkan.C.Dynamic
  ( getShaderInfoAMD
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_info
  ( VkShaderInfoTypeAMD(..)
  , VkShaderResourceUsageAMD(..)
  , VkShaderStatisticsInfoAMD(..)
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


-- No documentation found for TopLevel "ShaderInfoTypeAMD"
type ShaderInfoTypeAMD = VkShaderInfoTypeAMD
-- No documentation found for TopLevel "ShaderResourceUsageAMD"
data ShaderResourceUsageAMD = ShaderResourceUsageAMD
  { -- No documentation found for Nested "ShaderResourceUsageAMD" "numUsedVgprs"
  vkNumUsedVgprs :: Word32
  , -- No documentation found for Nested "ShaderResourceUsageAMD" "numUsedSgprs"
  vkNumUsedSgprs :: Word32
  , -- No documentation found for Nested "ShaderResourceUsageAMD" "ldsSizePerLocalWorkGroup"
  vkLdsSizePerLocalWorkGroup :: Word32
  , -- No documentation found for Nested "ShaderResourceUsageAMD" "ldsUsageSizeInBytes"
  vkLdsUsageSizeInBytes :: CSize
  , -- No documentation found for Nested "ShaderResourceUsageAMD" "scratchMemUsageInBytes"
  vkScratchMemUsageInBytes :: CSize
  }
  deriving (Show, Eq)
withCStructShaderResourceUsageAMD :: ShaderResourceUsageAMD -> (VkShaderResourceUsageAMD -> IO a) -> IO a
withCStructShaderResourceUsageAMD from cont = cont (VkShaderResourceUsageAMD (vkNumUsedVgprs (from :: ShaderResourceUsageAMD)) (vkNumUsedSgprs (from :: ShaderResourceUsageAMD)) (vkLdsSizePerLocalWorkGroup (from :: ShaderResourceUsageAMD)) (vkLdsUsageSizeInBytes (from :: ShaderResourceUsageAMD)) (vkScratchMemUsageInBytes (from :: ShaderResourceUsageAMD)))
fromCStructShaderResourceUsageAMD :: VkShaderResourceUsageAMD -> IO ShaderResourceUsageAMD
fromCStructShaderResourceUsageAMD c = ShaderResourceUsageAMD <$> pure (vkNumUsedVgprs (c :: VkShaderResourceUsageAMD))
                                                             <*> pure (vkNumUsedSgprs (c :: VkShaderResourceUsageAMD))
                                                             <*> pure (vkLdsSizePerLocalWorkGroup (c :: VkShaderResourceUsageAMD))
                                                             <*> pure (vkLdsUsageSizeInBytes (c :: VkShaderResourceUsageAMD))
                                                             <*> pure (vkScratchMemUsageInBytes (c :: VkShaderResourceUsageAMD))
-- No documentation found for TopLevel "ShaderStatisticsInfoAMD"
data ShaderStatisticsInfoAMD = ShaderStatisticsInfoAMD
  { -- No documentation found for Nested "ShaderStatisticsInfoAMD" "shaderStageMask"
  vkShaderStageMask :: ShaderStageFlags
  , -- No documentation found for Nested "ShaderStatisticsInfoAMD" "resourceUsage"
  vkResourceUsage :: ShaderResourceUsageAMD
  , -- No documentation found for Nested "ShaderStatisticsInfoAMD" "numPhysicalVgprs"
  vkNumPhysicalVgprs :: Word32
  , -- No documentation found for Nested "ShaderStatisticsInfoAMD" "numPhysicalSgprs"
  vkNumPhysicalSgprs :: Word32
  , -- No documentation found for Nested "ShaderStatisticsInfoAMD" "numAvailableVgprs"
  vkNumAvailableVgprs :: Word32
  , -- No documentation found for Nested "ShaderStatisticsInfoAMD" "numAvailableSgprs"
  vkNumAvailableSgprs :: Word32
  , -- No documentation found for Nested "ShaderStatisticsInfoAMD" "computeWorkGroupSize"
  vkComputeWorkGroupSize :: (Word32, Word32, Word32)
  }
  deriving (Show, Eq)
withCStructShaderStatisticsInfoAMD :: ShaderStatisticsInfoAMD -> (VkShaderStatisticsInfoAMD -> IO a) -> IO a
withCStructShaderStatisticsInfoAMD from cont = withCStructShaderResourceUsageAMD (vkResourceUsage (from :: ShaderStatisticsInfoAMD)) (\resourceUsage -> cont (VkShaderStatisticsInfoAMD (vkShaderStageMask (from :: ShaderStatisticsInfoAMD)) resourceUsage (vkNumPhysicalVgprs (from :: ShaderStatisticsInfoAMD)) (vkNumPhysicalSgprs (from :: ShaderStatisticsInfoAMD)) (vkNumAvailableVgprs (from :: ShaderStatisticsInfoAMD)) (vkNumAvailableSgprs (from :: ShaderStatisticsInfoAMD)) (fromTuple (vkComputeWorkGroupSize (from :: ShaderStatisticsInfoAMD)))))
fromCStructShaderStatisticsInfoAMD :: VkShaderStatisticsInfoAMD -> IO ShaderStatisticsInfoAMD
fromCStructShaderStatisticsInfoAMD c = ShaderStatisticsInfoAMD <$> pure (vkShaderStageMask (c :: VkShaderStatisticsInfoAMD))
                                                               <*> (fromCStructShaderResourceUsageAMD (vkResourceUsage (c :: VkShaderStatisticsInfoAMD)))
                                                               <*> pure (vkNumPhysicalVgprs (c :: VkShaderStatisticsInfoAMD))
                                                               <*> pure (vkNumPhysicalSgprs (c :: VkShaderStatisticsInfoAMD))
                                                               <*> pure (vkNumAvailableVgprs (c :: VkShaderStatisticsInfoAMD))
                                                               <*> pure (vkNumAvailableSgprs (c :: VkShaderStatisticsInfoAMD))
                                                               <*> pure (let x = (vkComputeWorkGroupSize (c :: VkShaderStatisticsInfoAMD)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                               , Data.Vector.Storable.Sized.unsafeIndex x 1
                                                               , Data.Vector.Storable.Sized.unsafeIndex x 2 ))

-- | Wrapper for vkGetShaderInfoAMD
getNumShaderInfoAMD :: Device ->  Pipeline ->  ShaderStageFlagBits ->  ShaderInfoTypeAMD ->  IO (VkResult, CSize)
getNumShaderInfoAMD = \(Device device commandTable) -> \pipeline -> \shaderStage -> \infoType -> alloca (\pInfoSize -> Graphics.Vulkan.C.Dynamic.getShaderInfoAMD commandTable device pipeline shaderStage infoType pInfoSize nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pInfoSize)))

-- | Wrapper for vkGetShaderInfoAMD
getShaderInfoAMD :: Device ->  Pipeline ->  ShaderStageFlagBits ->  ShaderInfoTypeAMD ->  CSize ->  IO (VkResult, ByteString)
getShaderInfoAMD = \(Device device commandTable) -> \pipeline -> \shaderStage -> \infoType -> \infoSize -> allocaArray (fromIntegral infoSize) (\pInfo -> with infoSize (\pInfoSize -> Graphics.Vulkan.C.Dynamic.getShaderInfoAMD commandTable device pipeline shaderStage infoType pInfoSize (castPtr pInfo) >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(curry packCStringLen pInfo =<< (fromIntegral <$> (peek pInfoSize)))))))
-- | Call 'getNumShaderInfoAMD' to get the number of return values, then use that
-- number to call 'getShaderInfoAMD' to get all the values.
getAllShaderInfoAMD :: Device ->  Pipeline ->  ShaderStageFlagBits ->  ShaderInfoTypeAMD ->  IO (ByteString)
getAllShaderInfoAMD device pipeline shaderStage infoType =
  snd <$> getNumShaderInfoAMD device pipeline shaderStage infoType
    >>= \num -> snd <$> getShaderInfoAMD device pipeline shaderStage infoType num

