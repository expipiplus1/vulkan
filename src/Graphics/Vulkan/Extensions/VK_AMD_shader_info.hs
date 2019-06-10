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
  , ShaderResourceUsageAMD(..)
  , ShaderStatisticsInfoAMD(..)
  , getNumShaderInfoAMD
  , getShaderInfoAMD
  , getAllShaderInfoAMD
  , pattern AMD_SHADER_INFO_EXTENSION_NAME
  , pattern AMD_SHADER_INFO_SPEC_VERSION
  ) where

import Data.ByteString
  ( ByteString
  , packCStringLen
  )
import Data.String
  ( IsString
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
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_info
  ( VkShaderInfoTypeAMD(..)
  , vkGetShaderInfoAMD
  , pattern VK_AMD_SHADER_INFO_EXTENSION_NAME
  , pattern VK_AMD_SHADER_INFO_SPEC_VERSION
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


-- No documentation found for TopLevel "ShaderInfoTypeAMD"
type ShaderInfoTypeAMD = VkShaderInfoTypeAMD


{-# complete SHADER_INFO_TYPE_STATISTICS_AMD, SHADER_INFO_TYPE_BINARY_AMD, SHADER_INFO_TYPE_DISASSEMBLY_AMD :: ShaderInfoTypeAMD #-}


-- No documentation found for Nested "ShaderInfoTypeAMD" "SHADER_INFO_TYPE_STATISTICS_AMD"
pattern SHADER_INFO_TYPE_STATISTICS_AMD :: (a ~ ShaderInfoTypeAMD) => a
pattern SHADER_INFO_TYPE_STATISTICS_AMD = VK_SHADER_INFO_TYPE_STATISTICS_AMD


-- No documentation found for Nested "ShaderInfoTypeAMD" "SHADER_INFO_TYPE_BINARY_AMD"
pattern SHADER_INFO_TYPE_BINARY_AMD :: (a ~ ShaderInfoTypeAMD) => a
pattern SHADER_INFO_TYPE_BINARY_AMD = VK_SHADER_INFO_TYPE_BINARY_AMD


-- No documentation found for Nested "ShaderInfoTypeAMD" "SHADER_INFO_TYPE_DISASSEMBLY_AMD"
pattern SHADER_INFO_TYPE_DISASSEMBLY_AMD :: (a ~ ShaderInfoTypeAMD) => a
pattern SHADER_INFO_TYPE_DISASSEMBLY_AMD = VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD


-- No documentation found for TopLevel "VkShaderResourceUsageAMD"
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

instance Zero ShaderResourceUsageAMD where
  zero = ShaderResourceUsageAMD zero
                                zero
                                zero
                                zero
                                zero



-- No documentation found for TopLevel "VkShaderStatisticsInfoAMD"
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

instance Zero ShaderStatisticsInfoAMD where
  zero = ShaderStatisticsInfoAMD zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 (zero, zero, zero)



-- No documentation found for TopLevel "vkGetShaderInfoAMD"
getNumShaderInfoAMD :: Device ->  Pipeline ->  ShaderStageFlagBits ->  ShaderInfoTypeAMD ->  IO (VkResult, CSize)
getNumShaderInfoAMD = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetShaderInfoAMD"
getShaderInfoAMD :: Device ->  Pipeline ->  ShaderStageFlagBits ->  ShaderInfoTypeAMD ->  CSize ->  IO (VkResult, ByteString)
getShaderInfoAMD = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getShaderInfoAMD'.
getAllShaderInfoAMD :: Device ->  Pipeline ->  ShaderStageFlagBits ->  ShaderInfoTypeAMD ->  IO (ByteString)
getAllShaderInfoAMD device' pipeline' shaderStage' infoType' =
  snd <$> getNumShaderInfoAMD device' pipeline' shaderStage' infoType'
    >>= \num -> snd <$> getShaderInfoAMD device' pipeline' shaderStage' infoType' num


-- No documentation found for TopLevel "VK_AMD_SHADER_INFO_EXTENSION_NAME"
pattern AMD_SHADER_INFO_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_SHADER_INFO_EXTENSION_NAME = VK_AMD_SHADER_INFO_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_SHADER_INFO_SPEC_VERSION"
pattern AMD_SHADER_INFO_SPEC_VERSION :: Integral a => a
pattern AMD_SHADER_INFO_SPEC_VERSION = VK_AMD_SHADER_INFO_SPEC_VERSION
