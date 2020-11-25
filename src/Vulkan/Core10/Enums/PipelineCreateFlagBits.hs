{-# language CPP #-}
-- No documentation found for Chapter "PipelineCreateFlagBits"
module Vulkan.Core10.Enums.PipelineCreateFlagBits  ( PipelineCreateFlags
                                                   , PipelineCreateFlagBits( PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
                                                                           , PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
                                                                           , PIPELINE_CREATE_DERIVATIVE_BIT
                                                                           , PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT
                                                                           , PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT
                                                                           , PIPELINE_CREATE_LIBRARY_BIT_KHR
                                                                           , PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV
                                                                           , PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR
                                                                           , PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR
                                                                           , PIPELINE_CREATE_DEFER_COMPILE_BIT_NV
                                                                           , PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR
                                                                           , PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR
                                                                           , PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR
                                                                           , PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR
                                                                           , PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR
                                                                           , PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR
                                                                           , PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR
                                                                           , PIPELINE_CREATE_DISPATCH_BASE_BIT
                                                                           , PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
                                                                           , ..
                                                                           )
                                                   ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type PipelineCreateFlags = PipelineCreateFlagBits

-- No documentation found for TopLevel "VkPipelineCreateFlagBits"
newtype PipelineCreateFlagBits = PipelineCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
pattern PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT                    = PipelineCreateFlagBits 0x00000001
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
pattern PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT                       = PipelineCreateFlagBits 0x00000002
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DERIVATIVE_BIT"
pattern PIPELINE_CREATE_DERIVATIVE_BIT                              = PipelineCreateFlagBits 0x00000004
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT"
pattern PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT             = PipelineCreateFlagBits 0x00000200
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT"
pattern PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT   = PipelineCreateFlagBits 0x00000100
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_LIBRARY_BIT_KHR"
pattern PIPELINE_CREATE_LIBRARY_BIT_KHR                             = PipelineCreateFlagBits 0x00000800
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV"
pattern PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV                    = PipelineCreateFlagBits 0x00040000
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR"
pattern PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR    = PipelineCreateFlagBits 0x00000080
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR"
pattern PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR                  = PipelineCreateFlagBits 0x00000040
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV"
pattern PIPELINE_CREATE_DEFER_COMPILE_BIT_NV                        = PipelineCreateFlagBits 0x00000020
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR = PipelineCreateFlagBits 0x00080000
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR              = PipelineCreateFlagBits 0x00002000
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR          = PipelineCreateFlagBits 0x00001000
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR = PipelineCreateFlagBits 0x00020000
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR    = PipelineCreateFlagBits 0x00010000
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR = PipelineCreateFlagBits 0x00008000
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR = PipelineCreateFlagBits 0x00004000
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DISPATCH_BASE_BIT"
pattern PIPELINE_CREATE_DISPATCH_BASE_BIT                           = PipelineCreateFlagBits 0x00000010
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT"
pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT            = PipelineCreateFlagBits 0x00000008

conNamePipelineCreateFlagBits :: String
conNamePipelineCreateFlagBits = "PipelineCreateFlagBits"

enumPrefixPipelineCreateFlagBits :: String
enumPrefixPipelineCreateFlagBits = "PIPELINE_CREATE_"

showTablePipelineCreateFlagBits :: [(PipelineCreateFlagBits, String)]
showTablePipelineCreateFlagBits =
  [ (PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT                 , "DISABLE_OPTIMIZATION_BIT")
  , (PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT                    , "ALLOW_DERIVATIVES_BIT")
  , (PIPELINE_CREATE_DERIVATIVE_BIT                           , "DERIVATIVE_BIT")
  , (PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT          , "EARLY_RETURN_ON_FAILURE_BIT_EXT")
  , (PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT, "FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT")
  , (PIPELINE_CREATE_LIBRARY_BIT_KHR                          , "LIBRARY_BIT_KHR")
  , (PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV                 , "INDIRECT_BINDABLE_BIT_NV")
  , (PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR , "CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR")
  , (PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR               , "CAPTURE_STATISTICS_BIT_KHR")
  , (PIPELINE_CREATE_DEFER_COMPILE_BIT_NV                     , "DEFER_COMPILE_BIT_NV")
  , ( PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR
    , "RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR"
    )
  , (PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR    , "RAY_TRACING_SKIP_AABBS_BIT_KHR")
  , (PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR, "RAY_TRACING_SKIP_TRIANGLES_BIT_KHR")
  , ( PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR"
    )
  , (PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR       , "RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR")
  , (PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR, "RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR")
  , (PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR    , "RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR")
  , (PIPELINE_CREATE_DISPATCH_BASE_BIT                              , "DISPATCH_BASE_BIT")
  , (PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT               , "VIEW_INDEX_FROM_DEVICE_INDEX_BIT")
  ]


instance Show PipelineCreateFlagBits where
showsPrec = enumShowsPrec enumPrefixPipelineCreateFlagBits
                          showTablePipelineCreateFlagBits
                          conNamePipelineCreateFlagBits
                          (\(PipelineCreateFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineCreateFlagBits where
  readPrec = enumReadPrec enumPrefixPipelineCreateFlagBits
                          showTablePipelineCreateFlagBits
                          conNamePipelineCreateFlagBits
                          PipelineCreateFlagBits

