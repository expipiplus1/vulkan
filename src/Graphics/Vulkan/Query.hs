{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Query where
import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkInternalAllocationType(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkSystemAllocationScope(..)
                             , PFN_vkFreeFunction
                             , PFN_vkInternalFreeNotification
                             )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkDeviceSize(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize
                      , CSize(..)
                      )
-- ** vkGetQueryPoolResults
foreign import ccall "vkGetQueryPoolResults" vkGetQueryPoolResults :: 
  VkDevice ->
  VkQueryPool ->
    Word32 ->
      Word32 ->
        CSize ->
          Ptr Void -> VkDeviceSize -> VkQueryResultFlags -> IO VkResult

-- ** vkDestroyQueryPool
foreign import ccall "vkDestroyQueryPool" vkDestroyQueryPool :: 
  VkDevice -> VkQueryPool -> Ptr VkAllocationCallbacks -> IO ()


data VkQueryPoolCreateInfo =
  VkQueryPoolCreateInfo{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkFlags :: VkQueryPoolCreateFlags 
                       , vkQueryType :: VkQueryType 
                       , vkQueryCount :: Word32 
                       , vkPipelineStatistics :: VkQueryPipelineStatisticFlags 
                       }
  deriving (Eq)

instance Storable VkQueryPoolCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkQueryPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueryType (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkQueryCount (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkPipelineStatistics (poked :: VkQueryPoolCreateInfo))


-- ** VkQueryResultFlags

newtype VkQueryResultFlagBits = VkQueryResultFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkQueryResultFlagBits
type VkQueryResultFlags = VkQueryResultFlagBits
-- | Results of the queries are written to the destination buffer as 64-bit values
pattern VK_QUERY_RESULT_64_BIT = VkQueryResultFlagBits 0x1
-- | Results of the queries are waited on before proceeding with the result copy
pattern VK_QUERY_RESULT_WAIT_BIT = VkQueryResultFlagBits 0x2
-- | Besides the results of the query, the availability of the results is also written
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = VkQueryResultFlagBits 0x4
-- | Copy the partial results of the query even if the final results aren't available
pattern VK_QUERY_RESULT_PARTIAL_BIT = VkQueryResultFlagBits 0x8


-- ** VkQueryType

newtype VkQueryType = VkQueryType Int32
  deriving (Eq, Storable)

pattern VK_QUERY_TYPE_OCCLUSION = VkQueryType 0
-- | Optional
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS = VkQueryType 1

pattern VK_QUERY_TYPE_TIMESTAMP = VkQueryType 2

newtype VkQueryPool = VkQueryPool Word64
  deriving (Eq, Storable)

-- ** vkCreateQueryPool
foreign import ccall "vkCreateQueryPool" vkCreateQueryPool :: 
  VkDevice ->
  Ptr VkQueryPoolCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkQueryPool -> IO VkResult

-- ** VkQueryControlFlags

newtype VkQueryControlFlagBits = VkQueryControlFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkQueryControlFlagBits
type VkQueryControlFlags = VkQueryControlFlagBits
-- | Require precise results to be collected by the query
pattern VK_QUERY_CONTROL_PRECISE_BIT = VkQueryControlFlagBits 0x1


-- ** VkQueryPoolCreateFlags
-- | Opaque flag
newtype VkQueryPoolCreateFlags = VkQueryPoolCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkQueryPipelineStatisticFlags

newtype VkQueryPipelineStatisticFlagBits = VkQueryPipelineStatisticFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkQueryPipelineStatisticFlagBits
type VkQueryPipelineStatisticFlags = VkQueryPipelineStatisticFlagBits
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT = VkQueryPipelineStatisticFlagBits 0x1
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x2
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x4
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x8
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x10
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x20
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x40
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x80
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = VkQueryPipelineStatisticFlagBits 0x100
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x200
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x400


