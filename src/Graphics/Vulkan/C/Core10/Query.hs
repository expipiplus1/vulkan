{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Query
  ( VkQueryPipelineStatisticFlagBits(..)
  , pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
  , VkQueryPipelineStatisticFlags
  , VkQueryPool
  , VkQueryPoolCreateFlags(..)
  , VkQueryPoolCreateInfo(..)
  , VkQueryResultFlagBits(..)
  , pattern VK_QUERY_RESULT_64_BIT
  , pattern VK_QUERY_RESULT_WAIT_BIT
  , pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT
  , pattern VK_QUERY_RESULT_PARTIAL_BIT
  , VkQueryResultFlags
  , VkQueryType(..)
  , pattern VK_QUERY_TYPE_OCCLUSION
  , pattern VK_QUERY_TYPE_PIPELINE_STATISTICS
  , pattern VK_QUERY_TYPE_TIMESTAMP
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateQueryPool
#endif
  , FN_vkCreateQueryPool
  , PFN_vkCreateQueryPool
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroyQueryPool
#endif
  , FN_vkDestroyQueryPool
  , PFN_vkDestroyQueryPool
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkGetQueryPoolResults
#endif
  , FN_vkGetQueryPoolResults
  , PFN_vkGetQueryPoolResults
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkQueryPipelineStatisticFlagBits

-- No documentation found for TopLevel "VkQueryPipelineStatisticFlagBits"
newtype VkQueryPipelineStatisticFlagBits = VkQueryPipelineStatisticFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkQueryPipelineStatisticFlagBits where
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT"
  showsPrec p (VkQueryPipelineStatisticFlagBits x) = showParen (p >= 11) (showString "VkQueryPipelineStatisticFlagBits " . showsPrec 11 x)

instance Read VkQueryPipelineStatisticFlagBits where
  readPrec = parens ( choose [ ("VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT",                    pure VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT",                  pure VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT",                  pure VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT",                pure VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT",                 pure VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT",                       pure VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT",                        pure VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT",                pure VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT",        pure VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT", pure VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT",                 pure VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryPipelineStatisticFlagBits")
                        v <- step readPrec
                        pure (VkQueryPipelineStatisticFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT = VkQueryPipelineStatisticFlagBits 0x00000001

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x00000002

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000004

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000008

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x00000010

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000020

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x00000040

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000080

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = VkQueryPipelineStatisticFlagBits 0x00000100

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000200

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000400
-- No documentation found for TopLevel "VkQueryPipelineStatisticFlags"
type VkQueryPipelineStatisticFlags = VkQueryPipelineStatisticFlagBits
-- | Dummy data to tag the 'Ptr' with
data VkQueryPool_T
-- No documentation found for TopLevel "VkQueryPool"
type VkQueryPool = Ptr VkQueryPool_T
-- ** VkQueryPoolCreateFlags

-- No documentation found for TopLevel "VkQueryPoolCreateFlags"
newtype VkQueryPoolCreateFlags = VkQueryPoolCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkQueryPoolCreateFlags where
  
  showsPrec p (VkQueryPoolCreateFlags x) = showParen (p >= 11) (showString "VkQueryPoolCreateFlags " . showsPrec 11 x)

instance Read VkQueryPoolCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryPoolCreateFlags")
                        v <- step readPrec
                        pure (VkQueryPoolCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkQueryPoolCreateInfo"
data VkQueryPoolCreateInfo = VkQueryPoolCreateInfo
  { -- No documentation found for Nested "VkQueryPoolCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkQueryPoolCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkQueryPoolCreateInfo" "flags"
  vkFlags :: VkQueryPoolCreateFlags
  , -- No documentation found for Nested "VkQueryPoolCreateInfo" "queryType"
  vkQueryType :: VkQueryType
  , -- No documentation found for Nested "VkQueryPoolCreateInfo" "queryCount"
  vkQueryCount :: Word32
  , -- No documentation found for Nested "VkQueryPoolCreateInfo" "pipelineStatistics"
  vkPipelineStatistics :: VkQueryPipelineStatisticFlags
  }
  deriving (Eq, Show)

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
-- ** VkQueryResultFlagBits

-- No documentation found for TopLevel "VkQueryResultFlagBits"
newtype VkQueryResultFlagBits = VkQueryResultFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkQueryResultFlagBits where
  showsPrec _ VK_QUERY_RESULT_64_BIT = showString "VK_QUERY_RESULT_64_BIT"
  showsPrec _ VK_QUERY_RESULT_WAIT_BIT = showString "VK_QUERY_RESULT_WAIT_BIT"
  showsPrec _ VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = showString "VK_QUERY_RESULT_WITH_AVAILABILITY_BIT"
  showsPrec _ VK_QUERY_RESULT_PARTIAL_BIT = showString "VK_QUERY_RESULT_PARTIAL_BIT"
  showsPrec p (VkQueryResultFlagBits x) = showParen (p >= 11) (showString "VkQueryResultFlagBits " . showsPrec 11 x)

instance Read VkQueryResultFlagBits where
  readPrec = parens ( choose [ ("VK_QUERY_RESULT_64_BIT",                pure VK_QUERY_RESULT_64_BIT)
                             , ("VK_QUERY_RESULT_WAIT_BIT",              pure VK_QUERY_RESULT_WAIT_BIT)
                             , ("VK_QUERY_RESULT_WITH_AVAILABILITY_BIT", pure VK_QUERY_RESULT_WITH_AVAILABILITY_BIT)
                             , ("VK_QUERY_RESULT_PARTIAL_BIT",           pure VK_QUERY_RESULT_PARTIAL_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryResultFlagBits")
                        v <- step readPrec
                        pure (VkQueryResultFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkQueryResultFlagBits" "VK_QUERY_RESULT_64_BIT"
pattern VK_QUERY_RESULT_64_BIT :: VkQueryResultFlagBits
pattern VK_QUERY_RESULT_64_BIT = VkQueryResultFlagBits 0x00000001

-- No documentation found for Nested "VkQueryResultFlagBits" "VK_QUERY_RESULT_WAIT_BIT"
pattern VK_QUERY_RESULT_WAIT_BIT :: VkQueryResultFlagBits
pattern VK_QUERY_RESULT_WAIT_BIT = VkQueryResultFlagBits 0x00000002

-- No documentation found for Nested "VkQueryResultFlagBits" "VK_QUERY_RESULT_WITH_AVAILABILITY_BIT"
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT :: VkQueryResultFlagBits
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = VkQueryResultFlagBits 0x00000004

-- No documentation found for Nested "VkQueryResultFlagBits" "VK_QUERY_RESULT_PARTIAL_BIT"
pattern VK_QUERY_RESULT_PARTIAL_BIT :: VkQueryResultFlagBits
pattern VK_QUERY_RESULT_PARTIAL_BIT = VkQueryResultFlagBits 0x00000008
-- No documentation found for TopLevel "VkQueryResultFlags"
type VkQueryResultFlags = VkQueryResultFlagBits
-- ** VkQueryType

-- No documentation found for TopLevel "VkQueryType"
newtype VkQueryType = VkQueryType Int32
  deriving (Eq, Ord, Storable)

instance Show VkQueryType where
  showsPrec _ VK_QUERY_TYPE_OCCLUSION = showString "VK_QUERY_TYPE_OCCLUSION"
  showsPrec _ VK_QUERY_TYPE_PIPELINE_STATISTICS = showString "VK_QUERY_TYPE_PIPELINE_STATISTICS"
  showsPrec _ VK_QUERY_TYPE_TIMESTAMP = showString "VK_QUERY_TYPE_TIMESTAMP"
  showsPrec p (VkQueryType x) = showParen (p >= 11) (showString "VkQueryType " . showsPrec 11 x)

instance Read VkQueryType where
  readPrec = parens ( choose [ ("VK_QUERY_TYPE_OCCLUSION",           pure VK_QUERY_TYPE_OCCLUSION)
                             , ("VK_QUERY_TYPE_PIPELINE_STATISTICS", pure VK_QUERY_TYPE_PIPELINE_STATISTICS)
                             , ("VK_QUERY_TYPE_TIMESTAMP",           pure VK_QUERY_TYPE_TIMESTAMP)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryType")
                        v <- step readPrec
                        pure (VkQueryType v)
                        )
                    )

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_OCCLUSION"
pattern VK_QUERY_TYPE_OCCLUSION :: VkQueryType
pattern VK_QUERY_TYPE_OCCLUSION = VkQueryType 0

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_PIPELINE_STATISTICS"
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS :: VkQueryType
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS = VkQueryType 1

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_TIMESTAMP"
pattern VK_QUERY_TYPE_TIMESTAMP :: VkQueryType
pattern VK_QUERY_TYPE_TIMESTAMP = VkQueryType 2
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCreateQueryPool"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateQueryPool" vkCreateQueryPool :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult

#endif
type FN_vkCreateQueryPool = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult
type PFN_vkCreateQueryPool = FunPtr FN_vkCreateQueryPool
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkDestroyQueryPool"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyQueryPool" vkDestroyQueryPool :: ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyQueryPool = ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyQueryPool = FunPtr FN_vkDestroyQueryPool
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkGetQueryPoolResults"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetQueryPoolResults" vkGetQueryPoolResults :: ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult

#endif
type FN_vkGetQueryPoolResults = ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult
type PFN_vkGetQueryPoolResults = FunPtr FN_vkGetQueryPoolResults
