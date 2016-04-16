{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Query where

import Graphics.Vulkan.Device( Device(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( VkAllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkFlags(..)
                           , VkResult(..)
                           , VkDeviceSize(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

-- ** vkGetQueryPoolResults
foreign import ccall "vkGetQueryPoolResults" vkGetQueryPoolResults ::
  Device ->
  QueryPool ->
    Word32 ->
      Word32 ->
        CSize ->
          Ptr Void -> VkDeviceSize -> VkQueryResultFlags -> IO VkResult

-- ** vkDestroyQueryPool
foreign import ccall "vkDestroyQueryPool" vkDestroyQueryPool ::
  Device -> QueryPool -> Ptr VkAllocationCallbacks -> IO ()


data VkQueryPoolCreateInfo =
  VkQueryPoolCreateInfo{ sType :: VkStructureType 
                       , pNext :: Ptr Void 
                       , flags :: VkQueryPoolCreateFlags 
                       , queryType :: VkQueryType 
                       , queryCount :: Word32 
                       , pipelineStatistics :: VkQueryPipelineStatisticFlags 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (queryType (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 24) (queryCount (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 28) (pipelineStatistics (poked :: VkQueryPoolCreateInfo))


-- ** VkQueryResultFlags

newtype VkQueryResultFlags = VkQueryResultFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkQueryResultFlags where
  showsPrec _ VK_QUERY_RESULT_64_BIT = showString "VK_QUERY_RESULT_64_BIT"
  showsPrec _ VK_QUERY_RESULT_WAIT_BIT = showString "VK_QUERY_RESULT_WAIT_BIT"
  showsPrec _ VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = showString "VK_QUERY_RESULT_WITH_AVAILABILITY_BIT"
  showsPrec _ VK_QUERY_RESULT_PARTIAL_BIT = showString "VK_QUERY_RESULT_PARTIAL_BIT"
  
  showsPrec p (VkQueryResultFlags x) = showParen (p >= 11) (showString "VkQueryResultFlags " . showsPrec 11 x)

instance Read VkQueryResultFlags where
  readPrec = parens ( choose [ ("VK_QUERY_RESULT_64_BIT", pure VK_QUERY_RESULT_64_BIT)
                             , ("VK_QUERY_RESULT_WAIT_BIT", pure VK_QUERY_RESULT_WAIT_BIT)
                             , ("VK_QUERY_RESULT_WITH_AVAILABILITY_BIT", pure VK_QUERY_RESULT_WITH_AVAILABILITY_BIT)
                             , ("VK_QUERY_RESULT_PARTIAL_BIT", pure VK_QUERY_RESULT_PARTIAL_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryResultFlags")
                        v <- step readPrec
                        pure (VkQueryResultFlags v)
                        )
                    )

-- | Results of the queries are written to the destination buffer as 64-bit values
pattern VK_QUERY_RESULT_64_BIT = VkQueryResultFlags 0x1
-- | Results of the queries are waited on before proceeding with the result copy
pattern VK_QUERY_RESULT_WAIT_BIT = VkQueryResultFlags 0x2
-- | Besides the results of the query, the availability of the results is also written
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = VkQueryResultFlags 0x4
-- | Copy the partial results of the query even if the final results aren't available
pattern VK_QUERY_RESULT_PARTIAL_BIT = VkQueryResultFlags 0x8


-- ** VkQueryType

newtype VkQueryType = VkQueryType Int32
  deriving (Eq, Storable)

instance Show VkQueryType where
  showsPrec _ VK_QUERY_TYPE_OCCLUSION = showString "VK_QUERY_TYPE_OCCLUSION"
  showsPrec _ VK_QUERY_TYPE_PIPELINE_STATISTICS = showString "VK_QUERY_TYPE_PIPELINE_STATISTICS"
  showsPrec _ VK_QUERY_TYPE_TIMESTAMP = showString "VK_QUERY_TYPE_TIMESTAMP"
  showsPrec p (VkQueryType x) = showParen (p >= 11) (showString "VkQueryType " . showsPrec 11 x)

instance Read VkQueryType where
  readPrec = parens ( choose [ ("VK_QUERY_TYPE_OCCLUSION", pure VK_QUERY_TYPE_OCCLUSION)
                             , ("VK_QUERY_TYPE_PIPELINE_STATISTICS", pure VK_QUERY_TYPE_PIPELINE_STATISTICS)
                             , ("VK_QUERY_TYPE_TIMESTAMP", pure VK_QUERY_TYPE_TIMESTAMP)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryType")
                        v <- step readPrec
                        pure (VkQueryType v)
                        )
                    )


pattern VK_QUERY_TYPE_OCCLUSION = VkQueryType 0
-- | Optional
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS = VkQueryType 1

pattern VK_QUERY_TYPE_TIMESTAMP = VkQueryType 2

newtype QueryPool = QueryPool Word64
  deriving (Eq, Storable)

-- ** vkCreateQueryPool
foreign import ccall "vkCreateQueryPool" vkCreateQueryPool ::
  Device ->
  Ptr VkQueryPoolCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr QueryPool -> IO VkResult

-- ** VkQueryControlFlags

newtype VkQueryControlFlags = VkQueryControlFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkQueryControlFlags where
  showsPrec _ VK_QUERY_CONTROL_PRECISE_BIT = showString "VK_QUERY_CONTROL_PRECISE_BIT"
  
  showsPrec p (VkQueryControlFlags x) = showParen (p >= 11) (showString "VkQueryControlFlags " . showsPrec 11 x)

instance Read VkQueryControlFlags where
  readPrec = parens ( choose [ ("VK_QUERY_CONTROL_PRECISE_BIT", pure VK_QUERY_CONTROL_PRECISE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryControlFlags")
                        v <- step readPrec
                        pure (VkQueryControlFlags v)
                        )
                    )

-- | Require precise results to be collected by the query
pattern VK_QUERY_CONTROL_PRECISE_BIT = VkQueryControlFlags 0x1


-- ** VkQueryPoolCreateFlags
-- | Opaque flag
newtype VkQueryPoolCreateFlags = VkQueryPoolCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkQueryPipelineStatisticFlags

newtype VkQueryPipelineStatisticFlags = VkQueryPipelineStatisticFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkQueryPipelineStatisticFlags where
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
  
  showsPrec p (VkQueryPipelineStatisticFlags x) = showParen (p >= 11) (showString "VkQueryPipelineStatisticFlags " . showsPrec 11 x)

instance Read VkQueryPipelineStatisticFlags where
  readPrec = parens ( choose [ ("VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT", pure VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT", pure VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT", pure VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT", pure VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT", pure VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT", pure VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT", pure VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT", pure VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT", pure VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT", pure VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT", pure VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryPipelineStatisticFlags")
                        v <- step readPrec
                        pure (VkQueryPipelineStatisticFlags v)
                        )
                    )

-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT = VkQueryPipelineStatisticFlags 0x1
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT = VkQueryPipelineStatisticFlags 0x2
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlags 0x4
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlags 0x8
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT = VkQueryPipelineStatisticFlags 0x10
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT = VkQueryPipelineStatisticFlags 0x20
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT = VkQueryPipelineStatisticFlags 0x40
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlags 0x80
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = VkQueryPipelineStatisticFlags 0x100
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlags 0x200
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlags 0x400


