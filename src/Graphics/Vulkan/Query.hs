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
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Core( StructureType(..)
                           , Result(..)
                           , DeviceSize(..)
                           , Flags(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

-- ** getQueryPoolResults
foreign import ccall "vkGetQueryPoolResults" getQueryPoolResults ::
  Device ->
  QueryPool ->
    Word32 ->
      Word32 ->
        CSize -> Ptr Void -> DeviceSize -> QueryResultFlags -> IO Result

-- ** destroyQueryPool
foreign import ccall "vkDestroyQueryPool" destroyQueryPool ::
  Device -> QueryPool -> Ptr AllocationCallbacks -> IO ()


data QueryPoolCreateInfo =
  QueryPoolCreateInfo{ sType :: StructureType 
                     , pNext :: Ptr Void 
                     , flags :: QueryPoolCreateFlags 
                     , queryType :: QueryType 
                     , queryCount :: Word32 
                     , pipelineStatistics :: QueryPipelineStatisticFlags 
                     }
  deriving (Eq)

instance Storable QueryPoolCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = QueryPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 20)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: QueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: QueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: QueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (queryType (poked :: QueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 24) (queryCount (poked :: QueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 28) (pipelineStatistics (poked :: QueryPoolCreateInfo))


-- ** QueryResultFlags

newtype QueryResultFlags = QueryResultFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show QueryResultFlags where
  showsPrec _ VK_QUERY_RESULT_64_BIT = showString "VK_QUERY_RESULT_64_BIT"
  showsPrec _ VK_QUERY_RESULT_WAIT_BIT = showString "VK_QUERY_RESULT_WAIT_BIT"
  showsPrec _ VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = showString "VK_QUERY_RESULT_WITH_AVAILABILITY_BIT"
  showsPrec _ VK_QUERY_RESULT_PARTIAL_BIT = showString "VK_QUERY_RESULT_PARTIAL_BIT"
  
  showsPrec p (QueryResultFlags x) = showParen (p >= 11) (showString "QueryResultFlags " . showsPrec 11 x)

instance Read QueryResultFlags where
  readPrec = parens ( choose [ ("VK_QUERY_RESULT_64_BIT", pure VK_QUERY_RESULT_64_BIT)
                             , ("VK_QUERY_RESULT_WAIT_BIT", pure VK_QUERY_RESULT_WAIT_BIT)
                             , ("VK_QUERY_RESULT_WITH_AVAILABILITY_BIT", pure VK_QUERY_RESULT_WITH_AVAILABILITY_BIT)
                             , ("VK_QUERY_RESULT_PARTIAL_BIT", pure VK_QUERY_RESULT_PARTIAL_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "QueryResultFlags")
                        v <- step readPrec
                        pure (QueryResultFlags v)
                        )
                    )

-- | Results of the queries are written to the destination buffer as 64-bit values
pattern VK_QUERY_RESULT_64_BIT = QueryResultFlags 0x1
-- | Results of the queries are waited on before proceeding with the result copy
pattern VK_QUERY_RESULT_WAIT_BIT = QueryResultFlags 0x2
-- | Besides the results of the query, the availability of the results is also written
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = QueryResultFlags 0x4
-- | Copy the partial results of the query even if the final results aren't available
pattern VK_QUERY_RESULT_PARTIAL_BIT = QueryResultFlags 0x8


-- ** QueryType

newtype QueryType = QueryType Int32
  deriving (Eq, Storable)

instance Show QueryType where
  showsPrec _ VK_QUERY_TYPE_OCCLUSION = showString "VK_QUERY_TYPE_OCCLUSION"
  showsPrec _ VK_QUERY_TYPE_PIPELINE_STATISTICS = showString "VK_QUERY_TYPE_PIPELINE_STATISTICS"
  showsPrec _ VK_QUERY_TYPE_TIMESTAMP = showString "VK_QUERY_TYPE_TIMESTAMP"
  showsPrec p (QueryType x) = showParen (p >= 11) (showString "QueryType " . showsPrec 11 x)

instance Read QueryType where
  readPrec = parens ( choose [ ("VK_QUERY_TYPE_OCCLUSION", pure VK_QUERY_TYPE_OCCLUSION)
                             , ("VK_QUERY_TYPE_PIPELINE_STATISTICS", pure VK_QUERY_TYPE_PIPELINE_STATISTICS)
                             , ("VK_QUERY_TYPE_TIMESTAMP", pure VK_QUERY_TYPE_TIMESTAMP)
                             ] +++
                      prec 10 (do
                        expectP (Ident "QueryType")
                        v <- step readPrec
                        pure (QueryType v)
                        )
                    )


pattern VK_QUERY_TYPE_OCCLUSION = QueryType 0
-- | Optional
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS = QueryType 1

pattern VK_QUERY_TYPE_TIMESTAMP = QueryType 2

newtype QueryPool = QueryPool Word64
  deriving (Eq, Storable)

-- ** createQueryPool
foreign import ccall "vkCreateQueryPool" createQueryPool ::
  Device ->
  Ptr QueryPoolCreateInfo ->
    Ptr AllocationCallbacks -> Ptr QueryPool -> IO Result

-- ** QueryControlFlags

newtype QueryControlFlags = QueryControlFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show QueryControlFlags where
  showsPrec _ VK_QUERY_CONTROL_PRECISE_BIT = showString "VK_QUERY_CONTROL_PRECISE_BIT"
  
  showsPrec p (QueryControlFlags x) = showParen (p >= 11) (showString "QueryControlFlags " . showsPrec 11 x)

instance Read QueryControlFlags where
  readPrec = parens ( choose [ ("VK_QUERY_CONTROL_PRECISE_BIT", pure VK_QUERY_CONTROL_PRECISE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "QueryControlFlags")
                        v <- step readPrec
                        pure (QueryControlFlags v)
                        )
                    )

-- | Require precise results to be collected by the query
pattern VK_QUERY_CONTROL_PRECISE_BIT = QueryControlFlags 0x1


-- ** QueryPoolCreateFlags
-- | Opaque flag
newtype QueryPoolCreateFlags = QueryPoolCreateFlags Flags
  deriving (Eq, Storable)

-- ** QueryPipelineStatisticFlags

newtype QueryPipelineStatisticFlags = QueryPipelineStatisticFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show QueryPipelineStatisticFlags where
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
  
  showsPrec p (QueryPipelineStatisticFlags x) = showParen (p >= 11) (showString "QueryPipelineStatisticFlags " . showsPrec 11 x)

instance Read QueryPipelineStatisticFlags where
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
                        expectP (Ident "QueryPipelineStatisticFlags")
                        v <- step readPrec
                        pure (QueryPipelineStatisticFlags v)
                        )
                    )

-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT = QueryPipelineStatisticFlags 0x1
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT = QueryPipelineStatisticFlags 0x2
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT = QueryPipelineStatisticFlags 0x4
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT = QueryPipelineStatisticFlags 0x8
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT = QueryPipelineStatisticFlags 0x10
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT = QueryPipelineStatisticFlags 0x20
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT = QueryPipelineStatisticFlags 0x40
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT = QueryPipelineStatisticFlags 0x80
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = QueryPipelineStatisticFlags 0x100
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = QueryPipelineStatisticFlags 0x200
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = QueryPipelineStatisticFlags 0x400


