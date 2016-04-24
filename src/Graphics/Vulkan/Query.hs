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
  showsPrec _ QueryResult64Bit = showString "QueryResult64Bit"
  showsPrec _ QueryResultWaitBit = showString "QueryResultWaitBit"
  showsPrec _ QueryResultWithAvailabilityBit = showString "QueryResultWithAvailabilityBit"
  showsPrec _ QueryResultPartialBit = showString "QueryResultPartialBit"
  
  showsPrec p (QueryResultFlags x) = showParen (p >= 11) (showString "QueryResultFlags " . showsPrec 11 x)

instance Read QueryResultFlags where
  readPrec = parens ( choose [ ("QueryResult64Bit", pure QueryResult64Bit)
                             , ("QueryResultWaitBit", pure QueryResultWaitBit)
                             , ("QueryResultWithAvailabilityBit", pure QueryResultWithAvailabilityBit)
                             , ("QueryResultPartialBit", pure QueryResultPartialBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "QueryResultFlags")
                        v <- step readPrec
                        pure (QueryResultFlags v)
                        )
                    )

-- | Results of the queries are written to the destination buffer as 64-bit values
pattern QueryResult64Bit = QueryResultFlags 0x1
-- | Results of the queries are waited on before proceeding with the result copy
pattern QueryResultWaitBit = QueryResultFlags 0x2
-- | Besides the results of the query, the availability of the results is also written
pattern QueryResultWithAvailabilityBit = QueryResultFlags 0x4
-- | Copy the partial results of the query even if the final results aren't available
pattern QueryResultPartialBit = QueryResultFlags 0x8


-- ** QueryType

newtype QueryType = QueryType Int32
  deriving (Eq, Storable)

instance Show QueryType where
  showsPrec _ QueryTypeOcclusion = showString "QueryTypeOcclusion"
  showsPrec _ QueryTypePipelineStatistics = showString "QueryTypePipelineStatistics"
  showsPrec _ QueryTypeTimestamp = showString "QueryTypeTimestamp"
  showsPrec p (QueryType x) = showParen (p >= 11) (showString "QueryType " . showsPrec 11 x)

instance Read QueryType where
  readPrec = parens ( choose [ ("QueryTypeOcclusion", pure QueryTypeOcclusion)
                             , ("QueryTypePipelineStatistics", pure QueryTypePipelineStatistics)
                             , ("QueryTypeTimestamp", pure QueryTypeTimestamp)
                             ] +++
                      prec 10 (do
                        expectP (Ident "QueryType")
                        v <- step readPrec
                        pure (QueryType v)
                        )
                    )


pattern QueryTypeOcclusion = QueryType 0
-- | Optional
pattern QueryTypePipelineStatistics = QueryType 1

pattern QueryTypeTimestamp = QueryType 2

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
  showsPrec _ QueryControlPreciseBit = showString "QueryControlPreciseBit"
  
  showsPrec p (QueryControlFlags x) = showParen (p >= 11) (showString "QueryControlFlags " . showsPrec 11 x)

instance Read QueryControlFlags where
  readPrec = parens ( choose [ ("QueryControlPreciseBit", pure QueryControlPreciseBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "QueryControlFlags")
                        v <- step readPrec
                        pure (QueryControlFlags v)
                        )
                    )

-- | Require precise results to be collected by the query
pattern QueryControlPreciseBit = QueryControlFlags 0x1


-- ** QueryPoolCreateFlags
-- | Opaque flag
newtype QueryPoolCreateFlags = QueryPoolCreateFlags Flags
  deriving (Eq, Storable)

-- ** QueryPipelineStatisticFlags

newtype QueryPipelineStatisticFlags = QueryPipelineStatisticFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show QueryPipelineStatisticFlags where
  showsPrec _ QueryPipelineStatisticInputAssemblyVerticesBit = showString "QueryPipelineStatisticInputAssemblyVerticesBit"
  showsPrec _ QueryPipelineStatisticInputAssemblyPrimitivesBit = showString "QueryPipelineStatisticInputAssemblyPrimitivesBit"
  showsPrec _ QueryPipelineStatisticVertexShaderInvocationsBit = showString "QueryPipelineStatisticVertexShaderInvocationsBit"
  showsPrec _ QueryPipelineStatisticGeometryShaderInvocationsBit = showString "QueryPipelineStatisticGeometryShaderInvocationsBit"
  showsPrec _ QueryPipelineStatisticGeometryShaderPrimitivesBit = showString "QueryPipelineStatisticGeometryShaderPrimitivesBit"
  showsPrec _ QueryPipelineStatisticClippingInvocationsBit = showString "QueryPipelineStatisticClippingInvocationsBit"
  showsPrec _ QueryPipelineStatisticClippingPrimitivesBit = showString "QueryPipelineStatisticClippingPrimitivesBit"
  showsPrec _ QueryPipelineStatisticFragmentShaderInvocationsBit = showString "QueryPipelineStatisticFragmentShaderInvocationsBit"
  showsPrec _ QueryPipelineStatisticTessellationControlShaderPatchesBit = showString "QueryPipelineStatisticTessellationControlShaderPatchesBit"
  showsPrec _ QueryPipelineStatisticTessellationEvaluationShaderInvocationsBit = showString "QueryPipelineStatisticTessellationEvaluationShaderInvocationsBit"
  showsPrec _ QueryPipelineStatisticComputeShaderInvocationsBit = showString "QueryPipelineStatisticComputeShaderInvocationsBit"
  
  showsPrec p (QueryPipelineStatisticFlags x) = showParen (p >= 11) (showString "QueryPipelineStatisticFlags " . showsPrec 11 x)

instance Read QueryPipelineStatisticFlags where
  readPrec = parens ( choose [ ("QueryPipelineStatisticInputAssemblyVerticesBit", pure QueryPipelineStatisticInputAssemblyVerticesBit)
                             , ("QueryPipelineStatisticInputAssemblyPrimitivesBit", pure QueryPipelineStatisticInputAssemblyPrimitivesBit)
                             , ("QueryPipelineStatisticVertexShaderInvocationsBit", pure QueryPipelineStatisticVertexShaderInvocationsBit)
                             , ("QueryPipelineStatisticGeometryShaderInvocationsBit", pure QueryPipelineStatisticGeometryShaderInvocationsBit)
                             , ("QueryPipelineStatisticGeometryShaderPrimitivesBit", pure QueryPipelineStatisticGeometryShaderPrimitivesBit)
                             , ("QueryPipelineStatisticClippingInvocationsBit", pure QueryPipelineStatisticClippingInvocationsBit)
                             , ("QueryPipelineStatisticClippingPrimitivesBit", pure QueryPipelineStatisticClippingPrimitivesBit)
                             , ("QueryPipelineStatisticFragmentShaderInvocationsBit", pure QueryPipelineStatisticFragmentShaderInvocationsBit)
                             , ("QueryPipelineStatisticTessellationControlShaderPatchesBit", pure QueryPipelineStatisticTessellationControlShaderPatchesBit)
                             , ("QueryPipelineStatisticTessellationEvaluationShaderInvocationsBit", pure QueryPipelineStatisticTessellationEvaluationShaderInvocationsBit)
                             , ("QueryPipelineStatisticComputeShaderInvocationsBit", pure QueryPipelineStatisticComputeShaderInvocationsBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "QueryPipelineStatisticFlags")
                        v <- step readPrec
                        pure (QueryPipelineStatisticFlags v)
                        )
                    )

-- | Optional
pattern QueryPipelineStatisticInputAssemblyVerticesBit = QueryPipelineStatisticFlags 0x1
-- | Optional
pattern QueryPipelineStatisticInputAssemblyPrimitivesBit = QueryPipelineStatisticFlags 0x2
-- | Optional
pattern QueryPipelineStatisticVertexShaderInvocationsBit = QueryPipelineStatisticFlags 0x4
-- | Optional
pattern QueryPipelineStatisticGeometryShaderInvocationsBit = QueryPipelineStatisticFlags 0x8
-- | Optional
pattern QueryPipelineStatisticGeometryShaderPrimitivesBit = QueryPipelineStatisticFlags 0x10
-- | Optional
pattern QueryPipelineStatisticClippingInvocationsBit = QueryPipelineStatisticFlags 0x20
-- | Optional
pattern QueryPipelineStatisticClippingPrimitivesBit = QueryPipelineStatisticFlags 0x40
-- | Optional
pattern QueryPipelineStatisticFragmentShaderInvocationsBit = QueryPipelineStatisticFlags 0x80
-- | Optional
pattern QueryPipelineStatisticTessellationControlShaderPatchesBit = QueryPipelineStatisticFlags 0x100
-- | Optional
pattern QueryPipelineStatisticTessellationEvaluationShaderInvocationsBit = QueryPipelineStatisticFlags 0x200
-- | Optional
pattern QueryPipelineStatisticComputeShaderInvocationsBit = QueryPipelineStatisticFlags 0x400


