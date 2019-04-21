{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( VkPhysicalDeviceTransformFeedbackFeaturesEXT(..)
  , VkPhysicalDeviceTransformFeedbackPropertiesEXT(..)
  , VkPipelineRasterizationStateStreamCreateFlagsEXT(..)
  , VkPipelineRasterizationStateStreamCreateInfoEXT(..)
  , FN_vkCmdBeginQueryIndexedEXT
  , PFN_vkCmdBeginQueryIndexedEXT
  , vkCmdBeginQueryIndexedEXT
  , FN_vkCmdBeginTransformFeedbackEXT
  , PFN_vkCmdBeginTransformFeedbackEXT
  , vkCmdBeginTransformFeedbackEXT
  , FN_vkCmdBindTransformFeedbackBuffersEXT
  , PFN_vkCmdBindTransformFeedbackBuffersEXT
  , vkCmdBindTransformFeedbackBuffersEXT
  , FN_vkCmdDrawIndirectByteCountEXT
  , PFN_vkCmdDrawIndirectByteCountEXT
  , vkCmdDrawIndirectByteCountEXT
  , FN_vkCmdEndQueryIndexedEXT
  , PFN_vkCmdEndQueryIndexedEXT
  , vkCmdEndQueryIndexedEXT
  , FN_vkCmdEndTransformFeedbackEXT
  , PFN_vkCmdEndTransformFeedbackEXT
  , vkCmdEndTransformFeedbackEXT
  , pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT
  , pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT
  , pattern VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT
  , pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
  , pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
  , pattern VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
  , pattern VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
  , pattern VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT
  , pattern VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
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


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferUsageFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkQueryControlFlagBits(..)
  , VkQueryControlFlags
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Query
  ( VkQueryType(..)
  , VkQueryPool
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkPhysicalDeviceTransformFeedbackFeaturesEXT - Structure describing
-- transform feedback features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceTransformFeedbackFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'VkPhysicalDeviceTransformFeedbackFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- 'VkPhysicalDeviceTransformFeedbackFeaturesEXT' /can/ also be used in the
-- @pNext@ chain of 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to
-- enable features.
--
-- Unresolved directive in VkPhysicalDeviceTransformFeedbackFeaturesEXT.txt
-- -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceTransformFeedbackFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceTransformFeedbackFeaturesEXT = VkPhysicalDeviceTransformFeedbackFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- | @transformFeedback@ indicates whether the implementation supports
  -- transform feedback and shader modules /can/ declare the
  -- @TransformFeedback@ capability.
  vkTransformFeedback :: VkBool32
  , -- | @geometryStreams@ indicates whether the implementation supports the
  -- @GeometryStreams@ SPIR-V capability.
  vkGeometryStreams :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceTransformFeedbackFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceTransformFeedbackFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                          <*> peek (ptr `plusPtr` 8)
                                                          <*> peek (ptr `plusPtr` 16)
                                                          <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceTransformFeedbackFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceTransformFeedbackFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkTransformFeedback (poked :: VkPhysicalDeviceTransformFeedbackFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkGeometryStreams (poked :: VkPhysicalDeviceTransformFeedbackFeaturesEXT))

instance Zero VkPhysicalDeviceTransformFeedbackFeaturesEXT where
  zero = VkPhysicalDeviceTransformFeedbackFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT
                                                      zero
                                                      zero
                                                      zero

-- | VkPhysicalDeviceTransformFeedbackPropertiesEXT - Structure describing
-- transform feedback properties that can be supported by an implementation
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceTransformFeedbackPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'VkPhysicalDeviceTransformFeedbackPropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits and properties.
--
-- Unresolved directive in
-- VkPhysicalDeviceTransformFeedbackPropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceTransformFeedbackPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceTransformFeedbackPropertiesEXT = VkPhysicalDeviceTransformFeedbackPropertiesEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @maxTransformFeedbackStreams@ is the maximum number of vertex streams
  -- that can be output from geometry shaders declared with the
  -- @GeometryStreams@ capability. If the implementation does not support
  -- 'VkPhysicalDeviceTransformFeedbackFeaturesEXT'::@geometryStreams@ then
  -- @maxTransformFeedbackStreams@ /must/ be set to @1@.
  vkMaxTransformFeedbackStreams :: Word32
  , -- | @maxTransformFeedbackBuffers@ is the maximum number of transform
  -- feedback buffers that can be bound for capturing shader outputs from the
  -- last vertex processing stage.
  vkMaxTransformFeedbackBuffers :: Word32
  , -- | @maxTransformFeedbackBufferSize@ is the maximum size that can be
  -- specified when binding a buffer for transform feedback in
  -- 'vkCmdBindTransformFeedbackBuffersEXT'.
  vkMaxTransformFeedbackBufferSize :: VkDeviceSize
  , -- | @maxTransformFeedbackStreamDataSize@ is the maximum amount of data in
  -- bytes for each vertex that captured to one or more transform feedback
  -- buffers associated with a specific vertex stream.
  vkMaxTransformFeedbackStreamDataSize :: Word32
  , -- | @maxTransformFeedbackBufferDataSize@ is the maximum amount of data in
  -- bytes for each vertex that can be captured to a specific transform
  -- feedback buffer.
  vkMaxTransformFeedbackBufferDataSize :: Word32
  , -- | @maxTransformFeedbackBufferDataStride@ is the maximum stride between
  -- each capture of vertex data to the buffer.
  vkMaxTransformFeedbackBufferDataStride :: Word32
  , -- | @transformFeedbackQueries@ is true if the implementation supports the
  -- 'VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT' query type.
  -- @transformFeedbackQueries@ is false if queries of this type /cannot/ be
  -- created.
  vkTransformFeedbackQueries :: VkBool32
  , -- | @transformFeedbackStreamsLinesTriangles@ is true if the implementation
  -- supports the geometry shader @OpExecutionMode@ of @OutputLineStrip@ and
  -- @OutputTriangleStrip@ in addition to @OutputPoints@ when more than one
  -- vertex stream is output. If @transformFeedbackStreamsLinesTriangles@ is
  -- false the implementation only supports an @OpExecutionMode@ of
  -- @OutputPoints@ when more than one vertex stream is output from the
  -- geometry shader.
  vkTransformFeedbackStreamsLinesTriangles :: VkBool32
  , -- | @transformFeedbackRasterizationStreamSelect@ is true if the
  -- implementation supports the @GeometryStreams@ SPIR-V capability and the
  -- application can use 'VkPipelineRasterizationStateStreamCreateInfoEXT' to
  -- modify which vertex stream output is used for rasterization. Otherwise
  -- vertex stream @0@ /must/ always be used for rasterization.
  vkTransformFeedbackRasterizationStreamSelect :: VkBool32
  , -- | @transformFeedbackDraw@ is true if the implementation supports the
  -- 'vkCmdDrawIndirectByteCountEXT' function otherwise the function /must/
  -- not be called.
  vkTransformFeedbackDraw :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceTransformFeedbackPropertiesEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceTransformFeedbackPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 20)
                                                            <*> peek (ptr `plusPtr` 24)
                                                            <*> peek (ptr `plusPtr` 32)
                                                            <*> peek (ptr `plusPtr` 36)
                                                            <*> peek (ptr `plusPtr` 40)
                                                            <*> peek (ptr `plusPtr` 44)
                                                            <*> peek (ptr `plusPtr` 48)
                                                            <*> peek (ptr `plusPtr` 52)
                                                            <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxTransformFeedbackStreams (poked :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkMaxTransformFeedbackBuffers (poked :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkMaxTransformFeedbackBufferSize (poked :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                *> poke (ptr `plusPtr` 32) (vkMaxTransformFeedbackStreamDataSize (poked :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkMaxTransformFeedbackBufferDataSize (poked :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                *> poke (ptr `plusPtr` 40) (vkMaxTransformFeedbackBufferDataStride (poked :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                *> poke (ptr `plusPtr` 44) (vkTransformFeedbackQueries (poked :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                *> poke (ptr `plusPtr` 48) (vkTransformFeedbackStreamsLinesTriangles (poked :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                *> poke (ptr `plusPtr` 52) (vkTransformFeedbackRasterizationStreamSelect (poked :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))
                *> poke (ptr `plusPtr` 56) (vkTransformFeedbackDraw (poked :: VkPhysicalDeviceTransformFeedbackPropertiesEXT))

instance Zero VkPhysicalDeviceTransformFeedbackPropertiesEXT where
  zero = VkPhysicalDeviceTransformFeedbackPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT
                                                        zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero
                                                        zero

-- ** VkPipelineRasterizationStateStreamCreateFlagsEXT

-- | VkPipelineRasterizationStateStreamCreateFlagsEXT - Reserved for future
-- use
--
-- = Description
--
-- 'VkPipelineRasterizationStateStreamCreateFlagsEXT' is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- No cross-references are available
newtype VkPipelineRasterizationStateStreamCreateFlagsEXT = VkPipelineRasterizationStateStreamCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkPipelineRasterizationStateStreamCreateFlagsEXT where
  
  showsPrec p (VkPipelineRasterizationStateStreamCreateFlagsEXT x) = showParen (p >= 11) (showString "VkPipelineRasterizationStateStreamCreateFlagsEXT " . showsPrec 11 x)

instance Read VkPipelineRasterizationStateStreamCreateFlagsEXT where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineRasterizationStateStreamCreateFlagsEXT")
                        v <- step readPrec
                        pure (VkPipelineRasterizationStateStreamCreateFlagsEXT v)
                        )
                    )



-- | VkPipelineRasterizationStateStreamCreateInfoEXT - Structure defining the
-- geometry stream used for rasterization
--
-- = Description
--
-- If this structure is not present, @rasterizationStream@ is assumed to be
-- zero.
--
-- == Valid Usage
--
-- Unresolved directive in
-- VkPipelineRasterizationStateStreamCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkPipelineRasterizationStateStreamCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPipelineRasterizationStateStreamCreateInfoEXT = VkPipelineRasterizationStateStreamCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineRasterizationStateStreamCreateFlagsEXT
  , -- | @rasterizationStream@ /must/ be zero if
  -- 'VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackRasterizationStreamSelect@
  -- is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
  vkRasterizationStream :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPipelineRasterizationStateStreamCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPipelineRasterizationStateStreamCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
                                                             <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineRasterizationStateStreamCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineRasterizationStateStreamCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineRasterizationStateStreamCreateInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkRasterizationStream (poked :: VkPipelineRasterizationStateStreamCreateInfoEXT))

instance Zero VkPipelineRasterizationStateStreamCreateInfoEXT where
  zero = VkPipelineRasterizationStateStreamCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT
                                                         zero
                                                         zero
                                                         zero

-- | vkCmdBeginQueryIndexedEXT - Begin an indexed query
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @queryPool@ is the query pool that will manage the results of the
--     query.
--
-- -   @query@ is the query index within the query pool that will contain
--     the results.
--
-- -   @flags@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlagBits'
--     specifying constraints on the types of queries that /can/ be
--     performed.
--
-- -   @index@ is the query type specific index. When the query type is
--     'VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT' the index represents
--     the vertex stream.
--
-- = Description
--
-- The 'vkCmdBeginQueryIndexedEXT' command operates the same as the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery'
-- command, except that it also accepts a query type specific @index@
-- parameter.
--
-- == Valid Usage
--
-- -   @queryPool@ /must/ have been created with a @queryType@ that differs
--     from that of any queries that are
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>
--     within @commandBuffer@
--
-- -   All queries used by the command /must/ be unavailable
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-occlusionQueryPrecise precise occlusion queries>
--     feature is not enabled, or the @queryType@ used to create
--     @queryPool@ was not
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_OCCLUSION', @flags@
--     /must/ not contain
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_QUERY_CONTROL_PRECISE_BIT'
--
-- -   @query@ /must/ be less than the number of queries in @queryPool@
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_OCCLUSION', the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_PIPELINE_STATISTICS'
--     and any of the @pipelineStatistics@ indicate graphics operations,
--     the 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_PIPELINE_STATISTICS'
--     and any of the @pipelineStatistics@ indicate compute operations, the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   If 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery'
--     is called within a render pass instance, the sum of @query@ and the
--     number of bits set in the current subpass’s view mask /must/ be less
--     than or equal to the number of queries in @queryPool@
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT' the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT' the @index@ parameter
--     /must/ be less than
--     'VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
--
-- -   If the @queryType@ used to create @queryPool@ was not
--     'VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT' the @index@ /must/ be
--     zero
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT' then
--     'VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackQueries@
--     /must/ be supported
--
-- Unresolved directive in vkCmdBeginQueryIndexedEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdBeginQueryIndexedEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginQueryIndexedEXT" vkCmdBeginQueryIndexedEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> ("index" ::: Word32) -> IO ()
#else
vkCmdBeginQueryIndexedEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> ("index" ::: Word32) -> IO ()
vkCmdBeginQueryIndexedEXT deviceCmds = mkVkCmdBeginQueryIndexedEXT (pVkCmdBeginQueryIndexedEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginQueryIndexedEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> ("index" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> ("index" ::: Word32) -> IO ())
#endif

type FN_vkCmdBeginQueryIndexedEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> ("index" ::: Word32) -> IO ()
type PFN_vkCmdBeginQueryIndexedEXT = FunPtr FN_vkCmdBeginQueryIndexedEXT

-- | vkCmdBeginTransformFeedbackEXT - Make transform feedback active in the
-- command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @firstCounterBuffer@ is the index of the first transform feedback
--     buffer corresponding to @pCounterBuffers@[0] and
--     @pCounterBufferOffsets@[0].
--
-- -   @counterBufferCount@ is the size of the @pCounterBuffers@ and
--     @pCounterBufferOffsets@ arrays.
--
-- -   @pCounterBuffers@ is an optional array of buffer handles to the
--     counter buffers which contain a 4 byte integer value representing
--     the byte offset from the start of the corresponding transform
--     feedback buffer from where to start capturing vertex data. If the
--     byte offset stored to the counter buffer location was done using
--     'vkCmdEndTransformFeedbackEXT' it can be used to resume transform
--     feedback from the previous location. If @pCounterBuffers@ is @NULL@,
--     then transform feedback will start capturing vertex data to byte
--     offset zero in all bound transform feedback buffers. For each
--     element of @pCounterBuffers@ that is
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', transform
--     feedback will start capturing vertex data to byte zero in the
--     corresponding bound transform feedback buffer.
--
-- -   @pCounterBufferOffsets@ is an optional array of offsets within each
--     of the @pCounterBuffers@ where the counter values were previously
--     written. The location in each counter buffer at these offsets /must/
--     be large enough to contain 4 bytes of data. This data is the number
--     of bytes captured by the previous transform feedback to this buffer.
--     If @pCounterBufferOffsets@ is @NULL@, then it is assumed the offsets
--     are zero.
--
-- = Description
--
-- The active transform feedback buffers will capture primitives emitted
-- from the corresponding @XfbBuffer@ in the bound graphics pipeline. Any
-- @XfbBuffer@ emitted that does not output to an active transform feedback
-- buffer will not be captured.
--
-- == Valid Usage
--
-- -   'VkPhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   Transform feedback /must/ not be active
--
-- -   @firstCounterBuffer@ /must/ be less than
--     'VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   The sum of @firstCounterBuffer@ and @counterBufferCount@ /must/ be
--     less than or equal to
--     'VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   If @counterBufferCount@ is not @0@, and @pCounterBuffers@ is not
--     @NULL@, @pCounterBuffers@ /must/ be a valid pointer to an array of
--     @counterBufferCount@
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handles that
--     are either valid or
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   For each buffer handle in the array, if it is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/
--     reference a buffer large enough to hold 4 bytes at the corresponding
--     offset from the @pCounterBufferOffsets@ array
--
-- -   If @pCounterBuffer@ is @NULL@, then @pCounterBufferOffsets@ /must/
--     also be @NULL@
--
-- -   For each buffer handle in the @pCounterBuffers@ array that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/ have
--     been created with a @usage@ value containing
--     'VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT'
--
-- -   Transform feedback /must/ not be made active in a render pass
--     instance with multiview enabled
--
-- Unresolved directive in vkCmdBeginTransformFeedbackEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdBeginTransformFeedbackEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginTransformFeedbackEXT" vkCmdBeginTransformFeedbackEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ()
#else
vkCmdBeginTransformFeedbackEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ()
vkCmdBeginTransformFeedbackEXT deviceCmds = mkVkCmdBeginTransformFeedbackEXT (pVkCmdBeginTransformFeedbackEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginTransformFeedbackEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ())
#endif

type FN_vkCmdBeginTransformFeedbackEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ()
type PFN_vkCmdBeginTransformFeedbackEXT = FunPtr FN_vkCmdBeginTransformFeedbackEXT

-- | vkCmdBindTransformFeedbackBuffersEXT - Bind transform feedback buffers
-- to a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @firstBinding@ is the index of the first transform feedback binding
--     whose state is updated by the command.
--
-- -   @bindingCount@ is the number of transform feedback bindings whose
--     state is updated by the command.
--
-- -   @pBuffers@ is a pointer to an array of buffer handles.
--
-- -   @pOffsets@ is a pointer to an array of buffer offsets.
--
-- -   @pSizes@ is an optional array of buffer sizes, which specifies the
--     maximum number of bytes to capture to the corresponding transform
--     feedback buffer. If @pSizes@ is @NULL@, or the value of the @pSizes@
--     array element is 'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE',
--     then the maximum bytes captured will be the size of the
--     corresponding buffer minus the buffer offset.
--
-- = Description
--
-- The values taken from elements i of @pBuffers@, @pOffsets@ and @pSizes@
-- replace the current state for the transform feedback binding
-- @firstBinding@ + i, for i in [0, @bindingCount@). The transform feedback
-- binding is updated to start at the offset indicated by @pOffsets@[i]
-- from the start of the buffer @pBuffers@[i].
--
-- == Valid Usage
--
-- -   'VkPhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   @firstBinding@ /must/ be less than
--     'VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   The sum of @firstBinding@ and @bindingCount@ /must/ be less than or
--     equal to
--     'VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   All elements of @pOffsets@ /must/ be less than the size of the
--     corresponding element in @pBuffers@
--
-- -   All elements of @pOffsets@ /must/ be a multiple of 4
--
-- -   All elements of @pBuffers@ /must/ have been created with the
--     'VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT' flag
--
-- -   If the optional @pSize@ array is specified, each element of @pSizes@
--     /must/ either be 'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE',
--     or be less than or equal to
--     'VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBufferSize@
--
-- -   All elements of @pSizes@ /must/ be less than or equal to the size of
--     the corresponding buffer in @pBuffers@
--
-- -   All elements of @pOffsets@ plus @pSizes@, where the @pSizes@,
--     element is not 'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE',
--     /must/ be less than or equal to the size of the corresponding
--     element in @pBuffers@
--
-- -   Each element of @pBuffers@ that is non-sparse /must/ be bound
--     completely and contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   Transform feedback /must/ not be active when the
--     'vkCmdBindTransformFeedbackBuffersEXT' command is recorded
--
-- Unresolved directive in vkCmdBindTransformFeedbackBuffersEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdBindTransformFeedbackBuffersEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBindTransformFeedbackBuffersEXT" vkCmdBindTransformFeedbackBuffersEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> ("pSizes" ::: Ptr VkDeviceSize) -> IO ()
#else
vkCmdBindTransformFeedbackBuffersEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> ("pSizes" ::: Ptr VkDeviceSize) -> IO ()
vkCmdBindTransformFeedbackBuffersEXT deviceCmds = mkVkCmdBindTransformFeedbackBuffersEXT (pVkCmdBindTransformFeedbackBuffersEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindTransformFeedbackBuffersEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> ("pSizes" ::: Ptr VkDeviceSize) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> ("pSizes" ::: Ptr VkDeviceSize) -> IO ())
#endif

type FN_vkCmdBindTransformFeedbackBuffersEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> ("pSizes" ::: Ptr VkDeviceSize) -> IO ()
type PFN_vkCmdBindTransformFeedbackBuffersEXT = FunPtr FN_vkCmdBindTransformFeedbackBuffersEXT

-- | vkCmdDrawIndirectByteCountEXT - Draw primitives where the vertex count
-- is derived from the counter byte value in the counter buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @instanceCount@ is the number of instances to draw.
--
-- -   @firstInstance@ is the instance ID of the first instance to draw.
--
-- -   @counterBuffer@ is the buffer handle from where the byte count is
--     read.
--
-- -   @counterBufferOffset@ is the offset into the buffer used to read the
--     byte count, which is used to calculate the vertex count for this
--     draw call.
--
-- -   @counterOffset@ is subtracted from the byte count read from the
--     @counterBuffer@ at the @counterBufferOffset@
--
-- -   @vertexStride@ is the stride in bytes between each element of the
--     vertex data that is used to calculate the vertex count from the
--     counter value. This value is typically the same value that was used
--     in the graphics pipeline state when the transform feedback was
--     captured as the @XfbStride@.
--
-- = Description
--
-- When the command is executed, primitives are assembled in the same way
-- as done with 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDraw'
-- except the @vertexCount@ is calculated based on the byte count read from
-- @counterBuffer@ at offset @counterBufferOffset@. The assembled
-- primitives execute the bound graphics pipeline.
--
-- The effective @vertexCount@ is calculated as follows:
--
-- > const uint32_t * counterBufferPtr = (const uint8_t *)counterBuffer.address + counterBufferOffset;
-- > vertexCount = floor(max(0, (*counterBufferPtr - counterOffset)) / vertexStride);
--
-- The effective @firstVertex@ is zero.
--
-- == Valid Usage
--
-- Unresolved directive in vkCmdDrawIndirectByteCountEXT.txt -
-- include::{chapters}\/commonvalidity\/draw_common.txt[] Unresolved
-- directive in vkCmdDrawIndirectByteCountEXT.txt -
-- include::{chapters}\/commonvalidity\/draw_vertex_binding.txt[] *
-- 'VkPhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
-- /must/ be enabled * The implementation /must/ support
-- 'VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackDraw@
-- * @vertexStride@ /must/ be greater than 0 and less than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxTransformFeedbackBufferDataStride@
-- * @counterBuffer@ /must/ have been created with the
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
-- bit set
--
-- Unresolved directive in vkCmdDrawIndirectByteCountEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdDrawIndirectByteCountEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndirectByteCountEXT" vkCmdDrawIndirectByteCountEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: VkBuffer) -> ("counterBufferOffset" ::: VkDeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> IO ()
#else
vkCmdDrawIndirectByteCountEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: VkBuffer) -> ("counterBufferOffset" ::: VkDeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> IO ()
vkCmdDrawIndirectByteCountEXT deviceCmds = mkVkCmdDrawIndirectByteCountEXT (pVkCmdDrawIndirectByteCountEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirectByteCountEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: VkBuffer) -> ("counterBufferOffset" ::: VkDeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: VkBuffer) -> ("counterBufferOffset" ::: VkDeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> IO ())
#endif

type FN_vkCmdDrawIndirectByteCountEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: VkBuffer) -> ("counterBufferOffset" ::: VkDeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndirectByteCountEXT = FunPtr FN_vkCmdDrawIndirectByteCountEXT

-- | vkCmdEndQueryIndexedEXT - Ends a query
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @queryPool@ is the query pool that is managing the results of the
--     query.
--
-- -   @query@ is the query index within the query pool where the result is
--     stored.
--
-- -   @index@ is the query type specific index.
--
-- = Description
--
-- The 'vkCmdEndQueryIndexedEXT' command operates the same as the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndQuery' command,
-- except that it also accepts a query type specific @index@ parameter.
--
-- == Valid Usage
--
-- -   All queries used by the command /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>
--
-- -   @query@ /must/ be less than the number of queries in @queryPool@
--
-- -   If 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndQuery' is
--     called within a render pass instance, the sum of @query@ and the
--     number of bits set in the current subpass’s view mask /must/ be less
--     than or equal to the number of queries in @queryPool@
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT' the @index@ parameter
--     /must/ be less than
--     'VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
--
-- -   If the @queryType@ used to create @queryPool@ was not
--     'VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT' the @index@ /must/ be
--     zero
--
-- Unresolved directive in vkCmdEndQueryIndexedEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdEndQueryIndexedEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndQueryIndexedEXT" vkCmdEndQueryIndexedEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("index" ::: Word32) -> IO ()
#else
vkCmdEndQueryIndexedEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("index" ::: Word32) -> IO ()
vkCmdEndQueryIndexedEXT deviceCmds = mkVkCmdEndQueryIndexedEXT (pVkCmdEndQueryIndexedEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndQueryIndexedEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("index" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("index" ::: Word32) -> IO ())
#endif

type FN_vkCmdEndQueryIndexedEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("index" ::: Word32) -> IO ()
type PFN_vkCmdEndQueryIndexedEXT = FunPtr FN_vkCmdEndQueryIndexedEXT

-- | vkCmdEndTransformFeedbackEXT - Make transform feedback inactive in the
-- command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @firstCounterBuffer@ is the index of the first transform feedback
--     buffer corresponding to @pCounterBuffers@[0] and
--     @pCounterBufferOffsets@[0].
--
-- -   @counterBufferCount@ is the size of the @pCounterBuffers@ and
--     @pCounterBufferOffsets@ arrays.
--
-- -   @pCounterBuffers@ is an optional array of buffer handles to the
--     counter buffers used to record the current byte positions of each
--     transform feedback buffer where the next vertex output data would be
--     captured. This /can/ be used by a subsequent
--     'vkCmdBeginTransformFeedbackEXT' call to resume transform feedback
--     capture from this position. It can also be used by
--     'vkCmdDrawIndirectByteCountEXT' to determine the vertex count of the
--     draw call.
--
-- -   @pCounterBufferOffsets@ is an optional array of offsets within each
--     of the @pCounterBuffers@ where the counter values can be written.
--     The location in each counter buffer at these offsets /must/ be large
--     enough to contain 4 bytes of data. The data stored at this location
--     is the byte offset from the start of the transform feedback buffer
--     binding where the next vertex data would be written. If
--     @pCounterBufferOffsets@ is @NULL@, then it is assumed the offsets
--     are zero.
--
-- == Valid Usage
--
-- -   'VkPhysicalDeviceTransformFeedbackFeaturesEXT'::@transformFeedback@
--     /must/ be enabled
--
-- -   Transform feedback /must/ be active
--
-- -   @firstCounterBuffer@ /must/ be less than
--     'VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   The sum of @firstCounterBuffer@ and @counterBufferCount@ /must/ be
--     less than or equal to
--     'VkPhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackBuffers@
--
-- -   If @counterBufferCount@ is not @0@, and @pCounterBuffers@ is not
--     @NULL@, @pCounterBuffers@ /must/ be a valid pointer to an array of
--     @counterBufferCount@
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handles that
--     are either valid or
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   For each buffer handle in the array, if it is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/
--     reference a buffer large enough to hold 4 bytes at the corresponding
--     offset from the @pCounterBufferOffsets@ array
--
-- -   If @pCounterBuffer@ is @NULL@, then @pCounterBufferOffsets@ /must/
--     also be @NULL@
--
-- -   For each buffer handle in the @pCounterBuffers@ array that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/ have
--     been created with a @usage@ value containing
--     'VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT'
--
-- Unresolved directive in vkCmdEndTransformFeedbackEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdEndTransformFeedbackEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndTransformFeedbackEXT" vkCmdEndTransformFeedbackEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ()
#else
vkCmdEndTransformFeedbackEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ()
vkCmdEndTransformFeedbackEXT deviceCmds = mkVkCmdEndTransformFeedbackEXT (pVkCmdEndTransformFeedbackEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndTransformFeedbackEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ())
#endif

type FN_vkCmdEndTransformFeedbackEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ()
type PFN_vkCmdEndTransformFeedbackEXT = FunPtr FN_vkCmdEndTransformFeedbackEXT

-- | 'VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT' specifies read
-- access to a transform feedback counter buffer which is read when
-- 'vkCmdBeginTransformFeedbackEXT' executes.
pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT = VkAccessFlagBits 0x04000000

-- | 'VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT' specifies write
-- access to a transform feedback counter buffer which is written when
-- 'vkCmdEndTransformFeedbackEXT' executes.
pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT = VkAccessFlagBits 0x08000000

-- | 'VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT' specifies write access to a
-- transform feedback buffer made when transform feedback is active.
pattern VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT = VkAccessFlagBits 0x02000000

-- | 'VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT' specifies that the
-- buffer is suitable for using for binding as a transform feedback buffer
-- with 'vkCmdBindTransformFeedbackBuffersEXT'.
pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT = VkBufferUsageFlagBits 0x00000800

-- | 'VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT' specifies
-- that the buffer is suitable for using as a counter buffer with
-- 'vkCmdBeginTransformFeedbackEXT' and 'vkCmdEndTransformFeedbackEXT'.
pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT = VkBufferUsageFlagBits 0x00001000

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME"
pattern VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME = "VK_EXT_transform_feedback"

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION"
pattern VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION :: Integral a => a
pattern VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = 1

-- | 'VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT' specifies the stage of
-- the pipeline where vertex attribute output values are written to the
-- transform feedback buffers.
pattern VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT = VkPipelineStageFlagBits 0x01000000

-- | 'VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT' specifies a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-transform-feedback transform feedback query>.
pattern VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT :: VkQueryType
pattern VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT = VkQueryType 1000028004

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT = VkStructureType 1000028000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT = VkStructureType 1000028001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT = VkStructureType 1000028002
