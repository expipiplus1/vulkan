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


-- No documentation found for TopLevel "VkPhysicalDeviceTransformFeedbackFeaturesEXT"
data VkPhysicalDeviceTransformFeedbackFeaturesEXT = VkPhysicalDeviceTransformFeedbackFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackFeaturesEXT" "transformFeedback"
  vkTransformFeedback :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackFeaturesEXT" "geometryStreams"
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

-- No documentation found for TopLevel "VkPhysicalDeviceTransformFeedbackPropertiesEXT"
data VkPhysicalDeviceTransformFeedbackPropertiesEXT = VkPhysicalDeviceTransformFeedbackPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackStreams"
  vkMaxTransformFeedbackStreams :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBuffers"
  vkMaxTransformFeedbackBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBufferSize"
  vkMaxTransformFeedbackBufferSize :: VkDeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackStreamDataSize"
  vkMaxTransformFeedbackStreamDataSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBufferDataSize"
  vkMaxTransformFeedbackBufferDataSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBufferDataStride"
  vkMaxTransformFeedbackBufferDataStride :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackQueries"
  vkTransformFeedbackQueries :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackStreamsLinesTriangles"
  vkTransformFeedbackStreamsLinesTriangles :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackRasterizationStreamSelect"
  vkTransformFeedbackRasterizationStreamSelect :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackDraw"
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

-- No documentation found for TopLevel "VkPipelineRasterizationStateStreamCreateFlagsEXT"
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



-- No documentation found for TopLevel "VkPipelineRasterizationStateStreamCreateInfoEXT"
data VkPipelineRasterizationStateStreamCreateInfoEXT = VkPipelineRasterizationStateStreamCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineRasterizationStateStreamCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineRasterizationStateStreamCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineRasterizationStateStreamCreateInfoEXT" "flags"
  vkFlags :: VkPipelineRasterizationStateStreamCreateFlagsEXT
  , -- No documentation found for Nested "VkPipelineRasterizationStateStreamCreateInfoEXT" "rasterizationStream"
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

-- No documentation found for TopLevel "vkCmdBeginQueryIndexedEXT"
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

-- No documentation found for TopLevel "vkCmdBeginTransformFeedbackEXT"
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

-- No documentation found for TopLevel "vkCmdBindTransformFeedbackBuffersEXT"
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

-- No documentation found for TopLevel "vkCmdDrawIndirectByteCountEXT"
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

-- No documentation found for TopLevel "vkCmdEndQueryIndexedEXT"
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

-- No documentation found for TopLevel "vkCmdEndTransformFeedbackEXT"
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

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT"
pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT = VkAccessFlagBits 0x04000000

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT"
pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT = VkAccessFlagBits 0x08000000

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT"
pattern VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT = VkAccessFlagBits 0x02000000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT"
pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT = VkBufferUsageFlagBits 0x00000800

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT"
pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT = VkBufferUsageFlagBits 0x00001000

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME"
pattern VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME = "VK_EXT_transform_feedback"

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION"
pattern VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION :: Integral a => a
pattern VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = 1

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT"
pattern VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT = VkPipelineStageFlagBits 0x01000000

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT"
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
