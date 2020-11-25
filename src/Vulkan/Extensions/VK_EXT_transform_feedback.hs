{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_transform_feedback"
module Vulkan.Extensions.VK_EXT_transform_feedback  ( cmdBindTransformFeedbackBuffersEXT
                                                    , cmdBeginTransformFeedbackEXT
                                                    , cmdUseTransformFeedbackEXT
                                                    , cmdEndTransformFeedbackEXT
                                                    , cmdBeginQueryIndexedEXT
                                                    , cmdUseQueryIndexedEXT
                                                    , cmdEndQueryIndexedEXT
                                                    , cmdDrawIndirectByteCountEXT
                                                    , PhysicalDeviceTransformFeedbackFeaturesEXT(..)
                                                    , PhysicalDeviceTransformFeedbackPropertiesEXT(..)
                                                    , PipelineRasterizationStateStreamCreateInfoEXT(..)
                                                    , PipelineRasterizationStateStreamCreateFlagsEXT(..)
                                                    , EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
                                                    , pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION
                                                    , EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
                                                    , pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
                                                    ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginQueryIndexedEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginTransformFeedbackEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindTransformFeedbackBuffersEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndirectByteCountEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndQueryIndexedEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndTransformFeedbackEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlagBits(..))
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlags)
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindTransformFeedbackBuffersEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> Ptr DeviceSize -> IO ()

-- No documentation found for TopLevel "vkCmdBindTransformFeedbackBuffersEXT"
cmdBindTransformFeedbackBuffersEXT :: forall io
                                    . (MonadIO io)
                                   => -- No documentation found for Nested "vkCmdBindTransformFeedbackBuffersEXT" "commandBuffer"
                                      CommandBuffer
                                   -> -- No documentation found for Nested "vkCmdBindTransformFeedbackBuffersEXT" "firstBinding"
                                      ("firstBinding" ::: Word32)
                                   -> -- No documentation found for Nested "vkCmdBindTransformFeedbackBuffersEXT" "pBuffers"
                                      ("buffers" ::: Vector Buffer)
                                   -> -- No documentation found for Nested "vkCmdBindTransformFeedbackBuffersEXT" "pOffsets"
                                      ("offsets" ::: Vector DeviceSize)
                                   -> -- No documentation found for Nested "vkCmdBindTransformFeedbackBuffersEXT" "pSizes"
                                      ("sizes" ::: Vector DeviceSize)
                                   -> io ()
cmdBindTransformFeedbackBuffersEXT commandBuffer firstBinding buffers offsets sizes = liftIO . evalContT $ do
  let vkCmdBindTransformFeedbackBuffersEXTPtr = pVkCmdBindTransformFeedbackBuffersEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBindTransformFeedbackBuffersEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindTransformFeedbackBuffersEXT is null" Nothing Nothing
  let vkCmdBindTransformFeedbackBuffersEXT' = mkVkCmdBindTransformFeedbackBuffersEXT vkCmdBindTransformFeedbackBuffersEXTPtr
  let pBuffersLength = Data.Vector.length $ (buffers)
  lift $ unless ((Data.Vector.length $ (offsets)) == pBuffersLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pOffsets and pBuffers must have the same length" Nothing Nothing
  let pSizesLength = Data.Vector.length $ (sizes)
  lift $ unless (fromIntegral pSizesLength == pBuffersLength || pSizesLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pSizes and pBuffers must have the same length" Nothing Nothing
  pPBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (buffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (buffers)
  pPOffsets <- ContT $ allocaBytesAligned @DeviceSize ((Data.Vector.length (offsets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (offsets)
  pSizes <- if Data.Vector.null (sizes)
    then pure nullPtr
    else do
      pPSizes <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (sizes))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPSizes `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((sizes))
      pure $ pPSizes
  lift $ vkCmdBindTransformFeedbackBuffersEXT' (commandBufferHandle (commandBuffer)) (firstBinding) ((fromIntegral pBuffersLength :: Word32)) (pPBuffers) (pPOffsets) pSizes
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginTransformFeedbackEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()

-- No documentation found for TopLevel "vkCmdBeginTransformFeedbackEXT"
cmdBeginTransformFeedbackEXT :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkCmdBeginTransformFeedbackEXT" "commandBuffer"
                                CommandBuffer
                             -> -- No documentation found for Nested "vkCmdBeginTransformFeedbackEXT" "firstCounterBuffer"
                                ("firstCounterBuffer" ::: Word32)
                             -> -- No documentation found for Nested "vkCmdBeginTransformFeedbackEXT" "pCounterBuffers"
                                ("counterBuffers" ::: Vector Buffer)
                             -> -- No documentation found for Nested "vkCmdBeginTransformFeedbackEXT" "pCounterBufferOffsets"
                                ("counterBufferOffsets" ::: Vector DeviceSize)
                             -> io ()
cmdBeginTransformFeedbackEXT commandBuffer firstCounterBuffer counterBuffers counterBufferOffsets = liftIO . evalContT $ do
  let vkCmdBeginTransformFeedbackEXTPtr = pVkCmdBeginTransformFeedbackEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBeginTransformFeedbackEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginTransformFeedbackEXT is null" Nothing Nothing
  let vkCmdBeginTransformFeedbackEXT' = mkVkCmdBeginTransformFeedbackEXT vkCmdBeginTransformFeedbackEXTPtr
  let pCounterBuffersLength = Data.Vector.length $ (counterBuffers)
  let pCounterBufferOffsetsLength = Data.Vector.length $ (counterBufferOffsets)
  lift $ unless (fromIntegral pCounterBufferOffsetsLength == pCounterBuffersLength || pCounterBufferOffsetsLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pCounterBufferOffsets and pCounterBuffers must have the same length" Nothing Nothing
  pPCounterBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (counterBuffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (counterBuffers)
  pCounterBufferOffsets <- if Data.Vector.null (counterBufferOffsets)
    then pure nullPtr
    else do
      pPCounterBufferOffsets <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (counterBufferOffsets))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBufferOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((counterBufferOffsets))
      pure $ pPCounterBufferOffsets
  lift $ vkCmdBeginTransformFeedbackEXT' (commandBufferHandle (commandBuffer)) (firstCounterBuffer) ((fromIntegral pCounterBuffersLength :: Word32)) (pPCounterBuffers) pCounterBufferOffsets
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginTransformFeedbackEXT' and 'cmdEndTransformFeedbackEXT'
--
-- Note that 'cmdEndTransformFeedbackEXT' is *not* called if an exception
-- is thrown by the inner action.
cmdUseTransformFeedbackEXT :: forall io r . MonadIO io => CommandBuffer -> Word32 -> Vector Buffer -> Vector DeviceSize -> io r -> io r
cmdUseTransformFeedbackEXT commandBuffer firstCounterBuffer pCounterBuffers pCounterBufferOffsets a =
  (cmdBeginTransformFeedbackEXT commandBuffer firstCounterBuffer pCounterBuffers pCounterBufferOffsets) *> a <* (cmdEndTransformFeedbackEXT commandBuffer firstCounterBuffer pCounterBuffers pCounterBufferOffsets)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndTransformFeedbackEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()

-- No documentation found for TopLevel "vkCmdEndTransformFeedbackEXT"
cmdEndTransformFeedbackEXT :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkCmdEndTransformFeedbackEXT" "commandBuffer"
                              CommandBuffer
                           -> -- No documentation found for Nested "vkCmdEndTransformFeedbackEXT" "firstCounterBuffer"
                              ("firstCounterBuffer" ::: Word32)
                           -> -- No documentation found for Nested "vkCmdEndTransformFeedbackEXT" "pCounterBuffers"
                              ("counterBuffers" ::: Vector Buffer)
                           -> -- No documentation found for Nested "vkCmdEndTransformFeedbackEXT" "pCounterBufferOffsets"
                              ("counterBufferOffsets" ::: Vector DeviceSize)
                           -> io ()
cmdEndTransformFeedbackEXT commandBuffer firstCounterBuffer counterBuffers counterBufferOffsets = liftIO . evalContT $ do
  let vkCmdEndTransformFeedbackEXTPtr = pVkCmdEndTransformFeedbackEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdEndTransformFeedbackEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndTransformFeedbackEXT is null" Nothing Nothing
  let vkCmdEndTransformFeedbackEXT' = mkVkCmdEndTransformFeedbackEXT vkCmdEndTransformFeedbackEXTPtr
  let pCounterBuffersLength = Data.Vector.length $ (counterBuffers)
  let pCounterBufferOffsetsLength = Data.Vector.length $ (counterBufferOffsets)
  lift $ unless (fromIntegral pCounterBufferOffsetsLength == pCounterBuffersLength || pCounterBufferOffsetsLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pCounterBufferOffsets and pCounterBuffers must have the same length" Nothing Nothing
  pPCounterBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (counterBuffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (counterBuffers)
  pCounterBufferOffsets <- if Data.Vector.null (counterBufferOffsets)
    then pure nullPtr
    else do
      pPCounterBufferOffsets <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (counterBufferOffsets))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterBufferOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((counterBufferOffsets))
      pure $ pPCounterBufferOffsets
  lift $ vkCmdEndTransformFeedbackEXT' (commandBufferHandle (commandBuffer)) (firstCounterBuffer) ((fromIntegral pCounterBuffersLength :: Word32)) (pPCounterBuffers) pCounterBufferOffsets
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginQueryIndexedEXT
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> QueryControlFlags -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> QueryControlFlags -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdBeginQueryIndexedEXT"
cmdBeginQueryIndexedEXT :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkCmdBeginQueryIndexedEXT" "commandBuffer"
                           CommandBuffer
                        -> -- No documentation found for Nested "vkCmdBeginQueryIndexedEXT" "queryPool"
                           QueryPool
                        -> -- No documentation found for Nested "vkCmdBeginQueryIndexedEXT" "query"
                           ("query" ::: Word32)
                        -> -- No documentation found for Nested "vkCmdBeginQueryIndexedEXT" "flags"
                           QueryControlFlags
                        -> -- No documentation found for Nested "vkCmdBeginQueryIndexedEXT" "index"
                           ("index" ::: Word32)
                        -> io ()
cmdBeginQueryIndexedEXT commandBuffer queryPool query flags index = liftIO $ do
  let vkCmdBeginQueryIndexedEXTPtr = pVkCmdBeginQueryIndexedEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBeginQueryIndexedEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginQueryIndexedEXT is null" Nothing Nothing
  let vkCmdBeginQueryIndexedEXT' = mkVkCmdBeginQueryIndexedEXT vkCmdBeginQueryIndexedEXTPtr
  vkCmdBeginQueryIndexedEXT' (commandBufferHandle (commandBuffer)) (queryPool) (query) (flags) (index)
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginQueryIndexedEXT' and 'cmdEndQueryIndexedEXT'
--
-- Note that 'cmdEndQueryIndexedEXT' is *not* called if an exception is
-- thrown by the inner action.
cmdUseQueryIndexedEXT :: forall io r . MonadIO io => CommandBuffer -> QueryPool -> Word32 -> QueryControlFlags -> Word32 -> io r -> io r
cmdUseQueryIndexedEXT commandBuffer queryPool query flags index a =
  (cmdBeginQueryIndexedEXT commandBuffer queryPool query flags index) *> a <* (cmdEndQueryIndexedEXT commandBuffer queryPool query index)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndQueryIndexedEXT
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdEndQueryIndexedEXT"
cmdEndQueryIndexedEXT :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vkCmdEndQueryIndexedEXT" "commandBuffer"
                         CommandBuffer
                      -> -- No documentation found for Nested "vkCmdEndQueryIndexedEXT" "queryPool"
                         QueryPool
                      -> -- No documentation found for Nested "vkCmdEndQueryIndexedEXT" "query"
                         ("query" ::: Word32)
                      -> -- No documentation found for Nested "vkCmdEndQueryIndexedEXT" "index"
                         ("index" ::: Word32)
                      -> io ()
cmdEndQueryIndexedEXT commandBuffer queryPool query index = liftIO $ do
  let vkCmdEndQueryIndexedEXTPtr = pVkCmdEndQueryIndexedEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdEndQueryIndexedEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndQueryIndexedEXT is null" Nothing Nothing
  let vkCmdEndQueryIndexedEXT' = mkVkCmdEndQueryIndexedEXT vkCmdEndQueryIndexedEXTPtr
  vkCmdEndQueryIndexedEXT' (commandBufferHandle (commandBuffer)) (queryPool) (query) (index)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirectByteCountEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdDrawIndirectByteCountEXT"
cmdDrawIndirectByteCountEXT :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "vkCmdDrawIndirectByteCountEXT" "commandBuffer"
                               CommandBuffer
                            -> -- No documentation found for Nested "vkCmdDrawIndirectByteCountEXT" "instanceCount"
                               ("instanceCount" ::: Word32)
                            -> -- No documentation found for Nested "vkCmdDrawIndirectByteCountEXT" "firstInstance"
                               ("firstInstance" ::: Word32)
                            -> -- No documentation found for Nested "vkCmdDrawIndirectByteCountEXT" "counterBuffer"
                               ("counterBuffer" ::: Buffer)
                            -> -- No documentation found for Nested "vkCmdDrawIndirectByteCountEXT" "counterBufferOffset"
                               ("counterBufferOffset" ::: DeviceSize)
                            -> -- No documentation found for Nested "vkCmdDrawIndirectByteCountEXT" "counterOffset"
                               ("counterOffset" ::: Word32)
                            -> -- No documentation found for Nested "vkCmdDrawIndirectByteCountEXT" "vertexStride"
                               ("vertexStride" ::: Word32)
                            -> io ()
cmdDrawIndirectByteCountEXT commandBuffer instanceCount firstInstance counterBuffer counterBufferOffset counterOffset vertexStride = liftIO $ do
  let vkCmdDrawIndirectByteCountEXTPtr = pVkCmdDrawIndirectByteCountEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawIndirectByteCountEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawIndirectByteCountEXT is null" Nothing Nothing
  let vkCmdDrawIndirectByteCountEXT' = mkVkCmdDrawIndirectByteCountEXT vkCmdDrawIndirectByteCountEXTPtr
  vkCmdDrawIndirectByteCountEXT' (commandBufferHandle (commandBuffer)) (instanceCount) (firstInstance) (counterBuffer) (counterBufferOffset) (counterOffset) (vertexStride)
  pure $ ()



-- No documentation found for TopLevel "VkPhysicalDeviceTransformFeedbackFeaturesEXT"
data PhysicalDeviceTransformFeedbackFeaturesEXT = PhysicalDeviceTransformFeedbackFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackFeaturesEXT" "transformFeedback"
    transformFeedback :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackFeaturesEXT" "geometryStreams"
    geometryStreams :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTransformFeedbackFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceTransformFeedbackFeaturesEXT

instance ToCStruct PhysicalDeviceTransformFeedbackFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTransformFeedbackFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (transformFeedback))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (geometryStreams))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTransformFeedbackFeaturesEXT where
  peekCStruct p = do
    transformFeedback <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    geometryStreams <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceTransformFeedbackFeaturesEXT
             (bool32ToBool transformFeedback) (bool32ToBool geometryStreams)


instance Storable PhysicalDeviceTransformFeedbackFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTransformFeedbackFeaturesEXT where
  zero = PhysicalDeviceTransformFeedbackFeaturesEXT
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceTransformFeedbackPropertiesEXT"
data PhysicalDeviceTransformFeedbackPropertiesEXT = PhysicalDeviceTransformFeedbackPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackStreams"
    maxTransformFeedbackStreams :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBuffers"
    maxTransformFeedbackBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBufferSize"
    maxTransformFeedbackBufferSize :: DeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackStreamDataSize"
    maxTransformFeedbackStreamDataSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBufferDataSize"
    maxTransformFeedbackBufferDataSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "maxTransformFeedbackBufferDataStride"
    maxTransformFeedbackBufferDataStride :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackQueries"
    transformFeedbackQueries :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackStreamsLinesTriangles"
    transformFeedbackStreamsLinesTriangles :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackRasterizationStreamSelect"
    transformFeedbackRasterizationStreamSelect :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceTransformFeedbackPropertiesEXT" "transformFeedbackDraw"
    transformFeedbackDraw :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTransformFeedbackPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceTransformFeedbackPropertiesEXT

instance ToCStruct PhysicalDeviceTransformFeedbackPropertiesEXT where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTransformFeedbackPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxTransformFeedbackStreams)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxTransformFeedbackBuffers)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (maxTransformFeedbackBufferSize)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxTransformFeedbackStreamDataSize)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxTransformFeedbackBufferDataSize)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxTransformFeedbackBufferDataStride)
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (transformFeedbackQueries))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (transformFeedbackStreamsLinesTriangles))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (transformFeedbackRasterizationStreamSelect))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (transformFeedbackDraw))
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTransformFeedbackPropertiesEXT where
  peekCStruct p = do
    maxTransformFeedbackStreams <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxTransformFeedbackBuffers <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxTransformFeedbackBufferSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    maxTransformFeedbackStreamDataSize <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxTransformFeedbackBufferDataSize <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    maxTransformFeedbackBufferDataStride <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    transformFeedbackQueries <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    transformFeedbackStreamsLinesTriangles <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    transformFeedbackRasterizationStreamSelect <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    transformFeedbackDraw <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    pure $ PhysicalDeviceTransformFeedbackPropertiesEXT
             maxTransformFeedbackStreams maxTransformFeedbackBuffers maxTransformFeedbackBufferSize maxTransformFeedbackStreamDataSize maxTransformFeedbackBufferDataSize maxTransformFeedbackBufferDataStride (bool32ToBool transformFeedbackQueries) (bool32ToBool transformFeedbackStreamsLinesTriangles) (bool32ToBool transformFeedbackRasterizationStreamSelect) (bool32ToBool transformFeedbackDraw)


instance Storable PhysicalDeviceTransformFeedbackPropertiesEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTransformFeedbackPropertiesEXT where
  zero = PhysicalDeviceTransformFeedbackPropertiesEXT
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



-- No documentation found for TopLevel "VkPipelineRasterizationStateStreamCreateInfoEXT"
data PipelineRasterizationStateStreamCreateInfoEXT = PipelineRasterizationStateStreamCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineRasterizationStateStreamCreateInfoEXT" "flags"
    flags :: PipelineRasterizationStateStreamCreateFlagsEXT
  , -- No documentation found for Nested "VkPipelineRasterizationStateStreamCreateInfoEXT" "rasterizationStream"
    rasterizationStream :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRasterizationStateStreamCreateInfoEXT)
#endif
deriving instance Show PipelineRasterizationStateStreamCreateInfoEXT

instance ToCStruct PipelineRasterizationStateStreamCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationStateStreamCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRasterizationStateStreamCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (rasterizationStream)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineRasterizationStateStreamCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @PipelineRasterizationStateStreamCreateFlagsEXT ((p `plusPtr` 16 :: Ptr PipelineRasterizationStateStreamCreateFlagsEXT))
    rasterizationStream <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PipelineRasterizationStateStreamCreateInfoEXT
             flags rasterizationStream


instance Storable PipelineRasterizationStateStreamCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationStateStreamCreateInfoEXT where
  zero = PipelineRasterizationStateStreamCreateInfoEXT
           zero
           zero


-- No documentation found for TopLevel "VkPipelineRasterizationStateStreamCreateFlagsEXT"
newtype PipelineRasterizationStateStreamCreateFlagsEXT = PipelineRasterizationStateStreamCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineRasterizationStateStreamCreateFlagsEXT :: String
conNamePipelineRasterizationStateStreamCreateFlagsEXT = "PipelineRasterizationStateStreamCreateFlagsEXT"

enumPrefixPipelineRasterizationStateStreamCreateFlagsEXT :: String
enumPrefixPipelineRasterizationStateStreamCreateFlagsEXT = ""

showTablePipelineRasterizationStateStreamCreateFlagsEXT :: [(PipelineRasterizationStateStreamCreateFlagsEXT, String)]
showTablePipelineRasterizationStateStreamCreateFlagsEXT = []


instance Show PipelineRasterizationStateStreamCreateFlagsEXT where
showsPrec = enumShowsPrec enumPrefixPipelineRasterizationStateStreamCreateFlagsEXT
                          showTablePipelineRasterizationStateStreamCreateFlagsEXT
                          conNamePipelineRasterizationStateStreamCreateFlagsEXT
                          (\(PipelineRasterizationStateStreamCreateFlagsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineRasterizationStateStreamCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixPipelineRasterizationStateStreamCreateFlagsEXT
                          showTablePipelineRasterizationStateStreamCreateFlagsEXT
                          conNamePipelineRasterizationStateStreamCreateFlagsEXT
                          PipelineRasterizationStateStreamCreateFlagsEXT


type EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION"
pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = 1


type EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME = "VK_EXT_transform_feedback"

-- No documentation found for TopLevel "VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME"
pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME = "VK_EXT_transform_feedback"

