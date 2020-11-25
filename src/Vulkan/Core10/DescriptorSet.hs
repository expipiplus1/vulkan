{-# language CPP #-}
-- No documentation found for Chapter "DescriptorSet"
module Vulkan.Core10.DescriptorSet  ( createDescriptorSetLayout
                                    , withDescriptorSetLayout
                                    , destroyDescriptorSetLayout
                                    , createDescriptorPool
                                    , withDescriptorPool
                                    , destroyDescriptorPool
                                    , resetDescriptorPool
                                    , allocateDescriptorSets
                                    , withDescriptorSets
                                    , freeDescriptorSets
                                    , updateDescriptorSets
                                    , DescriptorBufferInfo(..)
                                    , DescriptorImageInfo(..)
                                    , WriteDescriptorSet(..)
                                    , CopyDescriptorSet(..)
                                    , DescriptorSetLayoutBinding(..)
                                    , DescriptorSetLayoutCreateInfo(..)
                                    , DescriptorPoolSize(..)
                                    , DescriptorPoolCreateInfo(..)
                                    , DescriptorSetAllocateInfo(..)
                                    , DescriptorSet(..)
                                    , DescriptorSetLayout(..)
                                    , DescriptorPool(..)
                                    , DescriptorPoolResetFlags(..)
                                    , DescriptorType(..)
                                    , DescriptorPoolCreateFlagBits(..)
                                    , DescriptorPoolCreateFlags
                                    , DescriptorSetLayoutCreateFlagBits(..)
                                    , DescriptorSetLayoutCreateFlags
                                    ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (BufferView)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (DescriptorPool)
import Vulkan.Core10.Handles (DescriptorPool(..))
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_inline_uniform_block (DescriptorPoolInlineUniformBlockCreateInfoEXT)
import Vulkan.Core10.Enums.DescriptorPoolResetFlags (DescriptorPoolResetFlags)
import Vulkan.Core10.Enums.DescriptorPoolResetFlags (DescriptorPoolResetFlags(..))
import Vulkan.Core10.Handles (DescriptorSet)
import Vulkan.Core10.Handles (DescriptorSet(..))
import Vulkan.Core10.Handles (DescriptorSetLayout)
import Vulkan.Core10.Handles (DescriptorSetLayout(..))
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetLayoutBindingFlagsCreateInfo)
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlags)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetVariableDescriptorCountAllocateInfo)
import Vulkan.Core10.Enums.DescriptorType (DescriptorType)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkAllocateDescriptorSets))
import Vulkan.Dynamic (DeviceCmds(pVkCreateDescriptorPool))
import Vulkan.Dynamic (DeviceCmds(pVkCreateDescriptorSetLayout))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyDescriptorPool))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyDescriptorSetLayout))
import Vulkan.Dynamic (DeviceCmds(pVkFreeDescriptorSets))
import Vulkan.Dynamic (DeviceCmds(pVkResetDescriptorPool))
import Vulkan.Dynamic (DeviceCmds(pVkUpdateDescriptorSets))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Handles (ImageView)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Sampler)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (WriteDescriptorSetAccelerationStructureKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (WriteDescriptorSetAccelerationStructureNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_inline_uniform_block (WriteDescriptorSetInlineUniformBlockEXT)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_DESCRIPTOR_SET))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (DescriptorPool(..))
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlagBits(..))
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlags)
import Vulkan.Core10.Enums.DescriptorPoolResetFlags (DescriptorPoolResetFlags(..))
import Vulkan.Core10.Handles (DescriptorSet(..))
import Vulkan.Core10.Handles (DescriptorSetLayout(..))
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlagBits(..))
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlags)
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorSetLayout
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct DescriptorSetLayoutCreateInfo) -> Ptr AllocationCallbacks -> Ptr DescriptorSetLayout -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct DescriptorSetLayoutCreateInfo) -> Ptr AllocationCallbacks -> Ptr DescriptorSetLayout -> IO Result

-- No documentation found for TopLevel "vkCreateDescriptorSetLayout"
createDescriptorSetLayout :: forall a io
                           . (Extendss DescriptorSetLayoutCreateInfo a, PokeChain a, MonadIO io)
                          => -- No documentation found for Nested "vkCreateDescriptorSetLayout" "device"
                             Device
                          -> -- No documentation found for Nested "vkCreateDescriptorSetLayout" "pCreateInfo"
                             (DescriptorSetLayoutCreateInfo a)
                          -> -- No documentation found for Nested "vkCreateDescriptorSetLayout" "pAllocator"
                             ("allocator" ::: Maybe AllocationCallbacks)
                          -> io (DescriptorSetLayout)
createDescriptorSetLayout device createInfo allocator = liftIO . evalContT $ do
  let vkCreateDescriptorSetLayoutPtr = pVkCreateDescriptorSetLayout (deviceCmds (device :: Device))
  lift $ unless (vkCreateDescriptorSetLayoutPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDescriptorSetLayout is null" Nothing Nothing
  let vkCreateDescriptorSetLayout' = mkVkCreateDescriptorSetLayout vkCreateDescriptorSetLayoutPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSetLayout <- ContT $ bracket (callocBytes @DescriptorSetLayout 8) free
  r <- lift $ vkCreateDescriptorSetLayout' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPSetLayout)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSetLayout <- lift $ peek @DescriptorSetLayout pPSetLayout
  pure $ (pSetLayout)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDescriptorSetLayout' and 'destroyDescriptorSetLayout'
--
-- To ensure that 'destroyDescriptorSetLayout' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDescriptorSetLayout :: forall a io r . (Extendss DescriptorSetLayoutCreateInfo a, PokeChain a, MonadIO io) => Device -> DescriptorSetLayoutCreateInfo a -> Maybe AllocationCallbacks -> (io DescriptorSetLayout -> (DescriptorSetLayout -> io ()) -> r) -> r
withDescriptorSetLayout device pCreateInfo pAllocator b =
  b (createDescriptorSetLayout device pCreateInfo pAllocator)
    (\(o0) -> destroyDescriptorSetLayout device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorSetLayout
  :: FunPtr (Ptr Device_T -> DescriptorSetLayout -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> DescriptorSetLayout -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyDescriptorSetLayout"
destroyDescriptorSetLayout :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkDestroyDescriptorSetLayout" "device"
                              Device
                           -> -- No documentation found for Nested "vkDestroyDescriptorSetLayout" "descriptorSetLayout"
                              DescriptorSetLayout
                           -> -- No documentation found for Nested "vkDestroyDescriptorSetLayout" "pAllocator"
                              ("allocator" ::: Maybe AllocationCallbacks)
                           -> io ()
destroyDescriptorSetLayout device descriptorSetLayout allocator = liftIO . evalContT $ do
  let vkDestroyDescriptorSetLayoutPtr = pVkDestroyDescriptorSetLayout (deviceCmds (device :: Device))
  lift $ unless (vkDestroyDescriptorSetLayoutPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDescriptorSetLayout is null" Nothing Nothing
  let vkDestroyDescriptorSetLayout' = mkVkDestroyDescriptorSetLayout vkDestroyDescriptorSetLayoutPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyDescriptorSetLayout' (deviceHandle (device)) (descriptorSetLayout) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorPool
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct DescriptorPoolCreateInfo) -> Ptr AllocationCallbacks -> Ptr DescriptorPool -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct DescriptorPoolCreateInfo) -> Ptr AllocationCallbacks -> Ptr DescriptorPool -> IO Result

-- No documentation found for TopLevel "vkCreateDescriptorPool"
createDescriptorPool :: forall a io
                      . (Extendss DescriptorPoolCreateInfo a, PokeChain a, MonadIO io)
                     => -- No documentation found for Nested "vkCreateDescriptorPool" "device"
                        Device
                     -> -- No documentation found for Nested "vkCreateDescriptorPool" "pCreateInfo"
                        (DescriptorPoolCreateInfo a)
                     -> -- No documentation found for Nested "vkCreateDescriptorPool" "pAllocator"
                        ("allocator" ::: Maybe AllocationCallbacks)
                     -> io (DescriptorPool)
createDescriptorPool device createInfo allocator = liftIO . evalContT $ do
  let vkCreateDescriptorPoolPtr = pVkCreateDescriptorPool (deviceCmds (device :: Device))
  lift $ unless (vkCreateDescriptorPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDescriptorPool is null" Nothing Nothing
  let vkCreateDescriptorPool' = mkVkCreateDescriptorPool vkCreateDescriptorPoolPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPDescriptorPool <- ContT $ bracket (callocBytes @DescriptorPool 8) free
  r <- lift $ vkCreateDescriptorPool' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPDescriptorPool)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDescriptorPool <- lift $ peek @DescriptorPool pPDescriptorPool
  pure $ (pDescriptorPool)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDescriptorPool' and 'destroyDescriptorPool'
--
-- To ensure that 'destroyDescriptorPool' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDescriptorPool :: forall a io r . (Extendss DescriptorPoolCreateInfo a, PokeChain a, MonadIO io) => Device -> DescriptorPoolCreateInfo a -> Maybe AllocationCallbacks -> (io DescriptorPool -> (DescriptorPool -> io ()) -> r) -> r
withDescriptorPool device pCreateInfo pAllocator b =
  b (createDescriptorPool device pCreateInfo pAllocator)
    (\(o0) -> destroyDescriptorPool device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorPool
  :: FunPtr (Ptr Device_T -> DescriptorPool -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> DescriptorPool -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyDescriptorPool"
destroyDescriptorPool :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vkDestroyDescriptorPool" "device"
                         Device
                      -> -- No documentation found for Nested "vkDestroyDescriptorPool" "descriptorPool"
                         DescriptorPool
                      -> -- No documentation found for Nested "vkDestroyDescriptorPool" "pAllocator"
                         ("allocator" ::: Maybe AllocationCallbacks)
                      -> io ()
destroyDescriptorPool device descriptorPool allocator = liftIO . evalContT $ do
  let vkDestroyDescriptorPoolPtr = pVkDestroyDescriptorPool (deviceCmds (device :: Device))
  lift $ unless (vkDestroyDescriptorPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDescriptorPool is null" Nothing Nothing
  let vkDestroyDescriptorPool' = mkVkDestroyDescriptorPool vkDestroyDescriptorPoolPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyDescriptorPool' (deviceHandle (device)) (descriptorPool) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetDescriptorPool
  :: FunPtr (Ptr Device_T -> DescriptorPool -> DescriptorPoolResetFlags -> IO Result) -> Ptr Device_T -> DescriptorPool -> DescriptorPoolResetFlags -> IO Result

-- No documentation found for TopLevel "vkResetDescriptorPool"
resetDescriptorPool :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkResetDescriptorPool" "device"
                       Device
                    -> -- No documentation found for Nested "vkResetDescriptorPool" "descriptorPool"
                       DescriptorPool
                    -> -- No documentation found for Nested "vkResetDescriptorPool" "flags"
                       DescriptorPoolResetFlags
                    -> io ()
resetDescriptorPool device descriptorPool flags = liftIO $ do
  let vkResetDescriptorPoolPtr = pVkResetDescriptorPool (deviceCmds (device :: Device))
  unless (vkResetDescriptorPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetDescriptorPool is null" Nothing Nothing
  let vkResetDescriptorPool' = mkVkResetDescriptorPool vkResetDescriptorPoolPtr
  _ <- vkResetDescriptorPool' (deviceHandle (device)) (descriptorPool) (flags)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateDescriptorSets
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct DescriptorSetAllocateInfo) -> Ptr DescriptorSet -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct DescriptorSetAllocateInfo) -> Ptr DescriptorSet -> IO Result

-- No documentation found for TopLevel "vkAllocateDescriptorSets"
allocateDescriptorSets :: forall a io
                        . (Extendss DescriptorSetAllocateInfo a, PokeChain a, MonadIO io)
                       => -- No documentation found for Nested "vkAllocateDescriptorSets" "device"
                          Device
                       -> -- No documentation found for Nested "vkAllocateDescriptorSets" "pAllocateInfo"
                          (DescriptorSetAllocateInfo a)
                       -> io (("descriptorSets" ::: Vector DescriptorSet))
allocateDescriptorSets device allocateInfo = liftIO . evalContT $ do
  let vkAllocateDescriptorSetsPtr = pVkAllocateDescriptorSets (deviceCmds (device :: Device))
  lift $ unless (vkAllocateDescriptorSetsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAllocateDescriptorSets is null" Nothing Nothing
  let vkAllocateDescriptorSets' = mkVkAllocateDescriptorSets vkAllocateDescriptorSetsPtr
  pAllocateInfo <- ContT $ withCStruct (allocateInfo)
  pPDescriptorSets <- ContT $ bracket (callocBytes @DescriptorSet ((fromIntegral . Data.Vector.length . setLayouts $ (allocateInfo)) * 8)) free
  r <- lift $ vkAllocateDescriptorSets' (deviceHandle (device)) (forgetExtensions pAllocateInfo) (pPDescriptorSets)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDescriptorSets <- lift $ generateM (fromIntegral . Data.Vector.length . setLayouts $ (allocateInfo)) (\i -> peek @DescriptorSet ((pPDescriptorSets `advancePtrBytes` (8 * (i)) :: Ptr DescriptorSet)))
  pure $ (pDescriptorSets)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'allocateDescriptorSets' and 'freeDescriptorSets'
--
-- To ensure that 'freeDescriptorSets' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDescriptorSets :: forall a io r . (Extendss DescriptorSetAllocateInfo a, PokeChain a, MonadIO io) => Device -> DescriptorSetAllocateInfo a -> (io (Vector DescriptorSet) -> (Vector DescriptorSet -> io ()) -> r) -> r
withDescriptorSets device pAllocateInfo b =
  b (allocateDescriptorSets device pAllocateInfo)
    (\(o0) -> freeDescriptorSets device (descriptorPool (pAllocateInfo :: DescriptorSetAllocateInfo a)) o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeDescriptorSets
  :: FunPtr (Ptr Device_T -> DescriptorPool -> Word32 -> Ptr DescriptorSet -> IO Result) -> Ptr Device_T -> DescriptorPool -> Word32 -> Ptr DescriptorSet -> IO Result

-- No documentation found for TopLevel "vkFreeDescriptorSets"
freeDescriptorSets :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkFreeDescriptorSets" "device"
                      Device
                   -> -- No documentation found for Nested "vkFreeDescriptorSets" "descriptorPool"
                      DescriptorPool
                   -> -- No documentation found for Nested "vkFreeDescriptorSets" "pDescriptorSets"
                      ("descriptorSets" ::: Vector DescriptorSet)
                   -> io ()
freeDescriptorSets device descriptorPool descriptorSets = liftIO . evalContT $ do
  let vkFreeDescriptorSetsPtr = pVkFreeDescriptorSets (deviceCmds (device :: Device))
  lift $ unless (vkFreeDescriptorSetsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkFreeDescriptorSets is null" Nothing Nothing
  let vkFreeDescriptorSets' = mkVkFreeDescriptorSets vkFreeDescriptorSetsPtr
  pPDescriptorSets <- ContT $ allocaBytesAligned @DescriptorSet ((Data.Vector.length (descriptorSets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorSets `plusPtr` (8 * (i)) :: Ptr DescriptorSet) (e)) (descriptorSets)
  _ <- lift $ vkFreeDescriptorSets' (deviceHandle (device)) (descriptorPool) ((fromIntegral (Data.Vector.length $ (descriptorSets)) :: Word32)) (pPDescriptorSets)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUpdateDescriptorSets
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr (SomeStruct WriteDescriptorSet) -> Word32 -> Ptr CopyDescriptorSet -> IO ()) -> Ptr Device_T -> Word32 -> Ptr (SomeStruct WriteDescriptorSet) -> Word32 -> Ptr CopyDescriptorSet -> IO ()

-- No documentation found for TopLevel "vkUpdateDescriptorSets"
updateDescriptorSets :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkUpdateDescriptorSets" "device"
                        Device
                     -> -- No documentation found for Nested "vkUpdateDescriptorSets" "pDescriptorWrites"
                        ("descriptorWrites" ::: Vector (SomeStruct WriteDescriptorSet))
                     -> -- No documentation found for Nested "vkUpdateDescriptorSets" "pDescriptorCopies"
                        ("descriptorCopies" ::: Vector CopyDescriptorSet)
                     -> io ()
updateDescriptorSets device descriptorWrites descriptorCopies = liftIO . evalContT $ do
  let vkUpdateDescriptorSetsPtr = pVkUpdateDescriptorSets (deviceCmds (device :: Device))
  lift $ unless (vkUpdateDescriptorSetsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkUpdateDescriptorSets is null" Nothing Nothing
  let vkUpdateDescriptorSets' = mkVkUpdateDescriptorSets vkUpdateDescriptorSetsPtr
  pPDescriptorWrites <- ContT $ allocaBytesAligned @(WriteDescriptorSet _) ((Data.Vector.length (descriptorWrites)) * 64) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPDescriptorWrites `plusPtr` (64 * (i)) :: Ptr (WriteDescriptorSet _))) (e) . ($ ())) (descriptorWrites)
  pPDescriptorCopies <- ContT $ allocaBytesAligned @CopyDescriptorSet ((Data.Vector.length (descriptorCopies)) * 56) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorCopies `plusPtr` (56 * (i)) :: Ptr CopyDescriptorSet) (e)) (descriptorCopies)
  lift $ vkUpdateDescriptorSets' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (descriptorWrites)) :: Word32)) (forgetExtensions (pPDescriptorWrites)) ((fromIntegral (Data.Vector.length $ (descriptorCopies)) :: Word32)) (pPDescriptorCopies)
  pure $ ()



-- No documentation found for TopLevel "VkDescriptorBufferInfo"
data DescriptorBufferInfo = DescriptorBufferInfo
  { -- No documentation found for Nested "VkDescriptorBufferInfo" "buffer"
    buffer :: Buffer
  , -- No documentation found for Nested "VkDescriptorBufferInfo" "offset"
    offset :: DeviceSize
  , -- No documentation found for Nested "VkDescriptorBufferInfo" "range"
    range :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorBufferInfo)
#endif
deriving instance Show DescriptorBufferInfo

instance ToCStruct DescriptorBufferInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorBufferInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (range)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct DescriptorBufferInfo where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 0 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    range <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ DescriptorBufferInfo
             buffer offset range


instance Storable DescriptorBufferInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorBufferInfo where
  zero = DescriptorBufferInfo
           zero
           zero
           zero



-- No documentation found for TopLevel "VkDescriptorImageInfo"
data DescriptorImageInfo = DescriptorImageInfo
  { -- No documentation found for Nested "VkDescriptorImageInfo" "sampler"
    sampler :: Sampler
  , -- No documentation found for Nested "VkDescriptorImageInfo" "imageView"
    imageView :: ImageView
  , -- No documentation found for Nested "VkDescriptorImageInfo" "imageLayout"
    imageLayout :: ImageLayout
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorImageInfo)
#endif
deriving instance Show DescriptorImageInfo

instance ToCStruct DescriptorImageInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorImageInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Sampler)) (sampler)
    poke ((p `plusPtr` 8 :: Ptr ImageView)) (imageView)
    poke ((p `plusPtr` 16 :: Ptr ImageLayout)) (imageLayout)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Sampler)) (zero)
    poke ((p `plusPtr` 8 :: Ptr ImageView)) (zero)
    poke ((p `plusPtr` 16 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct DescriptorImageInfo where
  peekCStruct p = do
    sampler <- peek @Sampler ((p `plusPtr` 0 :: Ptr Sampler))
    imageView <- peek @ImageView ((p `plusPtr` 8 :: Ptr ImageView))
    imageLayout <- peek @ImageLayout ((p `plusPtr` 16 :: Ptr ImageLayout))
    pure $ DescriptorImageInfo
             sampler imageView imageLayout


instance Storable DescriptorImageInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorImageInfo where
  zero = DescriptorImageInfo
           zero
           zero
           zero



-- No documentation found for TopLevel "VkWriteDescriptorSet"
data WriteDescriptorSet (es :: [Type]) = WriteDescriptorSet
  { -- No documentation found for Nested "VkWriteDescriptorSet" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkWriteDescriptorSet" "dstSet"
    dstSet :: DescriptorSet
  , -- No documentation found for Nested "VkWriteDescriptorSet" "dstBinding"
    dstBinding :: Word32
  , -- No documentation found for Nested "VkWriteDescriptorSet" "dstArrayElement"
    dstArrayElement :: Word32
  , -- No documentation found for Nested "VkWriteDescriptorSet" "descriptorCount"
    descriptorCount :: Word32
  , -- No documentation found for Nested "VkWriteDescriptorSet" "descriptorType"
    descriptorType :: DescriptorType
  , -- No documentation found for Nested "VkWriteDescriptorSet" "pImageInfo"
    imageInfo :: Vector DescriptorImageInfo
  , -- No documentation found for Nested "VkWriteDescriptorSet" "pBufferInfo"
    bufferInfo :: Vector DescriptorBufferInfo
  , -- No documentation found for Nested "VkWriteDescriptorSet" "pTexelBufferView"
    texelBufferView :: Vector BufferView
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WriteDescriptorSet (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (WriteDescriptorSet es)

instance Extensible WriteDescriptorSet where
  extensibleType = STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  setNext x next = x{next = next}
  getNext WriteDescriptorSet{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends WriteDescriptorSet e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @WriteDescriptorSetAccelerationStructureNV = Just f
    | Just Refl <- eqT @e @WriteDescriptorSetAccelerationStructureKHR = Just f
    | Just Refl <- eqT @e @WriteDescriptorSetInlineUniformBlockEXT = Just f
    | otherwise = Nothing

instance (Extendss WriteDescriptorSet es, PokeChain es) => ToCStruct (WriteDescriptorSet es) where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteDescriptorSet{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorSet)) (dstSet)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (dstBinding)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (dstArrayElement)
    let pImageInfoLength = Data.Vector.length $ (imageInfo)
    lift $ unless (fromIntegral pImageInfoLength == (descriptorCount) || pImageInfoLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pImageInfo must be empty or have 'descriptorCount' elements" Nothing Nothing
    let pBufferInfoLength = Data.Vector.length $ (bufferInfo)
    lift $ unless (fromIntegral pBufferInfoLength == (descriptorCount) || pBufferInfoLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pBufferInfo must be empty or have 'descriptorCount' elements" Nothing Nothing
    let pTexelBufferViewLength = Data.Vector.length $ (texelBufferView)
    lift $ unless (fromIntegral pTexelBufferViewLength == (descriptorCount) || pTexelBufferViewLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pTexelBufferView must be empty or have 'descriptorCount' elements" Nothing Nothing
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((descriptorCount))
    lift $ poke ((p `plusPtr` 36 :: Ptr DescriptorType)) (descriptorType)
    pImageInfo'' <- if Data.Vector.null (imageInfo)
      then pure nullPtr
      else do
        pPImageInfo <- ContT $ allocaBytesAligned @DescriptorImageInfo (((Data.Vector.length (imageInfo))) * 24) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPImageInfo `plusPtr` (24 * (i)) :: Ptr DescriptorImageInfo) (e)) ((imageInfo))
        pure $ pPImageInfo
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr DescriptorImageInfo))) pImageInfo''
    pBufferInfo'' <- if Data.Vector.null (bufferInfo)
      then pure nullPtr
      else do
        pPBufferInfo <- ContT $ allocaBytesAligned @DescriptorBufferInfo (((Data.Vector.length (bufferInfo))) * 24) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPBufferInfo `plusPtr` (24 * (i)) :: Ptr DescriptorBufferInfo) (e)) ((bufferInfo))
        pure $ pPBufferInfo
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr DescriptorBufferInfo))) pBufferInfo''
    pTexelBufferView'' <- if Data.Vector.null (texelBufferView)
      then pure nullPtr
      else do
        pPTexelBufferView <- ContT $ allocaBytesAligned @BufferView (((Data.Vector.length (texelBufferView))) * 8) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPTexelBufferView `plusPtr` (8 * (i)) :: Ptr BufferView) (e)) ((texelBufferView))
        pure $ pPTexelBufferView
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr BufferView))) pTexelBufferView''
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorSet)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr DescriptorType)) (zero)
    lift $ f

instance (Extendss WriteDescriptorSet es, PeekChain es) => FromCStruct (WriteDescriptorSet es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    dstSet <- peek @DescriptorSet ((p `plusPtr` 16 :: Ptr DescriptorSet))
    dstBinding <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    dstArrayElement <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    descriptorCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    descriptorType <- peek @DescriptorType ((p `plusPtr` 36 :: Ptr DescriptorType))
    pImageInfo <- peek @(Ptr DescriptorImageInfo) ((p `plusPtr` 40 :: Ptr (Ptr DescriptorImageInfo)))
    let pImageInfoLength = if pImageInfo == nullPtr then 0 else (fromIntegral descriptorCount)
    pImageInfo' <- generateM pImageInfoLength (\i -> peekCStruct @DescriptorImageInfo ((pImageInfo `advancePtrBytes` (24 * (i)) :: Ptr DescriptorImageInfo)))
    pBufferInfo <- peek @(Ptr DescriptorBufferInfo) ((p `plusPtr` 48 :: Ptr (Ptr DescriptorBufferInfo)))
    let pBufferInfoLength = if pBufferInfo == nullPtr then 0 else (fromIntegral descriptorCount)
    pBufferInfo' <- generateM pBufferInfoLength (\i -> peekCStruct @DescriptorBufferInfo ((pBufferInfo `advancePtrBytes` (24 * (i)) :: Ptr DescriptorBufferInfo)))
    pTexelBufferView <- peek @(Ptr BufferView) ((p `plusPtr` 56 :: Ptr (Ptr BufferView)))
    let pTexelBufferViewLength = if pTexelBufferView == nullPtr then 0 else (fromIntegral descriptorCount)
    pTexelBufferView' <- generateM pTexelBufferViewLength (\i -> peek @BufferView ((pTexelBufferView `advancePtrBytes` (8 * (i)) :: Ptr BufferView)))
    pure $ WriteDescriptorSet
             next dstSet dstBinding dstArrayElement descriptorCount descriptorType pImageInfo' pBufferInfo' pTexelBufferView'

instance es ~ '[] => Zero (WriteDescriptorSet es) where
  zero = WriteDescriptorSet
           ()
           zero
           zero
           zero
           zero
           zero
           mempty
           mempty
           mempty



-- No documentation found for TopLevel "VkCopyDescriptorSet"
data CopyDescriptorSet = CopyDescriptorSet
  { -- No documentation found for Nested "VkCopyDescriptorSet" "srcSet"
    srcSet :: DescriptorSet
  , -- No documentation found for Nested "VkCopyDescriptorSet" "srcBinding"
    srcBinding :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "srcArrayElement"
    srcArrayElement :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "dstSet"
    dstSet :: DescriptorSet
  , -- No documentation found for Nested "VkCopyDescriptorSet" "dstBinding"
    dstBinding :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "dstArrayElement"
    dstArrayElement :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "descriptorCount"
    descriptorCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyDescriptorSet)
#endif
deriving instance Show CopyDescriptorSet

instance ToCStruct CopyDescriptorSet where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyDescriptorSet{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_DESCRIPTOR_SET)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DescriptorSet)) (srcSet)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (srcBinding)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (srcArrayElement)
    poke ((p `plusPtr` 32 :: Ptr DescriptorSet)) (dstSet)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (dstBinding)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (dstArrayElement)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (descriptorCount)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_DESCRIPTOR_SET)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DescriptorSet)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DescriptorSet)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    f

instance FromCStruct CopyDescriptorSet where
  peekCStruct p = do
    srcSet <- peek @DescriptorSet ((p `plusPtr` 16 :: Ptr DescriptorSet))
    srcBinding <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    srcArrayElement <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    dstSet <- peek @DescriptorSet ((p `plusPtr` 32 :: Ptr DescriptorSet))
    dstBinding <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    dstArrayElement <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    descriptorCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pure $ CopyDescriptorSet
             srcSet srcBinding srcArrayElement dstSet dstBinding dstArrayElement descriptorCount


instance Storable CopyDescriptorSet where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CopyDescriptorSet where
  zero = CopyDescriptorSet
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkDescriptorSetLayoutBinding"
data DescriptorSetLayoutBinding = DescriptorSetLayoutBinding
  { -- No documentation found for Nested "VkDescriptorSetLayoutBinding" "binding"
    binding :: Word32
  , -- No documentation found for Nested "VkDescriptorSetLayoutBinding" "descriptorType"
    descriptorType :: DescriptorType
  , -- No documentation found for Nested "VkDescriptorSetLayoutBinding" "descriptorCount"
    descriptorCount :: Word32
  , -- No documentation found for Nested "VkDescriptorSetLayoutBinding" "stageFlags"
    stageFlags :: ShaderStageFlags
  , -- No documentation found for Nested "VkDescriptorSetLayoutBinding" "pImmutableSamplers"
    immutableSamplers :: Vector Sampler
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetLayoutBinding)
#endif
deriving instance Show DescriptorSetLayoutBinding

instance ToCStruct DescriptorSetLayoutBinding where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetLayoutBinding{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (binding)
    lift $ poke ((p `plusPtr` 4 :: Ptr DescriptorType)) (descriptorType)
    let pImmutableSamplersLength = Data.Vector.length $ (immutableSamplers)
    descriptorCount'' <- lift $ if (descriptorCount) == 0
      then pure $ fromIntegral pImmutableSamplersLength
      else do
        unless (fromIntegral pImmutableSamplersLength == (descriptorCount) || pImmutableSamplersLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pImmutableSamplers must be empty or have 'descriptorCount' elements" Nothing Nothing
        pure (descriptorCount)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) (descriptorCount'')
    lift $ poke ((p `plusPtr` 12 :: Ptr ShaderStageFlags)) (stageFlags)
    pImmutableSamplers'' <- if Data.Vector.null (immutableSamplers)
      then pure nullPtr
      else do
        pPImmutableSamplers <- ContT $ allocaBytesAligned @Sampler (((Data.Vector.length (immutableSamplers))) * 8) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPImmutableSamplers `plusPtr` (8 * (i)) :: Ptr Sampler) (e)) ((immutableSamplers))
        pure $ pPImmutableSamplers
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr Sampler))) pImmutableSamplers''
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr DescriptorType)) (zero)
    poke ((p `plusPtr` 12 :: Ptr ShaderStageFlags)) (zero)
    f

instance FromCStruct DescriptorSetLayoutBinding where
  peekCStruct p = do
    binding <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    descriptorType <- peek @DescriptorType ((p `plusPtr` 4 :: Ptr DescriptorType))
    descriptorCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    stageFlags <- peek @ShaderStageFlags ((p `plusPtr` 12 :: Ptr ShaderStageFlags))
    pImmutableSamplers <- peek @(Ptr Sampler) ((p `plusPtr` 16 :: Ptr (Ptr Sampler)))
    let pImmutableSamplersLength = if pImmutableSamplers == nullPtr then 0 else (fromIntegral descriptorCount)
    pImmutableSamplers' <- generateM pImmutableSamplersLength (\i -> peek @Sampler ((pImmutableSamplers `advancePtrBytes` (8 * (i)) :: Ptr Sampler)))
    pure $ DescriptorSetLayoutBinding
             binding descriptorType descriptorCount stageFlags pImmutableSamplers'

instance Zero DescriptorSetLayoutBinding where
  zero = DescriptorSetLayoutBinding
           zero
           zero
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkDescriptorSetLayoutCreateInfo"
data DescriptorSetLayoutCreateInfo (es :: [Type]) = DescriptorSetLayoutCreateInfo
  { -- No documentation found for Nested "VkDescriptorSetLayoutCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkDescriptorSetLayoutCreateInfo" "flags"
    flags :: DescriptorSetLayoutCreateFlags
  , -- No documentation found for Nested "VkDescriptorSetLayoutCreateInfo" "pBindings"
    bindings :: Vector DescriptorSetLayoutBinding
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetLayoutCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DescriptorSetLayoutCreateInfo es)

instance Extensible DescriptorSetLayoutCreateInfo where
  extensibleType = STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
  setNext x next = x{next = next}
  getNext DescriptorSetLayoutCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DescriptorSetLayoutCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DescriptorSetLayoutBindingFlagsCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss DescriptorSetLayoutCreateInfo es, PokeChain es) => ToCStruct (DescriptorSetLayoutCreateInfo es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetLayoutCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorSetLayoutCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (bindings)) :: Word32))
    pPBindings' <- ContT $ allocaBytesAligned @DescriptorSetLayoutBinding ((Data.Vector.length (bindings)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPBindings' `plusPtr` (24 * (i)) :: Ptr DescriptorSetLayoutBinding) (e) . ($ ())) (bindings)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorSetLayoutBinding))) (pPBindings')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    pPBindings' <- ContT $ allocaBytesAligned @DescriptorSetLayoutBinding ((Data.Vector.length (mempty)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPBindings' `plusPtr` (24 * (i)) :: Ptr DescriptorSetLayoutBinding) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorSetLayoutBinding))) (pPBindings')
    lift $ f

instance (Extendss DescriptorSetLayoutCreateInfo es, PeekChain es) => FromCStruct (DescriptorSetLayoutCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @DescriptorSetLayoutCreateFlags ((p `plusPtr` 16 :: Ptr DescriptorSetLayoutCreateFlags))
    bindingCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pBindings <- peek @(Ptr DescriptorSetLayoutBinding) ((p `plusPtr` 24 :: Ptr (Ptr DescriptorSetLayoutBinding)))
    pBindings' <- generateM (fromIntegral bindingCount) (\i -> peekCStruct @DescriptorSetLayoutBinding ((pBindings `advancePtrBytes` (24 * (i)) :: Ptr DescriptorSetLayoutBinding)))
    pure $ DescriptorSetLayoutCreateInfo
             next flags pBindings'

instance es ~ '[] => Zero (DescriptorSetLayoutCreateInfo es) where
  zero = DescriptorSetLayoutCreateInfo
           ()
           zero
           mempty



-- No documentation found for TopLevel "VkDescriptorPoolSize"
data DescriptorPoolSize = DescriptorPoolSize
  { -- No documentation found for Nested "VkDescriptorPoolSize" "type"
    type' :: DescriptorType
  , -- No documentation found for Nested "VkDescriptorPoolSize" "descriptorCount"
    descriptorCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorPoolSize)
#endif
deriving instance Show DescriptorPoolSize

instance ToCStruct DescriptorPoolSize where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorPoolSize{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DescriptorType)) (type')
    poke ((p `plusPtr` 4 :: Ptr Word32)) (descriptorCount)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DescriptorType)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorPoolSize where
  peekCStruct p = do
    type' <- peek @DescriptorType ((p `plusPtr` 0 :: Ptr DescriptorType))
    descriptorCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ DescriptorPoolSize
             type' descriptorCount


instance Storable DescriptorPoolSize where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorPoolSize where
  zero = DescriptorPoolSize
           zero
           zero



-- No documentation found for TopLevel "VkDescriptorPoolCreateInfo"
data DescriptorPoolCreateInfo (es :: [Type]) = DescriptorPoolCreateInfo
  { -- No documentation found for Nested "VkDescriptorPoolCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkDescriptorPoolCreateInfo" "flags"
    flags :: DescriptorPoolCreateFlags
  , -- No documentation found for Nested "VkDescriptorPoolCreateInfo" "maxSets"
    maxSets :: Word32
  , -- No documentation found for Nested "VkDescriptorPoolCreateInfo" "pPoolSizes"
    poolSizes :: Vector DescriptorPoolSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorPoolCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DescriptorPoolCreateInfo es)

instance Extensible DescriptorPoolCreateInfo where
  extensibleType = STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
  setNext x next = x{next = next}
  getNext DescriptorPoolCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DescriptorPoolCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DescriptorPoolInlineUniformBlockCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss DescriptorPoolCreateInfo es, PokeChain es) => ToCStruct (DescriptorPoolCreateInfo es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorPoolCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorPoolCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (maxSets)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (poolSizes)) :: Word32))
    pPPoolSizes' <- ContT $ allocaBytesAligned @DescriptorPoolSize ((Data.Vector.length (poolSizes)) * 8) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPoolSizes' `plusPtr` (8 * (i)) :: Ptr DescriptorPoolSize) (e)) (poolSizes)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr DescriptorPoolSize))) (pPPoolSizes')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    pPPoolSizes' <- ContT $ allocaBytesAligned @DescriptorPoolSize ((Data.Vector.length (mempty)) * 8) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPoolSizes' `plusPtr` (8 * (i)) :: Ptr DescriptorPoolSize) (e)) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr DescriptorPoolSize))) (pPPoolSizes')
    lift $ f

instance (Extendss DescriptorPoolCreateInfo es, PeekChain es) => FromCStruct (DescriptorPoolCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @DescriptorPoolCreateFlags ((p `plusPtr` 16 :: Ptr DescriptorPoolCreateFlags))
    maxSets <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    poolSizeCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pPoolSizes <- peek @(Ptr DescriptorPoolSize) ((p `plusPtr` 32 :: Ptr (Ptr DescriptorPoolSize)))
    pPoolSizes' <- generateM (fromIntegral poolSizeCount) (\i -> peekCStruct @DescriptorPoolSize ((pPoolSizes `advancePtrBytes` (8 * (i)) :: Ptr DescriptorPoolSize)))
    pure $ DescriptorPoolCreateInfo
             next flags maxSets pPoolSizes'

instance es ~ '[] => Zero (DescriptorPoolCreateInfo es) where
  zero = DescriptorPoolCreateInfo
           ()
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkDescriptorSetAllocateInfo"
data DescriptorSetAllocateInfo (es :: [Type]) = DescriptorSetAllocateInfo
  { -- No documentation found for Nested "VkDescriptorSetAllocateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkDescriptorSetAllocateInfo" "descriptorPool"
    descriptorPool :: DescriptorPool
  , -- No documentation found for Nested "VkDescriptorSetAllocateInfo" "pSetLayouts"
    setLayouts :: Vector DescriptorSetLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetAllocateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DescriptorSetAllocateInfo es)

instance Extensible DescriptorSetAllocateInfo where
  extensibleType = STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
  setNext x next = x{next = next}
  getNext DescriptorSetAllocateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DescriptorSetAllocateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DescriptorSetVariableDescriptorCountAllocateInfo = Just f
    | otherwise = Nothing

instance (Extendss DescriptorSetAllocateInfo es, PokeChain es) => ToCStruct (DescriptorSetAllocateInfo es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetAllocateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorPool)) (descriptorPool)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (setLayouts)) :: Word32))
    pPSetLayouts' <- ContT $ allocaBytesAligned @DescriptorSetLayout ((Data.Vector.length (setLayouts)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSetLayouts' `plusPtr` (8 * (i)) :: Ptr DescriptorSetLayout) (e)) (setLayouts)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr DescriptorSetLayout))) (pPSetLayouts')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorPool)) (zero)
    pPSetLayouts' <- ContT $ allocaBytesAligned @DescriptorSetLayout ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSetLayouts' `plusPtr` (8 * (i)) :: Ptr DescriptorSetLayout) (e)) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr DescriptorSetLayout))) (pPSetLayouts')
    lift $ f

instance (Extendss DescriptorSetAllocateInfo es, PeekChain es) => FromCStruct (DescriptorSetAllocateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    descriptorPool <- peek @DescriptorPool ((p `plusPtr` 16 :: Ptr DescriptorPool))
    descriptorSetCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pSetLayouts <- peek @(Ptr DescriptorSetLayout) ((p `plusPtr` 32 :: Ptr (Ptr DescriptorSetLayout)))
    pSetLayouts' <- generateM (fromIntegral descriptorSetCount) (\i -> peek @DescriptorSetLayout ((pSetLayouts `advancePtrBytes` (8 * (i)) :: Ptr DescriptorSetLayout)))
    pure $ DescriptorSetAllocateInfo
             next descriptorPool pSetLayouts'

instance es ~ '[] => Zero (DescriptorSetAllocateInfo es) where
  zero = DescriptorSetAllocateInfo
           ()
           zero
           mempty

