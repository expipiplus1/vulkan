{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_bind_memory2"
module Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2  ( bindBufferMemory2
                                                        , bindImageMemory2
                                                        , BindBufferMemoryInfo(..)
                                                        , BindImageMemoryInfo(..)
                                                        , StructureType(..)
                                                        , ImageCreateFlagBits(..)
                                                        , ImageCreateFlags
                                                        ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2 (BindBufferMemoryDeviceGroupInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2 (BindImageMemoryDeviceGroupInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (BindImageMemorySwapchainInfoKHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (BindImagePlaneMemoryInfo)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkBindBufferMemory2))
import Vulkan.Dynamic (DeviceCmds(pVkBindImageMemory2))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindBufferMemory2
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr (SomeStruct BindBufferMemoryInfo) -> IO Result) -> Ptr Device_T -> Word32 -> Ptr (SomeStruct BindBufferMemoryInfo) -> IO Result

-- No documentation found for TopLevel "vkBindBufferMemory2"
bindBufferMemory2 :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkBindBufferMemory2" "device"
                     Device
                  -> -- No documentation found for Nested "vkBindBufferMemory2" "pBindInfos"
                     ("bindInfos" ::: Vector (SomeStruct BindBufferMemoryInfo))
                  -> io ()
bindBufferMemory2 device bindInfos = liftIO . evalContT $ do
  let vkBindBufferMemory2Ptr = pVkBindBufferMemory2 (deviceCmds (device :: Device))
  lift $ unless (vkBindBufferMemory2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBindBufferMemory2 is null" Nothing Nothing
  let vkBindBufferMemory2' = mkVkBindBufferMemory2 vkBindBufferMemory2Ptr
  pPBindInfos <- ContT $ allocaBytesAligned @(BindBufferMemoryInfo _) ((Data.Vector.length (bindInfos)) * 40) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPBindInfos `plusPtr` (40 * (i)) :: Ptr (BindBufferMemoryInfo _))) (e) . ($ ())) (bindInfos)
  r <- lift $ vkBindBufferMemory2' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (bindInfos)) :: Word32)) (forgetExtensions (pPBindInfos))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindImageMemory2
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr (SomeStruct BindImageMemoryInfo) -> IO Result) -> Ptr Device_T -> Word32 -> Ptr (SomeStruct BindImageMemoryInfo) -> IO Result

-- No documentation found for TopLevel "vkBindImageMemory2"
bindImageMemory2 :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkBindImageMemory2" "device"
                    Device
                 -> -- No documentation found for Nested "vkBindImageMemory2" "pBindInfos"
                    ("bindInfos" ::: Vector (SomeStruct BindImageMemoryInfo))
                 -> io ()
bindImageMemory2 device bindInfos = liftIO . evalContT $ do
  let vkBindImageMemory2Ptr = pVkBindImageMemory2 (deviceCmds (device :: Device))
  lift $ unless (vkBindImageMemory2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBindImageMemory2 is null" Nothing Nothing
  let vkBindImageMemory2' = mkVkBindImageMemory2 vkBindImageMemory2Ptr
  pPBindInfos <- ContT $ allocaBytesAligned @(BindImageMemoryInfo _) ((Data.Vector.length (bindInfos)) * 40) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPBindInfos `plusPtr` (40 * (i)) :: Ptr (BindImageMemoryInfo _))) (e) . ($ ())) (bindInfos)
  r <- lift $ vkBindImageMemory2' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (bindInfos)) :: Word32)) (forgetExtensions (pPBindInfos))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkBindBufferMemoryInfo"
data BindBufferMemoryInfo (es :: [Type]) = BindBufferMemoryInfo
  { -- No documentation found for Nested "VkBindBufferMemoryInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkBindBufferMemoryInfo" "buffer"
    buffer :: Buffer
  , -- No documentation found for Nested "VkBindBufferMemoryInfo" "memory"
    memory :: DeviceMemory
  , -- No documentation found for Nested "VkBindBufferMemoryInfo" "memoryOffset"
    memoryOffset :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindBufferMemoryInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BindBufferMemoryInfo es)

instance Extensible BindBufferMemoryInfo where
  extensibleType = STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  setNext x next = x{next = next}
  getNext BindBufferMemoryInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BindBufferMemoryInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @BindBufferMemoryDeviceGroupInfo = Just f
    | otherwise = Nothing

instance (Extendss BindBufferMemoryInfo es, PokeChain es) => ToCStruct (BindBufferMemoryInfo es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindBufferMemoryInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Buffer)) (buffer)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (memory)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (memoryOffset)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    lift $ f

instance (Extendss BindBufferMemoryInfo es, PeekChain es) => FromCStruct (BindBufferMemoryInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    buffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ BindBufferMemoryInfo
             next buffer memory memoryOffset

instance es ~ '[] => Zero (BindBufferMemoryInfo es) where
  zero = BindBufferMemoryInfo
           ()
           zero
           zero
           zero



-- No documentation found for TopLevel "VkBindImageMemoryInfo"
data BindImageMemoryInfo (es :: [Type]) = BindImageMemoryInfo
  { -- No documentation found for Nested "VkBindImageMemoryInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkBindImageMemoryInfo" "image"
    image :: Image
  , -- No documentation found for Nested "VkBindImageMemoryInfo" "memory"
    memory :: DeviceMemory
  , -- No documentation found for Nested "VkBindImageMemoryInfo" "memoryOffset"
    memoryOffset :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindImageMemoryInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BindImageMemoryInfo es)

instance Extensible BindImageMemoryInfo where
  extensibleType = STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  setNext x next = x{next = next}
  getNext BindImageMemoryInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BindImageMemoryInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @BindImagePlaneMemoryInfo = Just f
    | Just Refl <- eqT @e @BindImageMemorySwapchainInfoKHR = Just f
    | Just Refl <- eqT @e @BindImageMemoryDeviceGroupInfo = Just f
    | otherwise = Nothing

instance (Extendss BindImageMemoryInfo es, PokeChain es) => ToCStruct (BindImageMemoryInfo es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindImageMemoryInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (image)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (memory)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (memoryOffset)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    lift $ f

instance (Extendss BindImageMemoryInfo es, PeekChain es) => FromCStruct (BindImageMemoryInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    image <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ BindImageMemoryInfo
             next image memory memoryOffset

instance es ~ '[] => Zero (BindImageMemoryInfo es) where
  zero = BindImageMemoryInfo
           ()
           zero
           zero
           zero

