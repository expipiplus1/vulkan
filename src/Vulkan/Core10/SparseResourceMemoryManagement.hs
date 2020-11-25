{-# language CPP #-}
-- No documentation found for Chapter "SparseResourceMemoryManagement"
module Vulkan.Core10.SparseResourceMemoryManagement  ( getImageSparseMemoryRequirements
                                                     , getPhysicalDeviceSparseImageFormatProperties
                                                     , queueBindSparse
                                                     , SparseImageFormatProperties(..)
                                                     , SparseImageMemoryRequirements(..)
                                                     , ImageSubresource(..)
                                                     , SparseMemoryBind(..)
                                                     , SparseImageMemoryBind(..)
                                                     , SparseBufferMemoryBindInfo(..)
                                                     , SparseImageOpaqueMemoryBindInfo(..)
                                                     , SparseImageMemoryBindInfo(..)
                                                     , BindSparseInfo(..)
                                                     , ImageAspectFlagBits(..)
                                                     , ImageAspectFlags
                                                     , SparseImageFormatFlagBits(..)
                                                     , SparseImageFormatFlags
                                                     , SparseMemoryBindFlagBits(..)
                                                     , SparseMemoryBindFlags
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
import Vulkan.Core10.Handles (Buffer)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageSparseMemoryRequirements))
import Vulkan.Dynamic (DeviceCmds(pVkQueueBindSparse))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupBindSparseInfo)
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.Format (Format(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageTiling (ImageTiling)
import Vulkan.Core10.Enums.ImageTiling (ImageTiling(..))
import Vulkan.Core10.Enums.ImageType (ImageType)
import Vulkan.Core10.Enums.ImageType (ImageType(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlagBits(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSparseImageFormatProperties))
import Vulkan.Core10.FundamentalTypes (Offset3D)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.SparseImageFormatFlagBits (SparseImageFormatFlags)
import Vulkan.Core10.Enums.SparseMemoryBindFlagBits (SparseMemoryBindFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (TimelineSemaphoreSubmitInfo)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_SPARSE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(..))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.SparseImageFormatFlagBits (SparseImageFormatFlagBits(..))
import Vulkan.Core10.Enums.SparseImageFormatFlagBits (SparseImageFormatFlags)
import Vulkan.Core10.Enums.SparseMemoryBindFlagBits (SparseMemoryBindFlagBits(..))
import Vulkan.Core10.Enums.SparseMemoryBindFlagBits (SparseMemoryBindFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSparseMemoryRequirements
  :: FunPtr (Ptr Device_T -> Image -> Ptr Word32 -> Ptr SparseImageMemoryRequirements -> IO ()) -> Ptr Device_T -> Image -> Ptr Word32 -> Ptr SparseImageMemoryRequirements -> IO ()

-- No documentation found for TopLevel "vkGetImageSparseMemoryRequirements"
getImageSparseMemoryRequirements :: forall io
                                  . (MonadIO io)
                                 => -- No documentation found for Nested "vkGetImageSparseMemoryRequirements" "device"
                                    Device
                                 -> -- No documentation found for Nested "vkGetImageSparseMemoryRequirements" "image"
                                    Image
                                 -> io (("sparseMemoryRequirements" ::: Vector SparseImageMemoryRequirements))
getImageSparseMemoryRequirements device image = liftIO . evalContT $ do
  let vkGetImageSparseMemoryRequirementsPtr = pVkGetImageSparseMemoryRequirements (deviceCmds (device :: Device))
  lift $ unless (vkGetImageSparseMemoryRequirementsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageSparseMemoryRequirements is null" Nothing Nothing
  let vkGetImageSparseMemoryRequirements' = mkVkGetImageSparseMemoryRequirements vkGetImageSparseMemoryRequirementsPtr
  let device' = deviceHandle (device)
  pPSparseMemoryRequirementCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ vkGetImageSparseMemoryRequirements' device' (image) (pPSparseMemoryRequirementCount) (nullPtr)
  pSparseMemoryRequirementCount <- lift $ peek @Word32 pPSparseMemoryRequirementCount
  pPSparseMemoryRequirements <- ContT $ bracket (callocBytes @SparseImageMemoryRequirements ((fromIntegral (pSparseMemoryRequirementCount)) * 48)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPSparseMemoryRequirements `advancePtrBytes` (i * 48) :: Ptr SparseImageMemoryRequirements) . ($ ())) [0..(fromIntegral (pSparseMemoryRequirementCount)) - 1]
  lift $ vkGetImageSparseMemoryRequirements' device' (image) (pPSparseMemoryRequirementCount) ((pPSparseMemoryRequirements))
  pSparseMemoryRequirementCount' <- lift $ peek @Word32 pPSparseMemoryRequirementCount
  pSparseMemoryRequirements' <- lift $ generateM (fromIntegral (pSparseMemoryRequirementCount')) (\i -> peekCStruct @SparseImageMemoryRequirements (((pPSparseMemoryRequirements) `advancePtrBytes` (48 * (i)) :: Ptr SparseImageMemoryRequirements)))
  pure $ (pSparseMemoryRequirements')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSparseImageFormatProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Format -> ImageType -> SampleCountFlagBits -> ImageUsageFlags -> ImageTiling -> Ptr Word32 -> Ptr SparseImageFormatProperties -> IO ()) -> Ptr PhysicalDevice_T -> Format -> ImageType -> SampleCountFlagBits -> ImageUsageFlags -> ImageTiling -> Ptr Word32 -> Ptr SparseImageFormatProperties -> IO ()

-- No documentation found for TopLevel "vkGetPhysicalDeviceSparseImageFormatProperties"
getPhysicalDeviceSparseImageFormatProperties :: forall io
                                              . (MonadIO io)
                                             => -- No documentation found for Nested "vkGetPhysicalDeviceSparseImageFormatProperties" "physicalDevice"
                                                PhysicalDevice
                                             -> -- No documentation found for Nested "vkGetPhysicalDeviceSparseImageFormatProperties" "format"
                                                Format
                                             -> -- No documentation found for Nested "vkGetPhysicalDeviceSparseImageFormatProperties" "type"
                                                ImageType
                                             -> -- No documentation found for Nested "vkGetPhysicalDeviceSparseImageFormatProperties" "samples"
                                                ("samples" ::: SampleCountFlagBits)
                                             -> -- No documentation found for Nested "vkGetPhysicalDeviceSparseImageFormatProperties" "usage"
                                                ImageUsageFlags
                                             -> -- No documentation found for Nested "vkGetPhysicalDeviceSparseImageFormatProperties" "tiling"
                                                ImageTiling
                                             -> io (("properties" ::: Vector SparseImageFormatProperties))
getPhysicalDeviceSparseImageFormatProperties physicalDevice format type' samples usage tiling = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSparseImageFormatPropertiesPtr = pVkGetPhysicalDeviceSparseImageFormatProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSparseImageFormatPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSparseImageFormatProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceSparseImageFormatProperties' = mkVkGetPhysicalDeviceSparseImageFormatProperties vkGetPhysicalDeviceSparseImageFormatPropertiesPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ vkGetPhysicalDeviceSparseImageFormatProperties' physicalDevice' (format) (type') (samples) (usage) (tiling) (pPPropertyCount) (nullPtr)
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @SparseImageFormatProperties ((fromIntegral (pPropertyCount)) * 20)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 20) :: Ptr SparseImageFormatProperties) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  lift $ vkGetPhysicalDeviceSparseImageFormatProperties' physicalDevice' (format) (type') (samples) (usage) (tiling) (pPPropertyCount) ((pPProperties))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @SparseImageFormatProperties (((pPProperties) `advancePtrBytes` (20 * (i)) :: Ptr SparseImageFormatProperties)))
  pure $ (pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueBindSparse
  :: FunPtr (Ptr Queue_T -> Word32 -> Ptr (SomeStruct BindSparseInfo) -> Fence -> IO Result) -> Ptr Queue_T -> Word32 -> Ptr (SomeStruct BindSparseInfo) -> Fence -> IO Result

-- No documentation found for TopLevel "vkQueueBindSparse"
queueBindSparse :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkQueueBindSparse" "queue"
                   Queue
                -> -- No documentation found for Nested "vkQueueBindSparse" "pBindInfo"
                   ("bindInfo" ::: Vector (SomeStruct BindSparseInfo))
                -> -- No documentation found for Nested "vkQueueBindSparse" "fence"
                   Fence
                -> io ()
queueBindSparse queue bindInfo fence = liftIO . evalContT $ do
  let vkQueueBindSparsePtr = pVkQueueBindSparse (deviceCmds (queue :: Queue))
  lift $ unless (vkQueueBindSparsePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueBindSparse is null" Nothing Nothing
  let vkQueueBindSparse' = mkVkQueueBindSparse vkQueueBindSparsePtr
  pPBindInfo <- ContT $ allocaBytesAligned @(BindSparseInfo _) ((Data.Vector.length (bindInfo)) * 96) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPBindInfo `plusPtr` (96 * (i)) :: Ptr (BindSparseInfo _))) (e) . ($ ())) (bindInfo)
  r <- lift $ vkQueueBindSparse' (queueHandle (queue)) ((fromIntegral (Data.Vector.length $ (bindInfo)) :: Word32)) (forgetExtensions (pPBindInfo)) (fence)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkSparseImageFormatProperties"
data SparseImageFormatProperties = SparseImageFormatProperties
  { -- No documentation found for Nested "VkSparseImageFormatProperties" "aspectMask"
    aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "VkSparseImageFormatProperties" "imageGranularity"
    imageGranularity :: Extent3D
  , -- No documentation found for Nested "VkSparseImageFormatProperties" "flags"
    flags :: SparseImageFormatFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseImageFormatProperties)
#endif
deriving instance Show SparseImageFormatProperties

instance ToCStruct SparseImageFormatProperties where
  withCStruct x f = allocaBytesAligned 20 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageFormatProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (aspectMask)
    poke ((p `plusPtr` 4 :: Ptr Extent3D)) (imageGranularity)
    poke ((p `plusPtr` 16 :: Ptr SparseImageFormatFlags)) (flags)
    f
  cStructSize = 20
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 4 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct SparseImageFormatProperties where
  peekCStruct p = do
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 0 :: Ptr ImageAspectFlags))
    imageGranularity <- peekCStruct @Extent3D ((p `plusPtr` 4 :: Ptr Extent3D))
    flags <- peek @SparseImageFormatFlags ((p `plusPtr` 16 :: Ptr SparseImageFormatFlags))
    pure $ SparseImageFormatProperties
             aspectMask imageGranularity flags


instance Storable SparseImageFormatProperties where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SparseImageFormatProperties where
  zero = SparseImageFormatProperties
           zero
           zero
           zero



-- No documentation found for TopLevel "VkSparseImageMemoryRequirements"
data SparseImageMemoryRequirements = SparseImageMemoryRequirements
  { -- No documentation found for Nested "VkSparseImageMemoryRequirements" "formatProperties"
    formatProperties :: SparseImageFormatProperties
  , -- No documentation found for Nested "VkSparseImageMemoryRequirements" "imageMipTailFirstLod"
    imageMipTailFirstLod :: Word32
  , -- No documentation found for Nested "VkSparseImageMemoryRequirements" "imageMipTailSize"
    imageMipTailSize :: DeviceSize
  , -- No documentation found for Nested "VkSparseImageMemoryRequirements" "imageMipTailOffset"
    imageMipTailOffset :: DeviceSize
  , -- No documentation found for Nested "VkSparseImageMemoryRequirements" "imageMipTailStride"
    imageMipTailStride :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseImageMemoryRequirements)
#endif
deriving instance Show SparseImageMemoryRequirements

instance ToCStruct SparseImageMemoryRequirements where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageMemoryRequirements{..} f = do
    poke ((p `plusPtr` 0 :: Ptr SparseImageFormatProperties)) (formatProperties)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (imageMipTailFirstLod)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (imageMipTailSize)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (imageMipTailOffset)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (imageMipTailStride)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr SparseImageFormatProperties)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct SparseImageMemoryRequirements where
  peekCStruct p = do
    formatProperties <- peekCStruct @SparseImageFormatProperties ((p `plusPtr` 0 :: Ptr SparseImageFormatProperties))
    imageMipTailFirstLod <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    imageMipTailSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    imageMipTailOffset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    imageMipTailStride <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    pure $ SparseImageMemoryRequirements
             formatProperties imageMipTailFirstLod imageMipTailSize imageMipTailOffset imageMipTailStride


instance Storable SparseImageMemoryRequirements where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SparseImageMemoryRequirements where
  zero = SparseImageMemoryRequirements
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkImageSubresource"
data ImageSubresource = ImageSubresource
  { -- No documentation found for Nested "VkImageSubresource" "aspectMask"
    aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "VkImageSubresource" "mipLevel"
    mipLevel :: Word32
  , -- No documentation found for Nested "VkImageSubresource" "arrayLayer"
    arrayLayer :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageSubresource)
#endif
deriving instance Show ImageSubresource

instance ToCStruct ImageSubresource where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSubresource{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (aspectMask)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (mipLevel)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (arrayLayer)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct ImageSubresource where
  peekCStruct p = do
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 0 :: Ptr ImageAspectFlags))
    mipLevel <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    arrayLayer <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ ImageSubresource
             aspectMask mipLevel arrayLayer


instance Storable ImageSubresource where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSubresource where
  zero = ImageSubresource
           zero
           zero
           zero



-- No documentation found for TopLevel "VkSparseMemoryBind"
data SparseMemoryBind = SparseMemoryBind
  { -- No documentation found for Nested "VkSparseMemoryBind" "resourceOffset"
    resourceOffset :: DeviceSize
  , -- No documentation found for Nested "VkSparseMemoryBind" "size"
    size :: DeviceSize
  , -- No documentation found for Nested "VkSparseMemoryBind" "memory"
    memory :: DeviceMemory
  , -- No documentation found for Nested "VkSparseMemoryBind" "memoryOffset"
    memoryOffset :: DeviceSize
  , -- No documentation found for Nested "VkSparseMemoryBind" "flags"
    flags :: SparseMemoryBindFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseMemoryBind)
#endif
deriving instance Show SparseMemoryBind

instance ToCStruct SparseMemoryBind where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseMemoryBind{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (resourceOffset)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (memoryOffset)
    poke ((p `plusPtr` 32 :: Ptr SparseMemoryBindFlags)) (flags)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct SparseMemoryBind where
  peekCStruct p = do
    resourceOffset <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    flags <- peek @SparseMemoryBindFlags ((p `plusPtr` 32 :: Ptr SparseMemoryBindFlags))
    pure $ SparseMemoryBind
             resourceOffset size memory memoryOffset flags


instance Storable SparseMemoryBind where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SparseMemoryBind where
  zero = SparseMemoryBind
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkSparseImageMemoryBind"
data SparseImageMemoryBind = SparseImageMemoryBind
  { -- No documentation found for Nested "VkSparseImageMemoryBind" "subresource"
    subresource :: ImageSubresource
  , -- No documentation found for Nested "VkSparseImageMemoryBind" "offset"
    offset :: Offset3D
  , -- No documentation found for Nested "VkSparseImageMemoryBind" "extent"
    extent :: Extent3D
  , -- No documentation found for Nested "VkSparseImageMemoryBind" "memory"
    memory :: DeviceMemory
  , -- No documentation found for Nested "VkSparseImageMemoryBind" "memoryOffset"
    memoryOffset :: DeviceSize
  , -- No documentation found for Nested "VkSparseImageMemoryBind" "flags"
    flags :: SparseMemoryBindFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseImageMemoryBind)
#endif
deriving instance Show SparseImageMemoryBind

instance ToCStruct SparseImageMemoryBind where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageMemoryBind{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresource)) (subresource)
    poke ((p `plusPtr` 12 :: Ptr Offset3D)) (offset)
    poke ((p `plusPtr` 24 :: Ptr Extent3D)) (extent)
    poke ((p `plusPtr` 40 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (memoryOffset)
    poke ((p `plusPtr` 56 :: Ptr SparseMemoryBindFlags)) (flags)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresource)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Extent3D)) (zero)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct SparseImageMemoryBind where
  peekCStruct p = do
    subresource <- peekCStruct @ImageSubresource ((p `plusPtr` 0 :: Ptr ImageSubresource))
    offset <- peekCStruct @Offset3D ((p `plusPtr` 12 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 24 :: Ptr Extent3D))
    memory <- peek @DeviceMemory ((p `plusPtr` 40 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    flags <- peek @SparseMemoryBindFlags ((p `plusPtr` 56 :: Ptr SparseMemoryBindFlags))
    pure $ SparseImageMemoryBind
             subresource offset extent memory memoryOffset flags


instance Storable SparseImageMemoryBind where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SparseImageMemoryBind where
  zero = SparseImageMemoryBind
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkSparseBufferMemoryBindInfo"
data SparseBufferMemoryBindInfo = SparseBufferMemoryBindInfo
  { -- No documentation found for Nested "VkSparseBufferMemoryBindInfo" "buffer"
    buffer :: Buffer
  , -- No documentation found for Nested "VkSparseBufferMemoryBindInfo" "pBinds"
    binds :: Vector SparseMemoryBind
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseBufferMemoryBindInfo)
#endif
deriving instance Show SparseBufferMemoryBindInfo

instance ToCStruct SparseBufferMemoryBindInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseBufferMemoryBindInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Buffer)) (buffer)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (binds)) :: Word32))
    pPBinds' <- ContT $ allocaBytesAligned @SparseMemoryBind ((Data.Vector.length (binds)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBinds' `plusPtr` (40 * (i)) :: Ptr SparseMemoryBind) (e)) (binds)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr SparseMemoryBind))) (pPBinds')
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Buffer)) (zero)
    pPBinds' <- ContT $ allocaBytesAligned @SparseMemoryBind ((Data.Vector.length (mempty)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBinds' `plusPtr` (40 * (i)) :: Ptr SparseMemoryBind) (e)) (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr SparseMemoryBind))) (pPBinds')
    lift $ f

instance FromCStruct SparseBufferMemoryBindInfo where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 0 :: Ptr Buffer))
    bindCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pBinds <- peek @(Ptr SparseMemoryBind) ((p `plusPtr` 16 :: Ptr (Ptr SparseMemoryBind)))
    pBinds' <- generateM (fromIntegral bindCount) (\i -> peekCStruct @SparseMemoryBind ((pBinds `advancePtrBytes` (40 * (i)) :: Ptr SparseMemoryBind)))
    pure $ SparseBufferMemoryBindInfo
             buffer pBinds'

instance Zero SparseBufferMemoryBindInfo where
  zero = SparseBufferMemoryBindInfo
           zero
           mempty



-- No documentation found for TopLevel "VkSparseImageOpaqueMemoryBindInfo"
data SparseImageOpaqueMemoryBindInfo = SparseImageOpaqueMemoryBindInfo
  { -- No documentation found for Nested "VkSparseImageOpaqueMemoryBindInfo" "image"
    image :: Image
  , -- No documentation found for Nested "VkSparseImageOpaqueMemoryBindInfo" "pBinds"
    binds :: Vector SparseMemoryBind
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseImageOpaqueMemoryBindInfo)
#endif
deriving instance Show SparseImageOpaqueMemoryBindInfo

instance ToCStruct SparseImageOpaqueMemoryBindInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageOpaqueMemoryBindInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Image)) (image)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (binds)) :: Word32))
    pPBinds' <- ContT $ allocaBytesAligned @SparseMemoryBind ((Data.Vector.length (binds)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBinds' `plusPtr` (40 * (i)) :: Ptr SparseMemoryBind) (e)) (binds)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr SparseMemoryBind))) (pPBinds')
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Image)) (zero)
    pPBinds' <- ContT $ allocaBytesAligned @SparseMemoryBind ((Data.Vector.length (mempty)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBinds' `plusPtr` (40 * (i)) :: Ptr SparseMemoryBind) (e)) (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr SparseMemoryBind))) (pPBinds')
    lift $ f

instance FromCStruct SparseImageOpaqueMemoryBindInfo where
  peekCStruct p = do
    image <- peek @Image ((p `plusPtr` 0 :: Ptr Image))
    bindCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pBinds <- peek @(Ptr SparseMemoryBind) ((p `plusPtr` 16 :: Ptr (Ptr SparseMemoryBind)))
    pBinds' <- generateM (fromIntegral bindCount) (\i -> peekCStruct @SparseMemoryBind ((pBinds `advancePtrBytes` (40 * (i)) :: Ptr SparseMemoryBind)))
    pure $ SparseImageOpaqueMemoryBindInfo
             image pBinds'

instance Zero SparseImageOpaqueMemoryBindInfo where
  zero = SparseImageOpaqueMemoryBindInfo
           zero
           mempty



-- No documentation found for TopLevel "VkSparseImageMemoryBindInfo"
data SparseImageMemoryBindInfo = SparseImageMemoryBindInfo
  { -- No documentation found for Nested "VkSparseImageMemoryBindInfo" "image"
    image :: Image
  , -- No documentation found for Nested "VkSparseImageMemoryBindInfo" "pBinds"
    binds :: Vector SparseImageMemoryBind
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseImageMemoryBindInfo)
#endif
deriving instance Show SparseImageMemoryBindInfo

instance ToCStruct SparseImageMemoryBindInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageMemoryBindInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Image)) (image)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (binds)) :: Word32))
    pPBinds' <- ContT $ allocaBytesAligned @SparseImageMemoryBind ((Data.Vector.length (binds)) * 64) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBinds' `plusPtr` (64 * (i)) :: Ptr SparseImageMemoryBind) (e)) (binds)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr SparseImageMemoryBind))) (pPBinds')
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Image)) (zero)
    pPBinds' <- ContT $ allocaBytesAligned @SparseImageMemoryBind ((Data.Vector.length (mempty)) * 64) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBinds' `plusPtr` (64 * (i)) :: Ptr SparseImageMemoryBind) (e)) (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr SparseImageMemoryBind))) (pPBinds')
    lift $ f

instance FromCStruct SparseImageMemoryBindInfo where
  peekCStruct p = do
    image <- peek @Image ((p `plusPtr` 0 :: Ptr Image))
    bindCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pBinds <- peek @(Ptr SparseImageMemoryBind) ((p `plusPtr` 16 :: Ptr (Ptr SparseImageMemoryBind)))
    pBinds' <- generateM (fromIntegral bindCount) (\i -> peekCStruct @SparseImageMemoryBind ((pBinds `advancePtrBytes` (64 * (i)) :: Ptr SparseImageMemoryBind)))
    pure $ SparseImageMemoryBindInfo
             image pBinds'

instance Zero SparseImageMemoryBindInfo where
  zero = SparseImageMemoryBindInfo
           zero
           mempty



-- No documentation found for TopLevel "VkBindSparseInfo"
data BindSparseInfo (es :: [Type]) = BindSparseInfo
  { -- No documentation found for Nested "VkBindSparseInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkBindSparseInfo" "pWaitSemaphores"
    waitSemaphores :: Vector Semaphore
  , -- No documentation found for Nested "VkBindSparseInfo" "pBufferBinds"
    bufferBinds :: Vector SparseBufferMemoryBindInfo
  , -- No documentation found for Nested "VkBindSparseInfo" "pImageOpaqueBinds"
    imageOpaqueBinds :: Vector SparseImageOpaqueMemoryBindInfo
  , -- No documentation found for Nested "VkBindSparseInfo" "pImageBinds"
    imageBinds :: Vector SparseImageMemoryBindInfo
  , -- No documentation found for Nested "VkBindSparseInfo" "pSignalSemaphores"
    signalSemaphores :: Vector Semaphore
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindSparseInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BindSparseInfo es)

instance Extensible BindSparseInfo where
  extensibleType = STRUCTURE_TYPE_BIND_SPARSE_INFO
  setNext x next = x{next = next}
  getNext BindSparseInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BindSparseInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @TimelineSemaphoreSubmitInfo = Just f
    | Just Refl <- eqT @e @DeviceGroupBindSparseInfo = Just f
    | otherwise = Nothing

instance (Extendss BindSparseInfo es, PokeChain es) => ToCStruct (BindSparseInfo es) where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindSparseInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_SPARSE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (waitSemaphores)) :: Word32))
    pPWaitSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (waitSemaphores)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (waitSemaphores)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Semaphore))) (pPWaitSemaphores')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (bufferBinds)) :: Word32))
    pPBufferBinds' <- ContT $ allocaBytesAligned @SparseBufferMemoryBindInfo ((Data.Vector.length (bufferBinds)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPBufferBinds' `plusPtr` (24 * (i)) :: Ptr SparseBufferMemoryBindInfo) (e) . ($ ())) (bufferBinds)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SparseBufferMemoryBindInfo))) (pPBufferBinds')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (imageOpaqueBinds)) :: Word32))
    pPImageOpaqueBinds' <- ContT $ allocaBytesAligned @SparseImageOpaqueMemoryBindInfo ((Data.Vector.length (imageOpaqueBinds)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPImageOpaqueBinds' `plusPtr` (24 * (i)) :: Ptr SparseImageOpaqueMemoryBindInfo) (e) . ($ ())) (imageOpaqueBinds)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr SparseImageOpaqueMemoryBindInfo))) (pPImageOpaqueBinds')
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (imageBinds)) :: Word32))
    pPImageBinds' <- ContT $ allocaBytesAligned @SparseImageMemoryBindInfo ((Data.Vector.length (imageBinds)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPImageBinds' `plusPtr` (24 * (i)) :: Ptr SparseImageMemoryBindInfo) (e) . ($ ())) (imageBinds)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr SparseImageMemoryBindInfo))) (pPImageBinds')
    lift $ poke ((p `plusPtr` 80 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (signalSemaphores)) :: Word32))
    pPSignalSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (signalSemaphores)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (signalSemaphores)
    lift $ poke ((p `plusPtr` 88 :: Ptr (Ptr Semaphore))) (pPSignalSemaphores')
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_SPARSE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    pPWaitSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Semaphore))) (pPWaitSemaphores')
    pPBufferBinds' <- ContT $ allocaBytesAligned @SparseBufferMemoryBindInfo ((Data.Vector.length (mempty)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPBufferBinds' `plusPtr` (24 * (i)) :: Ptr SparseBufferMemoryBindInfo) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SparseBufferMemoryBindInfo))) (pPBufferBinds')
    pPImageOpaqueBinds' <- ContT $ allocaBytesAligned @SparseImageOpaqueMemoryBindInfo ((Data.Vector.length (mempty)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPImageOpaqueBinds' `plusPtr` (24 * (i)) :: Ptr SparseImageOpaqueMemoryBindInfo) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr SparseImageOpaqueMemoryBindInfo))) (pPImageOpaqueBinds')
    pPImageBinds' <- ContT $ allocaBytesAligned @SparseImageMemoryBindInfo ((Data.Vector.length (mempty)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPImageBinds' `plusPtr` (24 * (i)) :: Ptr SparseImageMemoryBindInfo) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr SparseImageMemoryBindInfo))) (pPImageBinds')
    pPSignalSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (mempty)
    lift $ poke ((p `plusPtr` 88 :: Ptr (Ptr Semaphore))) (pPSignalSemaphores')
    lift $ f

instance (Extendss BindSparseInfo es, PeekChain es) => FromCStruct (BindSparseInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    waitSemaphoreCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pWaitSemaphores <- peek @(Ptr Semaphore) ((p `plusPtr` 24 :: Ptr (Ptr Semaphore)))
    pWaitSemaphores' <- generateM (fromIntegral waitSemaphoreCount) (\i -> peek @Semaphore ((pWaitSemaphores `advancePtrBytes` (8 * (i)) :: Ptr Semaphore)))
    bufferBindCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pBufferBinds <- peek @(Ptr SparseBufferMemoryBindInfo) ((p `plusPtr` 40 :: Ptr (Ptr SparseBufferMemoryBindInfo)))
    pBufferBinds' <- generateM (fromIntegral bufferBindCount) (\i -> peekCStruct @SparseBufferMemoryBindInfo ((pBufferBinds `advancePtrBytes` (24 * (i)) :: Ptr SparseBufferMemoryBindInfo)))
    imageOpaqueBindCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pImageOpaqueBinds <- peek @(Ptr SparseImageOpaqueMemoryBindInfo) ((p `plusPtr` 56 :: Ptr (Ptr SparseImageOpaqueMemoryBindInfo)))
    pImageOpaqueBinds' <- generateM (fromIntegral imageOpaqueBindCount) (\i -> peekCStruct @SparseImageOpaqueMemoryBindInfo ((pImageOpaqueBinds `advancePtrBytes` (24 * (i)) :: Ptr SparseImageOpaqueMemoryBindInfo)))
    imageBindCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pImageBinds <- peek @(Ptr SparseImageMemoryBindInfo) ((p `plusPtr` 72 :: Ptr (Ptr SparseImageMemoryBindInfo)))
    pImageBinds' <- generateM (fromIntegral imageBindCount) (\i -> peekCStruct @SparseImageMemoryBindInfo ((pImageBinds `advancePtrBytes` (24 * (i)) :: Ptr SparseImageMemoryBindInfo)))
    signalSemaphoreCount <- peek @Word32 ((p `plusPtr` 80 :: Ptr Word32))
    pSignalSemaphores <- peek @(Ptr Semaphore) ((p `plusPtr` 88 :: Ptr (Ptr Semaphore)))
    pSignalSemaphores' <- generateM (fromIntegral signalSemaphoreCount) (\i -> peek @Semaphore ((pSignalSemaphores `advancePtrBytes` (8 * (i)) :: Ptr Semaphore)))
    pure $ BindSparseInfo
             next pWaitSemaphores' pBufferBinds' pImageOpaqueBinds' pImageBinds' pSignalSemaphores'

instance es ~ '[] => Zero (BindSparseInfo es) where
  zero = BindSparseInfo
           ()
           mempty
           mempty
           mempty
           mempty
           mempty

