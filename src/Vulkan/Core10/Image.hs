{-# language CPP #-}
-- No documentation found for Chapter "Image"
module Vulkan.Core10.Image  ( createImage
                            , withImage
                            , destroyImage
                            , getImageSubresourceLayout
                            , ImageCreateInfo(..)
                            , SubresourceLayout(..)
                            , Image(..)
                            , ImageLayout(..)
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationImageCreateInfoNV)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateImage))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyImage))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageSubresourceLayout))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent3D)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ExternalFormatANDROID)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryImageCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory (ExternalMemoryImageCreateInfoNV)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierExplicitCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierListCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_image_format_list (ImageFormatListCreateInfo)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage (ImageStencilUsageCreateInfo)
import Vulkan.Core10.SparseResourceMemoryManagement (ImageSubresource)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (ImageSwapchainCreateInfoKHR)
import Vulkan.Core10.Enums.ImageTiling (ImageTiling)
import Vulkan.Core10.Enums.ImageType (ImageType)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SharingMode (SharingMode)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImage
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct ImageCreateInfo) -> Ptr AllocationCallbacks -> Ptr Image -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct ImageCreateInfo) -> Ptr AllocationCallbacks -> Ptr Image -> IO Result

-- No documentation found for TopLevel "vkCreateImage"
createImage :: forall a io
             . (Extendss ImageCreateInfo a, PokeChain a, MonadIO io)
            => -- No documentation found for Nested "vkCreateImage" "device"
               Device
            -> -- No documentation found for Nested "vkCreateImage" "pCreateInfo"
               (ImageCreateInfo a)
            -> -- No documentation found for Nested "vkCreateImage" "pAllocator"
               ("allocator" ::: Maybe AllocationCallbacks)
            -> io (Image)
createImage device createInfo allocator = liftIO . evalContT $ do
  let vkCreateImagePtr = pVkCreateImage (deviceCmds (device :: Device))
  lift $ unless (vkCreateImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateImage is null" Nothing Nothing
  let vkCreateImage' = mkVkCreateImage vkCreateImagePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPImage <- ContT $ bracket (callocBytes @Image 8) free
  r <- lift $ vkCreateImage' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPImage)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImage <- lift $ peek @Image pPImage
  pure $ (pImage)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createImage' and 'destroyImage'
--
-- To ensure that 'destroyImage' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withImage :: forall a io r . (Extendss ImageCreateInfo a, PokeChain a, MonadIO io) => Device -> ImageCreateInfo a -> Maybe AllocationCallbacks -> (io Image -> (Image -> io ()) -> r) -> r
withImage device pCreateInfo pAllocator b =
  b (createImage device pCreateInfo pAllocator)
    (\(o0) -> destroyImage device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyImage
  :: FunPtr (Ptr Device_T -> Image -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Image -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyImage"
destroyImage :: forall io
              . (MonadIO io)
             => -- No documentation found for Nested "vkDestroyImage" "device"
                Device
             -> -- No documentation found for Nested "vkDestroyImage" "image"
                Image
             -> -- No documentation found for Nested "vkDestroyImage" "pAllocator"
                ("allocator" ::: Maybe AllocationCallbacks)
             -> io ()
destroyImage device image allocator = liftIO . evalContT $ do
  let vkDestroyImagePtr = pVkDestroyImage (deviceCmds (device :: Device))
  lift $ unless (vkDestroyImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyImage is null" Nothing Nothing
  let vkDestroyImage' = mkVkDestroyImage vkDestroyImagePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyImage' (deviceHandle (device)) (image) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSubresourceLayout
  :: FunPtr (Ptr Device_T -> Image -> Ptr ImageSubresource -> Ptr SubresourceLayout -> IO ()) -> Ptr Device_T -> Image -> Ptr ImageSubresource -> Ptr SubresourceLayout -> IO ()

-- No documentation found for TopLevel "vkGetImageSubresourceLayout"
getImageSubresourceLayout :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkGetImageSubresourceLayout" "device"
                             Device
                          -> -- No documentation found for Nested "vkGetImageSubresourceLayout" "image"
                             Image
                          -> -- No documentation found for Nested "vkGetImageSubresourceLayout" "pSubresource"
                             ImageSubresource
                          -> io (SubresourceLayout)
getImageSubresourceLayout device image subresource = liftIO . evalContT $ do
  let vkGetImageSubresourceLayoutPtr = pVkGetImageSubresourceLayout (deviceCmds (device :: Device))
  lift $ unless (vkGetImageSubresourceLayoutPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageSubresourceLayout is null" Nothing Nothing
  let vkGetImageSubresourceLayout' = mkVkGetImageSubresourceLayout vkGetImageSubresourceLayoutPtr
  pSubresource <- ContT $ withCStruct (subresource)
  pPLayout <- ContT (withZeroCStruct @SubresourceLayout)
  lift $ vkGetImageSubresourceLayout' (deviceHandle (device)) (image) pSubresource (pPLayout)
  pLayout <- lift $ peekCStruct @SubresourceLayout pPLayout
  pure $ (pLayout)



-- No documentation found for TopLevel "VkImageCreateInfo"
data ImageCreateInfo (es :: [Type]) = ImageCreateInfo
  { -- No documentation found for Nested "VkImageCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkImageCreateInfo" "flags"
    flags :: ImageCreateFlags
  , -- No documentation found for Nested "VkImageCreateInfo" "imageType"
    imageType :: ImageType
  , -- No documentation found for Nested "VkImageCreateInfo" "format"
    format :: Format
  , -- No documentation found for Nested "VkImageCreateInfo" "extent"
    extent :: Extent3D
  , -- No documentation found for Nested "VkImageCreateInfo" "mipLevels"
    mipLevels :: Word32
  , -- No documentation found for Nested "VkImageCreateInfo" "arrayLayers"
    arrayLayers :: Word32
  , -- No documentation found for Nested "VkImageCreateInfo" "samples"
    samples :: SampleCountFlagBits
  , -- No documentation found for Nested "VkImageCreateInfo" "tiling"
    tiling :: ImageTiling
  , -- No documentation found for Nested "VkImageCreateInfo" "usage"
    usage :: ImageUsageFlags
  , -- No documentation found for Nested "VkImageCreateInfo" "sharingMode"
    sharingMode :: SharingMode
  , -- No documentation found for Nested "VkImageCreateInfo" "pQueueFamilyIndices"
    queueFamilyIndices :: Vector Word32
  , -- No documentation found for Nested "VkImageCreateInfo" "initialLayout"
    initialLayout :: ImageLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageCreateInfo es)

instance Extensible ImageCreateInfo where
  extensibleType = STRUCTURE_TYPE_IMAGE_CREATE_INFO
  setNext x next = x{next = next}
  getNext ImageCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImageStencilUsageCreateInfo = Just f
    | Just Refl <- eqT @e @ImageDrmFormatModifierExplicitCreateInfoEXT = Just f
    | Just Refl <- eqT @e @ImageDrmFormatModifierListCreateInfoEXT = Just f
    | Just Refl <- eqT @e @ExternalFormatANDROID = Just f
    | Just Refl <- eqT @e @ImageFormatListCreateInfo = Just f
    | Just Refl <- eqT @e @ImageSwapchainCreateInfoKHR = Just f
    | Just Refl <- eqT @e @ExternalMemoryImageCreateInfo = Just f
    | Just Refl <- eqT @e @ExternalMemoryImageCreateInfoNV = Just f
    | Just Refl <- eqT @e @DedicatedAllocationImageCreateInfoNV = Just f
    | otherwise = Nothing

instance (Extendss ImageCreateInfo es, PokeChain es) => ToCStruct (ImageCreateInfo es) where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageType)) (imageType)
    lift $ poke ((p `plusPtr` 24 :: Ptr Format)) (format)
    lift $ poke ((p `plusPtr` 28 :: Ptr Extent3D)) (extent)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (mipLevels)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (arrayLayers)
    lift $ poke ((p `plusPtr` 48 :: Ptr SampleCountFlagBits)) (samples)
    lift $ poke ((p `plusPtr` 52 :: Ptr ImageTiling)) (tiling)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageUsageFlags)) (usage)
    lift $ poke ((p `plusPtr` 60 :: Ptr SharingMode)) (sharingMode)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueFamilyIndices)) :: Word32))
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (queueFamilyIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (queueFamilyIndices)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ poke ((p `plusPtr` 80 :: Ptr ImageLayout)) (initialLayout)
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageType)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Extent3D)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr SampleCountFlagBits)) (zero)
    lift $ poke ((p `plusPtr` 52 :: Ptr ImageTiling)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageUsageFlags)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr SharingMode)) (zero)
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ poke ((p `plusPtr` 80 :: Ptr ImageLayout)) (zero)
    lift $ f

instance (Extendss ImageCreateInfo es, PeekChain es) => FromCStruct (ImageCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @ImageCreateFlags ((p `plusPtr` 16 :: Ptr ImageCreateFlags))
    imageType <- peek @ImageType ((p `plusPtr` 20 :: Ptr ImageType))
    format <- peek @Format ((p `plusPtr` 24 :: Ptr Format))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 28 :: Ptr Extent3D))
    mipLevels <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    arrayLayers <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    samples <- peek @SampleCountFlagBits ((p `plusPtr` 48 :: Ptr SampleCountFlagBits))
    tiling <- peek @ImageTiling ((p `plusPtr` 52 :: Ptr ImageTiling))
    usage <- peek @ImageUsageFlags ((p `plusPtr` 56 :: Ptr ImageUsageFlags))
    sharingMode <- peek @SharingMode ((p `plusPtr` 60 :: Ptr SharingMode))
    queueFamilyIndexCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pQueueFamilyIndices <- peek @(Ptr Word32) ((p `plusPtr` 72 :: Ptr (Ptr Word32)))
    pQueueFamilyIndices' <- generateM (fromIntegral queueFamilyIndexCount) (\i -> peek @Word32 ((pQueueFamilyIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    initialLayout <- peek @ImageLayout ((p `plusPtr` 80 :: Ptr ImageLayout))
    pure $ ImageCreateInfo
             next flags imageType format extent mipLevels arrayLayers samples tiling usage sharingMode pQueueFamilyIndices' initialLayout

instance es ~ '[] => Zero (ImageCreateInfo es) where
  zero = ImageCreateInfo
           ()
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
           mempty
           zero



-- No documentation found for TopLevel "VkSubresourceLayout"
data SubresourceLayout = SubresourceLayout
  { -- No documentation found for Nested "VkSubresourceLayout" "offset"
    offset :: DeviceSize
  , -- No documentation found for Nested "VkSubresourceLayout" "size"
    size :: DeviceSize
  , -- No documentation found for Nested "VkSubresourceLayout" "rowPitch"
    rowPitch :: DeviceSize
  , -- No documentation found for Nested "VkSubresourceLayout" "arrayPitch"
    arrayPitch :: DeviceSize
  , -- No documentation found for Nested "VkSubresourceLayout" "depthPitch"
    depthPitch :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubresourceLayout)
#endif
deriving instance Show SubresourceLayout

instance ToCStruct SubresourceLayout where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubresourceLayout{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (rowPitch)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (arrayPitch)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (depthPitch)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct SubresourceLayout where
  peekCStruct p = do
    offset <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    rowPitch <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    arrayPitch <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    depthPitch <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ SubresourceLayout
             offset size rowPitch arrayPitch depthPitch


instance Storable SubresourceLayout where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubresourceLayout where
  zero = SubresourceLayout
           zero
           zero
           zero
           zero
           zero

