{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NVX_image_view_handle  ( getImageViewHandleNVX
                                                            , getImageViewAddressNVX
                                                            , ImageViewHandleInfoNVX(..)
                                                            , ImageViewAddressPropertiesNVX(..)
                                                            , NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION
                                                            , pattern NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION
                                                            , NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
                                                            , pattern NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
                                                            ) where

import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.Core10.Enums.DescriptorType (DescriptorType)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Core10.BaseType (DeviceAddress)
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetImageViewAddressNVX))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetImageViewHandleNVX))
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (ImageView)
import Graphics.Vulkan.Core10.Handles (ImageView(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Handles (Sampler)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageViewHandleNVX
  :: FunPtr (Ptr Device_T -> Ptr ImageViewHandleInfoNVX -> IO Word32) -> Ptr Device_T -> Ptr ImageViewHandleInfoNVX -> IO Word32

-- | vkGetImageViewHandleNVX - Get the handle for an image view for a
-- specific descriptor type
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image view.
--
-- -   @pInfo@ describes the image view to query and type of handle.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device', 'ImageViewHandleInfoNVX'
getImageViewHandleNVX :: forall io . MonadIO io => Device -> ImageViewHandleInfoNVX -> io (Word32)
getImageViewHandleNVX device info = liftIO . evalContT $ do
  let vkGetImageViewHandleNVX' = mkVkGetImageViewHandleNVX (pVkGetImageViewHandleNVX (deviceCmds (device :: Device)))
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkGetImageViewHandleNVX' (deviceHandle (device)) pInfo
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageViewAddressNVX
  :: FunPtr (Ptr Device_T -> ImageView -> Ptr ImageViewAddressPropertiesNVX -> IO Result) -> Ptr Device_T -> ImageView -> Ptr ImageViewAddressPropertiesNVX -> IO Result

-- | vkGetImageViewAddressNVX - Get the device address of an image view
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image view.
--
-- -   @imageView@ is a handle to the image view.
--
-- -   @pProperties@ contains the device address and size when the call
--     returns.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.ImageView',
-- 'ImageViewAddressPropertiesNVX'
getImageViewAddressNVX :: forall io . MonadIO io => Device -> ImageView -> io (ImageViewAddressPropertiesNVX)
getImageViewAddressNVX device imageView = liftIO . evalContT $ do
  let vkGetImageViewAddressNVX' = mkVkGetImageViewAddressNVX (pVkGetImageViewAddressNVX (deviceCmds (device :: Device)))
  pPProperties <- ContT (withZeroCStruct @ImageViewAddressPropertiesNVX)
  r <- lift $ vkGetImageViewAddressNVX' (deviceHandle (device)) (imageView) (pPProperties)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pProperties <- lift $ peekCStruct @ImageViewAddressPropertiesNVX pPProperties
  pure $ (pProperties)


-- | VkImageViewHandleInfoNVX - Structure specifying the image view for
-- handle queries
--
-- == Valid Usage
--
-- -   @descriptorType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--
-- -   @sampler@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Sampler'
--     if @descriptorType@ is
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--
-- -   If descriptorType is
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     or
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     the image that @imageView@ was created from /must/ have been created
--     with the
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
--     usage bit set
--
-- -   If descriptorType is
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     the image that @imageView@ was created from /must/ have been created
--     with the
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT'
--     usage bit set
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @imageView@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.ImageView' handle
--
-- -   @descriptorType@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DescriptorType' value
--
-- -   If @sampler@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @sampler@ /must/
--     be a valid 'Graphics.Vulkan.Core10.Handles.Sampler' handle
--
-- -   Both of @imageView@, and @sampler@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DescriptorType',
-- 'Graphics.Vulkan.Core10.Handles.ImageView',
-- 'Graphics.Vulkan.Core10.Handles.Sampler',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageViewHandleNVX'
data ImageViewHandleInfoNVX = ImageViewHandleInfoNVX
  { -- | @imageView@ is the image view to query.
    imageView :: ImageView
  , -- | @descriptorType@ is the type of descriptor for which to query a handle.
    descriptorType :: DescriptorType
  , -- | @sampler@ is the sampler to combine with the image view when generating
    -- the handle.
    sampler :: Sampler
  }
  deriving (Typeable)
deriving instance Show ImageViewHandleInfoNVX

instance ToCStruct ImageViewHandleInfoNVX where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageViewHandleInfoNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageView)) (imageView)
    poke ((p `plusPtr` 24 :: Ptr DescriptorType)) (descriptorType)
    poke ((p `plusPtr` 32 :: Ptr Sampler)) (sampler)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageView)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DescriptorType)) (zero)
    f

instance FromCStruct ImageViewHandleInfoNVX where
  peekCStruct p = do
    imageView <- peek @ImageView ((p `plusPtr` 16 :: Ptr ImageView))
    descriptorType <- peek @DescriptorType ((p `plusPtr` 24 :: Ptr DescriptorType))
    sampler <- peek @Sampler ((p `plusPtr` 32 :: Ptr Sampler))
    pure $ ImageViewHandleInfoNVX
             imageView descriptorType sampler

instance Storable ImageViewHandleInfoNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageViewHandleInfoNVX where
  zero = ImageViewHandleInfoNVX
           zero
           zero
           zero


-- | VkImageViewAddressPropertiesNVX - Structure specifying the image view
-- for handle queries
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.DeviceAddress',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageViewAddressNVX'
data ImageViewAddressPropertiesNVX = ImageViewAddressPropertiesNVX
  { -- | @deviceAddress@ is the device address of the image view.
    deviceAddress :: DeviceAddress
  , -- | @size@ is the size in bytes of the image view device memory.
    size :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show ImageViewAddressPropertiesNVX

instance ToCStruct ImageViewAddressPropertiesNVX where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageViewAddressPropertiesNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceAddress)) (deviceAddress)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct ImageViewAddressPropertiesNVX where
  peekCStruct p = do
    deviceAddress <- peek @DeviceAddress ((p `plusPtr` 16 :: Ptr DeviceAddress))
    size <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pure $ ImageViewAddressPropertiesNVX
             deviceAddress size

instance Storable ImageViewAddressPropertiesNVX where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageViewAddressPropertiesNVX where
  zero = ImageViewAddressPropertiesNVX
           zero
           zero


type NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION"
pattern NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION :: forall a . Integral a => a
pattern NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION = 2


type NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME = "VK_NVX_image_view_handle"

-- No documentation found for TopLevel "VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME"
pattern NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME = "VK_NVX_image_view_handle"

