{-# language CPP #-}
-- | = Name
--
-- VK_NVX_image_view_handle - device extension
--
-- = VK_NVX_image_view_handle
--
-- [__Name String__]
--     @VK_NVX_image_view_handle@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     31
--
-- [__Revision__]
--     4
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Eric Werness
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NVX_image_view_handle] @ewerness-nv%0A*Here describe the issue or question you have about the VK_NVX_image_view_handle extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-12-03
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Liam Middlebrook, NVIDIA
--
--     -   Rodrigo Locatti, NVIDIA
--
-- == Description
--
-- This extension allows applications to query an opaque handle from an
-- image view for use as a sampled image or storage image. This provides no
-- direct functionality itself.
--
-- == New Commands
--
-- -   'getDeviceCombinedImageSamplerIndexNVX'
--
-- -   'getImageViewAddressNVX'
--
-- -   'getImageViewHandle64NVX'
--
-- -   'getImageViewHandleNVX'
--
-- == New Structures
--
-- -   'ImageViewAddressPropertiesNVX'
--
-- -   'ImageViewHandleInfoNVX'
--
-- == New Enum Constants
--
-- -   'NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME'
--
-- -   'NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX'
--
-- == Version History
--
-- -   Revision 4, 2025-12-03 (Rodrigo Locatti)
--
--     -   Add 'getDeviceCombinedImageSamplerIndexNVX'
--
-- -   Revision 3, 2024-11-04 (Liam Middlebrook)
--
--     -   Add 'getImageViewHandle64NVX'
--
-- -   Revision 2, 2020-04-03 (Piers Daniell)
--
--     -   Add 'getImageViewAddressNVX'
--
-- -   Revision 1, 2018-12-07 (Eric Werness)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NVX_image_view_handle Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NVX_image_view_handle  ( getImageViewHandleNVX
                                                   , getImageViewHandle64NVX
                                                   , getImageViewAddressNVX
                                                   , getDeviceCombinedImageSamplerIndexNVX
                                                   , ImageViewHandleInfoNVX(..)
                                                   , ImageViewAddressPropertiesNVX(..)
                                                   , NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION
                                                   , pattern NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION
                                                   , NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
                                                   , pattern NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
                                                   ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
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
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceCombinedImageSamplerIndexNVX))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageViewAddressNVX))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageViewHandle64NVX))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageViewHandleNVX))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Handles (ImageView)
import Vulkan.Core10.Handles (ImageView(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Sampler)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageViewHandleNVX
  :: FunPtr (Ptr Device_T -> Ptr ImageViewHandleInfoNVX -> IO Word32) -> Ptr Device_T -> Ptr ImageViewHandleInfoNVX -> IO Word32

-- | vkGetImageViewHandleNVX - Get the handle for an image view for a
-- specific descriptor type
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_image_view_handle VK_NVX_image_view_handle>,
-- 'Vulkan.Core10.Handles.Device', 'ImageViewHandleInfoNVX'
getImageViewHandleNVX :: forall io
                       . (MonadIO io)
                      => -- | @device@ is the logical device that owns the image view.
                         --
                         -- #VUID-vkGetImageViewHandleNVX-device-parameter# @device@ /must/ be a
                         -- valid 'Vulkan.Core10.Handles.Device' handle
                         Device
                      -> -- | @pInfo@ describes the image view to query and type of handle.
                         --
                         -- #VUID-vkGetImageViewHandleNVX-pInfo-parameter# @pInfo@ /must/ be a valid
                         -- pointer to a valid 'ImageViewHandleInfoNVX' structure
                         ImageViewHandleInfoNVX
                      -> io (Word32)
getImageViewHandleNVX device info = liftIO . evalContT $ do
  let vkGetImageViewHandleNVXPtr = pVkGetImageViewHandleNVX (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetImageViewHandleNVXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageViewHandleNVX is null" Nothing Nothing
  let vkGetImageViewHandleNVX' = mkVkGetImageViewHandleNVX vkGetImageViewHandleNVXPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetImageViewHandleNVX" (vkGetImageViewHandleNVX'
                                                            (deviceHandle (device))
                                                            pInfo)
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageViewHandle64NVX
  :: FunPtr (Ptr Device_T -> Ptr ImageViewHandleInfoNVX -> IO Word64) -> Ptr Device_T -> Ptr ImageViewHandleInfoNVX -> IO Word64

-- | vkGetImageViewHandle64NVX - Get the 64-bit handle for an image view for
-- a specific descriptor type
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_image_view_handle VK_NVX_image_view_handle>,
-- 'Vulkan.Core10.Handles.Device', 'ImageViewHandleInfoNVX'
getImageViewHandle64NVX :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the logical device that owns the image view.
                           --
                           -- #VUID-vkGetImageViewHandle64NVX-device-parameter# @device@ /must/ be a
                           -- valid 'Vulkan.Core10.Handles.Device' handle
                           Device
                        -> -- | @pInfo@ describes the image view to query and type of handle.
                           --
                           -- #VUID-vkGetImageViewHandle64NVX-pInfo-parameter# @pInfo@ /must/ be a
                           -- valid pointer to a valid 'ImageViewHandleInfoNVX' structure
                           ImageViewHandleInfoNVX
                        -> io (Word64)
getImageViewHandle64NVX device info = liftIO . evalContT $ do
  let vkGetImageViewHandle64NVXPtr = pVkGetImageViewHandle64NVX (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetImageViewHandle64NVXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageViewHandle64NVX is null" Nothing Nothing
  let vkGetImageViewHandle64NVX' = mkVkGetImageViewHandle64NVX vkGetImageViewHandle64NVXPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetImageViewHandle64NVX" (vkGetImageViewHandle64NVX'
                                                              (deviceHandle (device))
                                                              pInfo)
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageViewAddressNVX
  :: FunPtr (Ptr Device_T -> ImageView -> Ptr ImageViewAddressPropertiesNVX -> IO Result) -> Ptr Device_T -> ImageView -> Ptr ImageViewAddressPropertiesNVX -> IO Result

-- | vkGetImageViewAddressNVX - Get the device address of an image view
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_image_view_handle VK_NVX_image_view_handle>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.ImageView',
-- 'ImageViewAddressPropertiesNVX'
getImageViewAddressNVX :: forall io
                        . (MonadIO io)
                       => -- | @device@ is the logical device that owns the image view.
                          --
                          -- #VUID-vkGetImageViewAddressNVX-device-parameter# @device@ /must/ be a
                          -- valid 'Vulkan.Core10.Handles.Device' handle
                          Device
                       -> -- | @imageView@ is a handle to the image view.
                          --
                          -- #VUID-vkGetImageViewAddressNVX-imageView-parameter# @imageView@ /must/
                          -- be a valid 'Vulkan.Core10.Handles.ImageView' handle
                          --
                          -- #VUID-vkGetImageViewAddressNVX-imageView-parent# @imageView@ /must/ have
                          -- been created, allocated, or retrieved from @device@
                          ImageView
                       -> io (ImageViewAddressPropertiesNVX)
getImageViewAddressNVX device imageView = liftIO . evalContT $ do
  let vkGetImageViewAddressNVXPtr = pVkGetImageViewAddressNVX (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetImageViewAddressNVXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageViewAddressNVX is null" Nothing Nothing
  let vkGetImageViewAddressNVX' = mkVkGetImageViewAddressNVX vkGetImageViewAddressNVXPtr
  pPProperties <- ContT (withZeroCStruct @ImageViewAddressPropertiesNVX)
  r <- lift $ traceAroundEvent "vkGetImageViewAddressNVX" (vkGetImageViewAddressNVX'
                                                             (deviceHandle (device))
                                                             (imageView)
                                                             (pPProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pProperties <- lift $ peekCStruct @ImageViewAddressPropertiesNVX pPProperties
  pure $ (pProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceCombinedImageSamplerIndexNVX
  :: FunPtr (Ptr Device_T -> Word64 -> Word64 -> IO Word64) -> Ptr Device_T -> Word64 -> Word64 -> IO Word64

-- | vkGetDeviceCombinedImageSamplerIndexNVX - Get the handle for an image
-- view and sampler index
--
-- = Description
--
-- Shaders take @imageViewIndex@ and @samplerIndex@, and multiply it by
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.PhysicalDeviceDescriptorHeapPropertiesEXT'::@imageDescriptorSize@
-- and
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.PhysicalDeviceDescriptorHeapPropertiesEXT'::@samplerDescriptorSize@
-- respectively to obtain the descriptor offset in bytes.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_image_view_handle VK_NVX_image_view_handle>,
-- 'Vulkan.Core10.Handles.Device'
getDeviceCombinedImageSamplerIndexNVX :: forall io
                                       . (MonadIO io)
                                      => -- | @device@ is the logical device that will use the result handle.
                                         --
                                         -- #VUID-vkGetDeviceCombinedImageSamplerIndexNVX-device-parameter# @device@
                                         -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                         Device
                                      -> -- | @imageViewIndex@ is the index within the resource heap.
                                         ("imageViewIndex" ::: Word64)
                                      -> -- | @samplerIndex@ is the index within the sampler heap.
                                         ("samplerIndex" ::: Word64)
                                      -> io (Word64)
getDeviceCombinedImageSamplerIndexNVX device
                                        imageViewIndex
                                        samplerIndex = liftIO $ do
  let vkGetDeviceCombinedImageSamplerIndexNVXPtr = pVkGetDeviceCombinedImageSamplerIndexNVX (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkGetDeviceCombinedImageSamplerIndexNVXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceCombinedImageSamplerIndexNVX is null" Nothing Nothing
  let vkGetDeviceCombinedImageSamplerIndexNVX' = mkVkGetDeviceCombinedImageSamplerIndexNVX vkGetDeviceCombinedImageSamplerIndexNVXPtr
  r <- traceAroundEvent "vkGetDeviceCombinedImageSamplerIndexNVX" (vkGetDeviceCombinedImageSamplerIndexNVX'
                                                                     (deviceHandle (device))
                                                                     (imageViewIndex)
                                                                     (samplerIndex))
  pure $ (r)


-- | VkImageViewHandleInfoNVX - Structure specifying the image view for
-- handle queries
--
-- == Valid Usage
--
-- -   #VUID-VkImageViewHandleInfoNVX-descriptorType-02654#
--     @descriptorType@ /must/ be
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--
-- -   #VUID-VkImageViewHandleInfoNVX-sampler-02655# @sampler@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Sampler' if @descriptorType@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--
-- -   #VUID-VkImageViewHandleInfoNVX-imageView-02656# If descriptorType is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     the image that @imageView@ was created from /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
--     usage flag set
--
-- -   #VUID-VkImageViewHandleInfoNVX-imageView-02657# If descriptorType is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     the image that @imageView@ was created from /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT'
--     usage flag set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageViewHandleInfoNVX-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX'
--
-- -   #VUID-VkImageViewHandleInfoNVX-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkImageViewHandleInfoNVX-imageView-parameter# @imageView@
--     /must/ be a valid 'Vulkan.Core10.Handles.ImageView' handle
--
-- -   #VUID-VkImageViewHandleInfoNVX-descriptorType-parameter#
--     @descriptorType@ /must/ be a valid
--     'Vulkan.Core10.Enums.DescriptorType.DescriptorType' value
--
-- -   #VUID-VkImageViewHandleInfoNVX-sampler-parameter# If @sampler@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @sampler@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Sampler' handle
--
-- -   #VUID-VkImageViewHandleInfoNVX-commonparent# Both of @imageView@,
--     and @sampler@ that are valid handles of non-ignored parameters
--     /must/ have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_image_view_handle VK_NVX_image_view_handle>,
-- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType',
-- 'Vulkan.Core10.Handles.ImageView', 'Vulkan.Core10.Handles.Sampler',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageViewHandle64NVX', 'getImageViewHandleNVX'
data ImageViewHandleInfoNVX = ImageViewHandleInfoNVX
  { -- | @imageView@ is the image view to query.
    imageView :: ImageView
  , -- | @descriptorType@ is the type of descriptor for which to query a handle.
    descriptorType :: DescriptorType
  , -- | @sampler@ is the sampler to combine with the image view when generating
    -- the handle.
    sampler :: Sampler
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewHandleInfoNVX)
#endif
deriving instance Show ImageViewHandleInfoNVX

instance ToCStruct ImageViewHandleInfoNVX where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_image_view_handle VK_NVX_image_view_handle>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageViewAddressNVX'
data ImageViewAddressPropertiesNVX = ImageViewAddressPropertiesNVX
  { -- | @deviceAddress@ is the device address of the image view.
    deviceAddress :: DeviceAddress
  , -- | @size@ is the size in bytes of the image view device memory.
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewAddressPropertiesNVX)
#endif
deriving instance Show ImageViewAddressPropertiesNVX

instance ToCStruct ImageViewAddressPropertiesNVX where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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


type NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION"
pattern NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION :: forall a . Integral a => a
pattern NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION = 4


type NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME = "VK_NVX_image_view_handle"

-- No documentation found for TopLevel "VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME"
pattern NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME = "VK_NVX_image_view_handle"

