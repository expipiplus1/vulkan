{-# language CPP #-}
-- No documentation found for Chapter "VK_NVX_image_view_handle"
module Vulkan.Extensions.VK_NVX_image_view_handle  ( getImageViewHandleNVX
                                                   , getImageViewAddressNVX
                                                   , ImageViewHandleInfoNVX(..)
                                                   , ImageViewAddressPropertiesNVX(..)
                                                   , NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION
                                                   , pattern NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION
                                                   , NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
                                                   , pattern NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
                                                   ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
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
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkGetImageViewAddressNVX))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageViewHandleNVX))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (ImageView)
import Vulkan.Core10.Handles (ImageView(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Sampler)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageViewHandleNVX
  :: FunPtr (Ptr Device_T -> Ptr ImageViewHandleInfoNVX -> IO Word32) -> Ptr Device_T -> Ptr ImageViewHandleInfoNVX -> IO Word32

-- No documentation found for TopLevel "vkGetImageViewHandleNVX"
getImageViewHandleNVX :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vkGetImageViewHandleNVX" "device"
                         Device
                      -> -- No documentation found for Nested "vkGetImageViewHandleNVX" "pInfo"
                         ImageViewHandleInfoNVX
                      -> io (Word32)
getImageViewHandleNVX device info = liftIO . evalContT $ do
  let vkGetImageViewHandleNVXPtr = pVkGetImageViewHandleNVX (deviceCmds (device :: Device))
  lift $ unless (vkGetImageViewHandleNVXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageViewHandleNVX is null" Nothing Nothing
  let vkGetImageViewHandleNVX' = mkVkGetImageViewHandleNVX vkGetImageViewHandleNVXPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkGetImageViewHandleNVX' (deviceHandle (device)) pInfo
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageViewAddressNVX
  :: FunPtr (Ptr Device_T -> ImageView -> Ptr ImageViewAddressPropertiesNVX -> IO Result) -> Ptr Device_T -> ImageView -> Ptr ImageViewAddressPropertiesNVX -> IO Result

-- No documentation found for TopLevel "vkGetImageViewAddressNVX"
getImageViewAddressNVX :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkGetImageViewAddressNVX" "device"
                          Device
                       -> -- No documentation found for Nested "vkGetImageViewAddressNVX" "imageView"
                          ImageView
                       -> io (ImageViewAddressPropertiesNVX)
getImageViewAddressNVX device imageView = liftIO . evalContT $ do
  let vkGetImageViewAddressNVXPtr = pVkGetImageViewAddressNVX (deviceCmds (device :: Device))
  lift $ unless (vkGetImageViewAddressNVXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageViewAddressNVX is null" Nothing Nothing
  let vkGetImageViewAddressNVX' = mkVkGetImageViewAddressNVX vkGetImageViewAddressNVXPtr
  pPProperties <- ContT (withZeroCStruct @ImageViewAddressPropertiesNVX)
  r <- lift $ vkGetImageViewAddressNVX' (deviceHandle (device)) (imageView) (pPProperties)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pProperties <- lift $ peekCStruct @ImageViewAddressPropertiesNVX pPProperties
  pure $ (pProperties)



-- No documentation found for TopLevel "VkImageViewHandleInfoNVX"
data ImageViewHandleInfoNVX = ImageViewHandleInfoNVX
  { -- No documentation found for Nested "VkImageViewHandleInfoNVX" "imageView"
    imageView :: ImageView
  , -- No documentation found for Nested "VkImageViewHandleInfoNVX" "descriptorType"
    descriptorType :: DescriptorType
  , -- No documentation found for Nested "VkImageViewHandleInfoNVX" "sampler"
    sampler :: Sampler
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewHandleInfoNVX)
#endif
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



-- No documentation found for TopLevel "VkImageViewAddressPropertiesNVX"
data ImageViewAddressPropertiesNVX = ImageViewAddressPropertiesNVX
  { -- No documentation found for Nested "VkImageViewAddressPropertiesNVX" "deviceAddress"
    deviceAddress :: DeviceAddress
  , -- No documentation found for Nested "VkImageViewAddressPropertiesNVX" "size"
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewAddressPropertiesNVX)
#endif
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

