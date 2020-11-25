{-# language CPP #-}
-- No documentation found for Chapter "ImageView"
module Vulkan.Core10.ImageView  ( createImageView
                                , withImageView
                                , destroyImageView
                                , ComponentMapping(..)
                                , ImageSubresourceRange(..)
                                , ImageViewCreateInfo(..)
                                , ImageView(..)
                                , ImageViewType(..)
                                , ComponentSwizzle(..)
                                , ImageViewCreateFlagBits(..)
                                , ImageViewCreateFlags
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
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Enums.ComponentSwizzle (ComponentSwizzle)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateImageView))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyImageView))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Handles (ImageView)
import Vulkan.Core10.Handles (ImageView(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_astc_decode_mode (ImageViewASTCDecodeModeEXT)
import Vulkan.Core10.Enums.ImageViewCreateFlagBits (ImageViewCreateFlags)
import Vulkan.Core10.Enums.ImageViewType (ImageViewType)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (ImageViewUsageCreateInfo)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionInfo)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.ComponentSwizzle (ComponentSwizzle(..))
import Vulkan.Core10.Handles (ImageView(..))
import Vulkan.Core10.Enums.ImageViewCreateFlagBits (ImageViewCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageViewCreateFlagBits (ImageViewCreateFlags)
import Vulkan.Core10.Enums.ImageViewType (ImageViewType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImageView
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct ImageViewCreateInfo) -> Ptr AllocationCallbacks -> Ptr ImageView -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct ImageViewCreateInfo) -> Ptr AllocationCallbacks -> Ptr ImageView -> IO Result

-- No documentation found for TopLevel "vkCreateImageView"
createImageView :: forall a io
                 . (Extendss ImageViewCreateInfo a, PokeChain a, MonadIO io)
                => -- No documentation found for Nested "vkCreateImageView" "device"
                   Device
                -> -- No documentation found for Nested "vkCreateImageView" "pCreateInfo"
                   (ImageViewCreateInfo a)
                -> -- No documentation found for Nested "vkCreateImageView" "pAllocator"
                   ("allocator" ::: Maybe AllocationCallbacks)
                -> io (ImageView)
createImageView device createInfo allocator = liftIO . evalContT $ do
  let vkCreateImageViewPtr = pVkCreateImageView (deviceCmds (device :: Device))
  lift $ unless (vkCreateImageViewPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateImageView is null" Nothing Nothing
  let vkCreateImageView' = mkVkCreateImageView vkCreateImageViewPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPView <- ContT $ bracket (callocBytes @ImageView 8) free
  r <- lift $ vkCreateImageView' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPView)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pView <- lift $ peek @ImageView pPView
  pure $ (pView)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createImageView' and 'destroyImageView'
--
-- To ensure that 'destroyImageView' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withImageView :: forall a io r . (Extendss ImageViewCreateInfo a, PokeChain a, MonadIO io) => Device -> ImageViewCreateInfo a -> Maybe AllocationCallbacks -> (io ImageView -> (ImageView -> io ()) -> r) -> r
withImageView device pCreateInfo pAllocator b =
  b (createImageView device pCreateInfo pAllocator)
    (\(o0) -> destroyImageView device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyImageView
  :: FunPtr (Ptr Device_T -> ImageView -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> ImageView -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyImageView"
destroyImageView :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkDestroyImageView" "device"
                    Device
                 -> -- No documentation found for Nested "vkDestroyImageView" "imageView"
                    ImageView
                 -> -- No documentation found for Nested "vkDestroyImageView" "pAllocator"
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io ()
destroyImageView device imageView allocator = liftIO . evalContT $ do
  let vkDestroyImageViewPtr = pVkDestroyImageView (deviceCmds (device :: Device))
  lift $ unless (vkDestroyImageViewPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyImageView is null" Nothing Nothing
  let vkDestroyImageView' = mkVkDestroyImageView vkDestroyImageViewPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyImageView' (deviceHandle (device)) (imageView) pAllocator
  pure $ ()



-- No documentation found for TopLevel "VkComponentMapping"
data ComponentMapping = ComponentMapping
  { -- No documentation found for Nested "VkComponentMapping" "r"
    r :: ComponentSwizzle
  , -- No documentation found for Nested "VkComponentMapping" "g"
    g :: ComponentSwizzle
  , -- No documentation found for Nested "VkComponentMapping" "b"
    b :: ComponentSwizzle
  , -- No documentation found for Nested "VkComponentMapping" "a"
    a :: ComponentSwizzle
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ComponentMapping)
#endif
deriving instance Show ComponentMapping

instance ToCStruct ComponentMapping where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ComponentMapping{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ComponentSwizzle)) (r)
    poke ((p `plusPtr` 4 :: Ptr ComponentSwizzle)) (g)
    poke ((p `plusPtr` 8 :: Ptr ComponentSwizzle)) (b)
    poke ((p `plusPtr` 12 :: Ptr ComponentSwizzle)) (a)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ComponentSwizzle)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ComponentSwizzle)) (zero)
    poke ((p `plusPtr` 8 :: Ptr ComponentSwizzle)) (zero)
    poke ((p `plusPtr` 12 :: Ptr ComponentSwizzle)) (zero)
    f

instance FromCStruct ComponentMapping where
  peekCStruct p = do
    r <- peek @ComponentSwizzle ((p `plusPtr` 0 :: Ptr ComponentSwizzle))
    g <- peek @ComponentSwizzle ((p `plusPtr` 4 :: Ptr ComponentSwizzle))
    b <- peek @ComponentSwizzle ((p `plusPtr` 8 :: Ptr ComponentSwizzle))
    a <- peek @ComponentSwizzle ((p `plusPtr` 12 :: Ptr ComponentSwizzle))
    pure $ ComponentMapping
             r g b a


instance Storable ComponentMapping where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ComponentMapping where
  zero = ComponentMapping
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkImageSubresourceRange"
data ImageSubresourceRange = ImageSubresourceRange
  { -- No documentation found for Nested "VkImageSubresourceRange" "aspectMask"
    aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "VkImageSubresourceRange" "baseMipLevel"
    baseMipLevel :: Word32
  , -- No documentation found for Nested "VkImageSubresourceRange" "levelCount"
    levelCount :: Word32
  , -- No documentation found for Nested "VkImageSubresourceRange" "baseArrayLayer"
    baseArrayLayer :: Word32
  , -- No documentation found for Nested "VkImageSubresourceRange" "layerCount"
    layerCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageSubresourceRange)
#endif
deriving instance Show ImageSubresourceRange

instance ToCStruct ImageSubresourceRange where
  withCStruct x f = allocaBytesAligned 20 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSubresourceRange{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (aspectMask)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (baseMipLevel)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (levelCount)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (baseArrayLayer)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (layerCount)
    f
  cStructSize = 20
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct ImageSubresourceRange where
  peekCStruct p = do
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 0 :: Ptr ImageAspectFlags))
    baseMipLevel <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    levelCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    baseArrayLayer <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    layerCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ ImageSubresourceRange
             aspectMask baseMipLevel levelCount baseArrayLayer layerCount


instance Storable ImageSubresourceRange where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSubresourceRange where
  zero = ImageSubresourceRange
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkImageViewCreateInfo"
data ImageViewCreateInfo (es :: [Type]) = ImageViewCreateInfo
  { -- No documentation found for Nested "VkImageViewCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkImageViewCreateInfo" "flags"
    flags :: ImageViewCreateFlags
  , -- No documentation found for Nested "VkImageViewCreateInfo" "image"
    image :: Image
  , -- No documentation found for Nested "VkImageViewCreateInfo" "viewType"
    viewType :: ImageViewType
  , -- No documentation found for Nested "VkImageViewCreateInfo" "format"
    format :: Format
  , -- No documentation found for Nested "VkImageViewCreateInfo" "components"
    components :: ComponentMapping
  , -- No documentation found for Nested "VkImageViewCreateInfo" "subresourceRange"
    subresourceRange :: ImageSubresourceRange
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageViewCreateInfo es)

instance Extensible ImageViewCreateInfo where
  extensibleType = STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
  setNext x next = x{next = next}
  getNext ImageViewCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageViewCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImageViewASTCDecodeModeEXT = Just f
    | Just Refl <- eqT @e @SamplerYcbcrConversionInfo = Just f
    | Just Refl <- eqT @e @ImageViewUsageCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss ImageViewCreateInfo es, PokeChain es) => ToCStruct (ImageViewCreateInfo es) where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageViewCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageViewCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (image)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageViewType)) (viewType)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (format)
    lift $ poke ((p `plusPtr` 40 :: Ptr ComponentMapping)) (components)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageSubresourceRange)) (subresourceRange)
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageViewType)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr ComponentMapping)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageSubresourceRange)) (zero)
    lift $ f

instance (Extendss ImageViewCreateInfo es, PeekChain es) => FromCStruct (ImageViewCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @ImageViewCreateFlags ((p `plusPtr` 16 :: Ptr ImageViewCreateFlags))
    image <- peek @Image ((p `plusPtr` 24 :: Ptr Image))
    viewType <- peek @ImageViewType ((p `plusPtr` 32 :: Ptr ImageViewType))
    format <- peek @Format ((p `plusPtr` 36 :: Ptr Format))
    components <- peekCStruct @ComponentMapping ((p `plusPtr` 40 :: Ptr ComponentMapping))
    subresourceRange <- peekCStruct @ImageSubresourceRange ((p `plusPtr` 56 :: Ptr ImageSubresourceRange))
    pure $ ImageViewCreateInfo
             next flags image viewType format components subresourceRange

instance es ~ '[] => Zero (ImageViewCreateInfo es) where
  zero = ImageViewCreateInfo
           ()
           zero
           zero
           zero
           zero
           zero
           zero

