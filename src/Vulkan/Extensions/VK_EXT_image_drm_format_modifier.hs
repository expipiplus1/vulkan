{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_image_drm_format_modifier"
module Vulkan.Extensions.VK_EXT_image_drm_format_modifier  ( getImageDrmFormatModifierPropertiesEXT
                                                           , DrmFormatModifierPropertiesListEXT(..)
                                                           , DrmFormatModifierPropertiesEXT(..)
                                                           , PhysicalDeviceImageDrmFormatModifierInfoEXT(..)
                                                           , ImageDrmFormatModifierListCreateInfoEXT(..)
                                                           , ImageDrmFormatModifierExplicitCreateInfoEXT(..)
                                                           , ImageDrmFormatModifierPropertiesEXT(..)
                                                           , EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION
                                                           , pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION
                                                           , EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME
                                                           , pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME
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
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageDrmFormatModifierPropertiesEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SharingMode (SharingMode)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Image (SubresourceLayout)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageDrmFormatModifierPropertiesEXT
  :: FunPtr (Ptr Device_T -> Image -> Ptr ImageDrmFormatModifierPropertiesEXT -> IO Result) -> Ptr Device_T -> Image -> Ptr ImageDrmFormatModifierPropertiesEXT -> IO Result

-- No documentation found for TopLevel "vkGetImageDrmFormatModifierPropertiesEXT"
getImageDrmFormatModifierPropertiesEXT :: forall io
                                        . (MonadIO io)
                                       => -- No documentation found for Nested "vkGetImageDrmFormatModifierPropertiesEXT" "device"
                                          Device
                                       -> -- No documentation found for Nested "vkGetImageDrmFormatModifierPropertiesEXT" "image"
                                          Image
                                       -> io (ImageDrmFormatModifierPropertiesEXT)
getImageDrmFormatModifierPropertiesEXT device image = liftIO . evalContT $ do
  let vkGetImageDrmFormatModifierPropertiesEXTPtr = pVkGetImageDrmFormatModifierPropertiesEXT (deviceCmds (device :: Device))
  lift $ unless (vkGetImageDrmFormatModifierPropertiesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageDrmFormatModifierPropertiesEXT is null" Nothing Nothing
  let vkGetImageDrmFormatModifierPropertiesEXT' = mkVkGetImageDrmFormatModifierPropertiesEXT vkGetImageDrmFormatModifierPropertiesEXTPtr
  pPProperties <- ContT (withZeroCStruct @ImageDrmFormatModifierPropertiesEXT)
  r <- lift $ vkGetImageDrmFormatModifierPropertiesEXT' (deviceHandle (device)) (image) (pPProperties)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pProperties <- lift $ peekCStruct @ImageDrmFormatModifierPropertiesEXT pPProperties
  pure $ (pProperties)



-- No documentation found for TopLevel "VkDrmFormatModifierPropertiesListEXT"
data DrmFormatModifierPropertiesListEXT = DrmFormatModifierPropertiesListEXT
  { -- No documentation found for Nested "VkDrmFormatModifierPropertiesListEXT" "drmFormatModifierCount"
    drmFormatModifierCount :: Word32
  , -- No documentation found for Nested "VkDrmFormatModifierPropertiesListEXT" "pDrmFormatModifierProperties"
    drmFormatModifierProperties :: Ptr DrmFormatModifierPropertiesEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrmFormatModifierPropertiesListEXT)
#endif
deriving instance Show DrmFormatModifierPropertiesListEXT

instance ToCStruct DrmFormatModifierPropertiesListEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrmFormatModifierPropertiesListEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (drmFormatModifierCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr DrmFormatModifierPropertiesEXT))) (drmFormatModifierProperties)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DrmFormatModifierPropertiesListEXT where
  peekCStruct p = do
    drmFormatModifierCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pDrmFormatModifierProperties <- peek @(Ptr DrmFormatModifierPropertiesEXT) ((p `plusPtr` 24 :: Ptr (Ptr DrmFormatModifierPropertiesEXT)))
    pure $ DrmFormatModifierPropertiesListEXT
             drmFormatModifierCount pDrmFormatModifierProperties


instance Storable DrmFormatModifierPropertiesListEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrmFormatModifierPropertiesListEXT where
  zero = DrmFormatModifierPropertiesListEXT
           zero
           zero



-- No documentation found for TopLevel "VkDrmFormatModifierPropertiesEXT"
data DrmFormatModifierPropertiesEXT = DrmFormatModifierPropertiesEXT
  { -- No documentation found for Nested "VkDrmFormatModifierPropertiesEXT" "drmFormatModifier"
    drmFormatModifier :: Word64
  , -- No documentation found for Nested "VkDrmFormatModifierPropertiesEXT" "drmFormatModifierPlaneCount"
    drmFormatModifierPlaneCount :: Word32
  , -- No documentation found for Nested "VkDrmFormatModifierPropertiesEXT" "drmFormatModifierTilingFeatures"
    drmFormatModifierTilingFeatures :: FormatFeatureFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrmFormatModifierPropertiesEXT)
#endif
deriving instance Show DrmFormatModifierPropertiesEXT

instance ToCStruct DrmFormatModifierPropertiesEXT where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrmFormatModifierPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word64)) (drmFormatModifier)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (drmFormatModifierPlaneCount)
    poke ((p `plusPtr` 12 :: Ptr FormatFeatureFlags)) (drmFormatModifierTilingFeatures)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr FormatFeatureFlags)) (zero)
    f

instance FromCStruct DrmFormatModifierPropertiesEXT where
  peekCStruct p = do
    drmFormatModifier <- peek @Word64 ((p `plusPtr` 0 :: Ptr Word64))
    drmFormatModifierPlaneCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    drmFormatModifierTilingFeatures <- peek @FormatFeatureFlags ((p `plusPtr` 12 :: Ptr FormatFeatureFlags))
    pure $ DrmFormatModifierPropertiesEXT
             drmFormatModifier drmFormatModifierPlaneCount drmFormatModifierTilingFeatures


instance Storable DrmFormatModifierPropertiesEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrmFormatModifierPropertiesEXT where
  zero = DrmFormatModifierPropertiesEXT
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceImageDrmFormatModifierInfoEXT"
data PhysicalDeviceImageDrmFormatModifierInfoEXT = PhysicalDeviceImageDrmFormatModifierInfoEXT
  { -- No documentation found for Nested "VkPhysicalDeviceImageDrmFormatModifierInfoEXT" "drmFormatModifier"
    drmFormatModifier :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceImageDrmFormatModifierInfoEXT" "sharingMode"
    sharingMode :: SharingMode
  , -- No documentation found for Nested "VkPhysicalDeviceImageDrmFormatModifierInfoEXT" "pQueueFamilyIndices"
    queueFamilyIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageDrmFormatModifierInfoEXT)
#endif
deriving instance Show PhysicalDeviceImageDrmFormatModifierInfoEXT

instance ToCStruct PhysicalDeviceImageDrmFormatModifierInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageDrmFormatModifierInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word64)) (drmFormatModifier)
    lift $ poke ((p `plusPtr` 24 :: Ptr SharingMode)) (sharingMode)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueFamilyIndices)) :: Word32))
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (queueFamilyIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (queueFamilyIndices)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr SharingMode)) (zero)
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ f

instance FromCStruct PhysicalDeviceImageDrmFormatModifierInfoEXT where
  peekCStruct p = do
    drmFormatModifier <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    sharingMode <- peek @SharingMode ((p `plusPtr` 24 :: Ptr SharingMode))
    queueFamilyIndexCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pQueueFamilyIndices <- peek @(Ptr Word32) ((p `plusPtr` 32 :: Ptr (Ptr Word32)))
    pQueueFamilyIndices' <- generateM (fromIntegral queueFamilyIndexCount) (\i -> peek @Word32 ((pQueueFamilyIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ PhysicalDeviceImageDrmFormatModifierInfoEXT
             drmFormatModifier sharingMode pQueueFamilyIndices'

instance Zero PhysicalDeviceImageDrmFormatModifierInfoEXT where
  zero = PhysicalDeviceImageDrmFormatModifierInfoEXT
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkImageDrmFormatModifierListCreateInfoEXT"
data ImageDrmFormatModifierListCreateInfoEXT = ImageDrmFormatModifierListCreateInfoEXT
  { -- No documentation found for Nested "VkImageDrmFormatModifierListCreateInfoEXT" "pDrmFormatModifiers"
    drmFormatModifiers :: Vector Word64 }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageDrmFormatModifierListCreateInfoEXT)
#endif
deriving instance Show ImageDrmFormatModifierListCreateInfoEXT

instance ToCStruct ImageDrmFormatModifierListCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageDrmFormatModifierListCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (drmFormatModifiers)) :: Word32))
    pPDrmFormatModifiers' <- ContT $ allocaBytesAligned @Word64 ((Data.Vector.length (drmFormatModifiers)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDrmFormatModifiers' `plusPtr` (8 * (i)) :: Ptr Word64) (e)) (drmFormatModifiers)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word64))) (pPDrmFormatModifiers')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPDrmFormatModifiers' <- ContT $ allocaBytesAligned @Word64 ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDrmFormatModifiers' `plusPtr` (8 * (i)) :: Ptr Word64) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word64))) (pPDrmFormatModifiers')
    lift $ f

instance FromCStruct ImageDrmFormatModifierListCreateInfoEXT where
  peekCStruct p = do
    drmFormatModifierCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pDrmFormatModifiers <- peek @(Ptr Word64) ((p `plusPtr` 24 :: Ptr (Ptr Word64)))
    pDrmFormatModifiers' <- generateM (fromIntegral drmFormatModifierCount) (\i -> peek @Word64 ((pDrmFormatModifiers `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pure $ ImageDrmFormatModifierListCreateInfoEXT
             pDrmFormatModifiers'

instance Zero ImageDrmFormatModifierListCreateInfoEXT where
  zero = ImageDrmFormatModifierListCreateInfoEXT
           mempty



-- No documentation found for TopLevel "VkImageDrmFormatModifierExplicitCreateInfoEXT"
data ImageDrmFormatModifierExplicitCreateInfoEXT = ImageDrmFormatModifierExplicitCreateInfoEXT
  { -- No documentation found for Nested "VkImageDrmFormatModifierExplicitCreateInfoEXT" "drmFormatModifier"
    drmFormatModifier :: Word64
  , -- No documentation found for Nested "VkImageDrmFormatModifierExplicitCreateInfoEXT" "pPlaneLayouts"
    planeLayouts :: Vector SubresourceLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageDrmFormatModifierExplicitCreateInfoEXT)
#endif
deriving instance Show ImageDrmFormatModifierExplicitCreateInfoEXT

instance ToCStruct ImageDrmFormatModifierExplicitCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageDrmFormatModifierExplicitCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word64)) (drmFormatModifier)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (planeLayouts)) :: Word32))
    pPPlaneLayouts' <- ContT $ allocaBytesAligned @SubresourceLayout ((Data.Vector.length (planeLayouts)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPlaneLayouts' `plusPtr` (40 * (i)) :: Ptr SubresourceLayout) (e)) (planeLayouts)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr SubresourceLayout))) (pPPlaneLayouts')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    pPPlaneLayouts' <- ContT $ allocaBytesAligned @SubresourceLayout ((Data.Vector.length (mempty)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPlaneLayouts' `plusPtr` (40 * (i)) :: Ptr SubresourceLayout) (e)) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr SubresourceLayout))) (pPPlaneLayouts')
    lift $ f

instance FromCStruct ImageDrmFormatModifierExplicitCreateInfoEXT where
  peekCStruct p = do
    drmFormatModifier <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    drmFormatModifierPlaneCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pPlaneLayouts <- peek @(Ptr SubresourceLayout) ((p `plusPtr` 32 :: Ptr (Ptr SubresourceLayout)))
    pPlaneLayouts' <- generateM (fromIntegral drmFormatModifierPlaneCount) (\i -> peekCStruct @SubresourceLayout ((pPlaneLayouts `advancePtrBytes` (40 * (i)) :: Ptr SubresourceLayout)))
    pure $ ImageDrmFormatModifierExplicitCreateInfoEXT
             drmFormatModifier pPlaneLayouts'

instance Zero ImageDrmFormatModifierExplicitCreateInfoEXT where
  zero = ImageDrmFormatModifierExplicitCreateInfoEXT
           zero
           mempty



-- No documentation found for TopLevel "VkImageDrmFormatModifierPropertiesEXT"
data ImageDrmFormatModifierPropertiesEXT = ImageDrmFormatModifierPropertiesEXT
  { -- No documentation found for Nested "VkImageDrmFormatModifierPropertiesEXT" "drmFormatModifier"
    drmFormatModifier :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageDrmFormatModifierPropertiesEXT)
#endif
deriving instance Show ImageDrmFormatModifierPropertiesEXT

instance ToCStruct ImageDrmFormatModifierPropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageDrmFormatModifierPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (drmFormatModifier)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    f

instance FromCStruct ImageDrmFormatModifierPropertiesEXT where
  peekCStruct p = do
    drmFormatModifier <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    pure $ ImageDrmFormatModifierPropertiesEXT
             drmFormatModifier


instance Storable ImageDrmFormatModifierPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageDrmFormatModifierPropertiesEXT where
  zero = ImageDrmFormatModifierPropertiesEXT
           zero


type EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION"
pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION = 1


type EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME = "VK_EXT_image_drm_format_modifier"

-- No documentation found for TopLevel "VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME"
pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME = "VK_EXT_image_drm_format_modifier"

