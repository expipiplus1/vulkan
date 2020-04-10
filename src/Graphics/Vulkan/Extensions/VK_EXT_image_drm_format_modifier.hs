{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier  ( getImageDrmFormatModifierPropertiesEXT
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

import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetImageDrmFormatModifierPropertiesEXT))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Image)
import Graphics.Vulkan.Core10.Handles (Image(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.SharingMode (SharingMode)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.Core10.Image (SubresourceLayout)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageDrmFormatModifierPropertiesEXT
  :: FunPtr (Ptr Device_T -> Image -> Ptr ImageDrmFormatModifierPropertiesEXT -> IO Result) -> Ptr Device_T -> Image -> Ptr ImageDrmFormatModifierPropertiesEXT -> IO Result

-- | vkGetImageDrmFormatModifierPropertiesEXT - Returns an image’s DRM format
-- modifier
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @image@ is the queried image.
--
-- -   @pProperties@ will return properties of the image’s /DRM format
--     modifier/.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.Image',
-- 'ImageDrmFormatModifierPropertiesEXT'
getImageDrmFormatModifierPropertiesEXT :: forall io . MonadIO io => Device -> Image -> io (ImageDrmFormatModifierPropertiesEXT)
getImageDrmFormatModifierPropertiesEXT device image = liftIO . evalContT $ do
  let vkGetImageDrmFormatModifierPropertiesEXT' = mkVkGetImageDrmFormatModifierPropertiesEXT (pVkGetImageDrmFormatModifierPropertiesEXT (deviceCmds (device :: Device)))
  pPProperties <- ContT (withZeroCStruct @ImageDrmFormatModifierPropertiesEXT)
  _ <- lift $ vkGetImageDrmFormatModifierPropertiesEXT' (deviceHandle (device)) (image) (pPProperties)
  pProperties <- lift $ peekCStruct @ImageDrmFormatModifierPropertiesEXT pPProperties
  pure $ (pProperties)


-- | VkDrmFormatModifierPropertiesListEXT - Structure specifying the list of
-- DRM format modifiers supported for a format
--
-- = Description
--
-- If @pDrmFormatModifierProperties@ is @NULL@, then the function returns
-- in @drmFormatModifierCount@ the number of modifiers compatible with the
-- queried @format@. Otherwise, the application /must/ set
-- @drmFormatModifierCount@ to the length of the array
-- @pDrmFormatModifierProperties@; the function will write at most
-- @drmFormatModifierCount@ elements to the array, and will return in
-- @drmFormatModifierCount@ the number of elements written.
--
-- Among the elements in array @pDrmFormatModifierProperties@, each
-- returned @drmFormatModifier@ /must/ be unique.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DrmFormatModifierPropertiesEXT',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DrmFormatModifierPropertiesListEXT = DrmFormatModifierPropertiesListEXT
  { -- | @drmFormatModifierCount@ is an inout parameter related to the number of
    -- modifiers compatible with the @format@, as described below.
    drmFormatModifierCount :: Word32
  , -- | @pDrmFormatModifierProperties@ is either @NULL@ or an array of
    -- 'DrmFormatModifierPropertiesEXT' structures.
    drmFormatModifierProperties :: Ptr DrmFormatModifierPropertiesEXT
  }
  deriving (Typeable)
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


-- | VkDrmFormatModifierPropertiesEXT - Structure specifying properties of a
-- format when combined with a DRM format modifier
--
-- = Description
--
-- The returned @drmFormatModifierTilingFeatures@ /must/ contain at least
-- one bit.
--
-- The implementation /must/ not return @DRM_FORMAT_MOD_INVALID@ in
-- @drmFormatModifier@.
--
-- An image’s /memory planecount/ (as returned by
-- @drmFormatModifierPlaneCount@) is distinct from its /format planecount/
-- (in the sense of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
-- Y′CBCR formats). In
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- each @VK_IMAGE_ASPECT_MEMORY_PLANE@//i/_BIT_EXT represents a _memory
-- plane/ and each @VK_IMAGE_ASPECT_PLANE@//i/_BIT a _format plane/.
--
-- An image’s set of /format planes/ is an ordered partition of the image’s
-- __content__ into separable groups of format channels. The ordered
-- partition is encoded in the name of each
-- 'Graphics.Vulkan.Core10.Enums.Format.Format'. For example,
-- 'Graphics.Vulkan.Core10.Enums.Format.FORMAT_G8_B8R8_2PLANE_420_UNORM'
-- contains two /format planes/; the first plane contains the green channel
-- and the second plane contains the blue channel and red channel. If the
-- format name does not contain @PLANE@, then the format contains a single
-- plane; for example,
-- 'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R8G8B8A8_UNORM'. Some
-- commands, such as
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage', do
-- not operate on all format channels in the image, but instead operate
-- only on the /format planes/ explicitly chosen by the application and
-- operate on each /format plane/ independently.
--
-- An image’s set of /memory planes/ is an ordered partition of the image’s
-- __memory__ rather than the image’s __content__. Each /memory plane/ is a
-- contiguous range of memory. The union of an image’s /memory planes/ is
-- not necessarily contiguous.
--
-- If an image is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-linear-resource linear>,
-- then the partition is the same for /memory planes/ and for /format
-- planes/. Therefore, if the returned @drmFormatModifier@ is
-- @DRM_FORMAT_MOD_LINEAR@, then @drmFormatModifierPlaneCount@ /must/ equal
-- the /format planecount/, and @drmFormatModifierTilingFeatures@ /must/ be
-- identical to the
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2'::@linearTilingFeatures@
-- returned in the same @pNext@ chain.
--
-- If an image is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-linear-resource non-linear>,
-- then the partition of the image’s __memory__ into /memory planes/ is
-- implementation-specific and /may/ be unrelated to the partition of the
-- image’s __content__ into /format planes/. For example, consider an image
-- whose @format@ is
-- 'Graphics.Vulkan.Core10.Enums.Format.FORMAT_G8_B8_R8_3PLANE_420_UNORM',
-- @tiling@ is
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
-- whose @drmFormatModifier@ is not @DRM_FORMAT_MOD_LINEAR@, and @flags@
-- lacks
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'.
-- The image has 3 /format planes/, and commands such
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage' act
-- on each /format plane/ independently as if the data of each /format
-- plane/ were separable from the data of the other planes. In a
-- straightforward implementation, the implementation /may/ store the
-- image’s content in 3 adjacent /memory planes/ where each /memory plane/
-- corresponds exactly to a /format plane/. However, the implementation
-- /may/ also store the image’s content in a single /memory plane/ where
-- all format channels are combined using an implementation-private
-- block-compressed format; or the implementation /may/ store the image’s
-- content in a collection of 7 adjacent /memory planes/ using an
-- implementation-private sharding technique. Because the image is
-- non-linear and non-disjoint, the implementation has much freedom when
-- choosing the image’s placement in memory.
--
-- The /memory planecount/ applies to function parameters and structures
-- only when the API specifies an explicit requirement on
-- @drmFormatModifierPlaneCount@. In all other cases, the /memory
-- planecount/ is ignored.
--
-- = See Also
--
-- 'DrmFormatModifierPropertiesListEXT',
-- 'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlags'
data DrmFormatModifierPropertiesEXT = DrmFormatModifierPropertiesEXT
  { -- | @drmFormatModifier@ is a /Linux DRM format modifier/.
    drmFormatModifier :: Word64
  , -- | @drmFormatModifierPlaneCount@ is the number of /memory planes/ in any
    -- image created with @format@ and @drmFormatModifier@. An image’s /memory
    -- planecount/ is distinct from its /format planecount/, as explained
    -- below.
    drmFormatModifierPlaneCount :: Word32
  , -- | @drmFormatModifierTilingFeatures@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits'
    -- that are supported by any image created with @format@ and
    -- @drmFormatModifier@.
    drmFormatModifierTilingFeatures :: FormatFeatureFlags
  }
  deriving (Typeable)
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


-- | VkPhysicalDeviceImageDrmFormatModifierInfoEXT - Structure specifying a
-- DRM format modifier as image creation parameter
--
-- = Description
--
-- If the @drmFormatModifier@ is incompatible with the parameters specified
-- in
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
-- and its @pNext@ chain, then
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
-- returns
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'. The
-- implementation /must/ support the query of any @drmFormatModifier@,
-- including unknown and invalid modifier values.
--
-- == Valid Usage
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     then @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values.
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     then @queueFamilyIndexCount@ /must/ be greater than @1@.
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     each element of @pQueueFamilyIndices@ /must/ be unique and /must/ be
--     less than the @pQueueFamilyPropertyCount@ returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2'
--     for the @physicalDevice@ that was used to create @device@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT'
--
-- -   @sharingMode@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode' value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageDrmFormatModifierInfoEXT = PhysicalDeviceImageDrmFormatModifierInfoEXT
  { -- | @drmFormatModifier@ is the image’s /Linux DRM format modifier/,
    -- corresponding to
    -- 'ImageDrmFormatModifierExplicitCreateInfoEXT'::@modifier@ or to
    -- 'ImageDrmFormatModifierListCreateInfoEXT'::@pModifiers@.
    drmFormatModifier :: Word64
  , -- | @sharingMode@ specifies how the image will be accessed by multiple queue
    -- families.
    sharingMode :: SharingMode
  , -- | @pQueueFamilyIndices@ is a list of queue families that will access the
    -- image (ignored if @sharingMode@ is not
    -- 'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT').
    queueFamilyIndices :: Vector Word32
  }
  deriving (Typeable)
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


-- | VkImageDrmFormatModifierListCreateInfoEXT - Specify that an image must
-- be created with a DRM format modifier from the provided list
--
-- == Valid Usage
--
-- -   Each /modifier/ in @pDrmFormatModifiers@ must be compatible with the
--     parameters in 'Graphics.Vulkan.Core10.Image.ImageCreateInfo' and its
--     @pNext@ chain, as determined by querying
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
--     extended with 'PhysicalDeviceImageDrmFormatModifierInfoEXT'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT'
--
-- -   @pDrmFormatModifiers@ /must/ be a valid pointer to an array of
--     @drmFormatModifierCount@ @uint64_t@ values
--
-- -   @drmFormatModifierCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data ImageDrmFormatModifierListCreateInfoEXT = ImageDrmFormatModifierListCreateInfoEXT
  { -- | @pDrmFormatModifiers@ is a pointer to an array of /Linux DRM format
    -- modifiers/.
    drmFormatModifiers :: Vector Word64 }
  deriving (Typeable)
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


-- | VkImageDrmFormatModifierExplicitCreateInfoEXT - Specify that an image be
-- created with the provided DRM format modifier and explicit memory layout
--
-- = Description
--
-- The @i@th member of @pPlaneLayouts@ describes the layout of the image’s
-- @i@th /memory plane/ (that is,
-- @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@). In each element of
-- @pPlaneLayouts@, the implementation /must/ ignore @size@. The
-- implementation calculates the size of each plane, which the application
-- /can/ query with
-- 'Graphics.Vulkan.Core10.Image.getImageSubresourceLayout'.
--
-- When creating an image with
-- 'ImageDrmFormatModifierExplicitCreateInfoEXT', it is the application’s
-- responsibility to satisfy all valid usage requirements. However, the
-- implementation /must/ validate that the provided @pPlaneLayouts@, when
-- combined with the provided @drmFormatModifier@ and other creation
-- parameters in 'Graphics.Vulkan.Core10.Image.ImageCreateInfo' and its
-- @pNext@ chain, produce a valid image. (This validation is necessarily
-- implementation-dependent and outside the scope of Vulkan, and therefore
-- not described by valid usage requirements). If this validation fails,
-- then 'Graphics.Vulkan.Core10.Image.createImage' returns
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT'.
--
-- == Valid Usage
--
-- -   @drmFormatModifier@ must be compatible with the parameters in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' and its @pNext@
--     chain, as determined by querying
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
--     extended with 'PhysicalDeviceImageDrmFormatModifierInfoEXT'.
--
-- -   @drmFormatModifierPlaneCount@ /must/ be equal to the
--     'DrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
--     associated with
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@format@ and
--     @drmFormatModifier@, as found by querying
--     'DrmFormatModifierPropertiesListEXT'.
--
-- -   For each element of @pPlaneLayouts@, @size@ /must/ be 0
--
-- -   For each element of @pPlaneLayouts@, @arrayPitch@ /must/ be 0 if
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@arrayLayers@ is 1.
--
-- -   For each element of @pPlaneLayouts@, @depthPitch@ /must/ be 0 if
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@extent.depth@ is 1.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT'
--
-- -   If @drmFormatModifierPlaneCount@ is not @0@, @pPlaneLayouts@ /must/
--     be a valid pointer to an array of @drmFormatModifierPlaneCount@
--     'Graphics.Vulkan.Core10.Image.SubresourceLayout' structures
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Graphics.Vulkan.Core10.Image.SubresourceLayout'
data ImageDrmFormatModifierExplicitCreateInfoEXT = ImageDrmFormatModifierExplicitCreateInfoEXT
  { -- | @drmFormatModifier@ is the /Linux DRM format modifier/ with which the
    -- image will be created.
    drmFormatModifier :: Word64
  , -- | @pPlaneLayouts@ is a pointer to an array of
    -- 'Graphics.Vulkan.Core10.Image.SubresourceLayout' structures describing
    -- the image’s /memory planes/.
    planeLayouts :: Vector SubresourceLayout
  }
  deriving (Typeable)
deriving instance Show ImageDrmFormatModifierExplicitCreateInfoEXT

instance ToCStruct ImageDrmFormatModifierExplicitCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageDrmFormatModifierExplicitCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word64)) (drmFormatModifier)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (planeLayouts)) :: Word32))
    pPPlaneLayouts' <- ContT $ allocaBytesAligned @SubresourceLayout ((Data.Vector.length (planeLayouts)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPPlaneLayouts' `plusPtr` (40 * (i)) :: Ptr SubresourceLayout) (e) . ($ ())) (planeLayouts)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr SubresourceLayout))) (pPPlaneLayouts')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    pPPlaneLayouts' <- ContT $ allocaBytesAligned @SubresourceLayout ((Data.Vector.length (mempty)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPPlaneLayouts' `plusPtr` (40 * (i)) :: Ptr SubresourceLayout) (e) . ($ ())) (mempty)
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


-- | VkImageDrmFormatModifierPropertiesEXT - Properties of an image’s Linux
-- DRM format modifier
--
-- = Description
--
-- If the @image@ was created with
-- 'ImageDrmFormatModifierListCreateInfoEXT', then the returned
-- @drmFormatModifier@ /must/ belong to the list of modifiers provided at
-- time of image creation in
-- 'ImageDrmFormatModifierListCreateInfoEXT'::@pDrmFormatModifiers@. If the
-- @image@ was created with 'ImageDrmFormatModifierExplicitCreateInfoEXT',
-- then the returned @drmFormatModifier@ /must/ be the modifier provided at
-- time of image creation in
-- 'ImageDrmFormatModifierExplicitCreateInfoEXT'::@drmFormatModifier@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageDrmFormatModifierPropertiesEXT'
data ImageDrmFormatModifierPropertiesEXT = ImageDrmFormatModifierPropertiesEXT
  { -- | @drmFormatModifier@ returns the image’s
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-drm-format-modifier Linux DRM format modifier>.
    drmFormatModifier :: Word64 }
  deriving (Typeable)
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

