{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_format_feature_flags2"
module Vulkan.Core13.Promoted_From_VK_KHR_format_feature_flags2  ( FormatProperties3(..)
                                                                 , StructureType(..)
                                                                 , FormatFeatureFlagBits2(..)
                                                                 , FormatFeatureFlags2
                                                                 ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core13.Enums.FormatFeatureFlags2 (FormatFeatureFlags2)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FORMAT_PROPERTIES_3))
import Vulkan.Core13.Enums.FormatFeatureFlags2 (FormatFeatureFlagBits2(..))
import Vulkan.Core13.Enums.FormatFeatureFlags2 (FormatFeatureFlags2)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkFormatProperties3 - Structure specifying image format properties
--
-- = Description
--
-- The bits reported in @linearTilingFeatures@, @optimalTilingFeatures@ and
-- @bufferFeatures@ /must/ include the bits reported in the corresponding
-- fields of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2'::@formatProperties@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlags2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data FormatProperties3 = FormatProperties3
  { -- | @linearTilingFeatures@ is a bitmask of
    -- 'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2'
    -- specifying features supported by images created with a @tiling@
    -- parameter of 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR'.
    linearTilingFeatures :: FormatFeatureFlags2
  , -- | @optimalTilingFeatures@ is a bitmask of
    -- 'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2'
    -- specifying features supported by images created with a @tiling@
    -- parameter of 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'.
    optimalTilingFeatures :: FormatFeatureFlags2
  , -- | @bufferFeatures@ is a bitmask of
    -- 'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2'
    -- specifying features supported by buffers.
    bufferFeatures :: FormatFeatureFlags2
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FormatProperties3)
#endif
deriving instance Show FormatProperties3

instance ToCStruct FormatProperties3 where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FormatProperties3{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FORMAT_PROPERTIES_3)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FormatFeatureFlags2)) (linearTilingFeatures)
    poke ((p `plusPtr` 24 :: Ptr FormatFeatureFlags2)) (optimalTilingFeatures)
    poke ((p `plusPtr` 32 :: Ptr FormatFeatureFlags2)) (bufferFeatures)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FORMAT_PROPERTIES_3)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct FormatProperties3 where
  peekCStruct p = do
    linearTilingFeatures <- peek @FormatFeatureFlags2 ((p `plusPtr` 16 :: Ptr FormatFeatureFlags2))
    optimalTilingFeatures <- peek @FormatFeatureFlags2 ((p `plusPtr` 24 :: Ptr FormatFeatureFlags2))
    bufferFeatures <- peek @FormatFeatureFlags2 ((p `plusPtr` 32 :: Ptr FormatFeatureFlags2))
    pure $ FormatProperties3
             linearTilingFeatures optimalTilingFeatures bufferFeatures

instance Storable FormatProperties3 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FormatProperties3 where
  zero = FormatProperties3
           zero
           zero
           zero

