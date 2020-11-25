{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_separate_stencil_usage"
module Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage  ( ImageStencilUsageCreateInfo(..)
                                                                  , StructureType(..)
                                                                  ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkImageStencilUsageCreateInfo"
data ImageStencilUsageCreateInfo = ImageStencilUsageCreateInfo
  { -- No documentation found for Nested "VkImageStencilUsageCreateInfo" "stencilUsage"
    stencilUsage :: ImageUsageFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageStencilUsageCreateInfo)
#endif
deriving instance Show ImageStencilUsageCreateInfo

instance ToCStruct ImageStencilUsageCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageStencilUsageCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageUsageFlags)) (stencilUsage)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageUsageFlags)) (zero)
    f

instance FromCStruct ImageStencilUsageCreateInfo where
  peekCStruct p = do
    stencilUsage <- peek @ImageUsageFlags ((p `plusPtr` 16 :: Ptr ImageUsageFlags))
    pure $ ImageStencilUsageCreateInfo
             stencilUsage


instance Storable ImageStencilUsageCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageStencilUsageCreateInfo where
  zero = ImageStencilUsageCreateInfo
           zero

