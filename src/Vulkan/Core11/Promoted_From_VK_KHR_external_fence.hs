{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_external_fence"
module Vulkan.Core11.Promoted_From_VK_KHR_external_fence  ( ExportFenceCreateInfo(..)
                                                          , StructureType(..)
                                                          , FenceImportFlagBits(..)
                                                          , FenceImportFlags
                                                          ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO))
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlagBits(..))
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkExportFenceCreateInfo - Structure specifying handle types that can be
-- exported from a fence
--
-- == Valid Usage
--
-- -   #VUID-VkExportFenceCreateInfo-handleTypes-01446# The bits in
--     @handleTypes@ /must/ be supported and compatible, as reported by
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities.ExternalFenceProperties'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExportFenceCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO'
--
-- -   #VUID-VkExportFenceCreateInfo-handleTypes-parameter# @handleTypes@
--     /must/ be a valid combination of
--     'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits'
--     values
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportFenceCreateInfo = ExportFenceCreateInfo
  { -- | @handleTypes@ is a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits'
    -- specifying one or more fence handle types the application /can/ export
    -- from the resulting fence. The application /can/ request multiple handle
    -- types for the same fence.
    handleTypes :: ExternalFenceHandleTypeFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportFenceCreateInfo)
#endif
deriving instance Show ExportFenceCreateInfo

instance ToCStruct ExportFenceCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportFenceCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalFenceHandleTypeFlags)) (handleTypes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExportFenceCreateInfo where
  peekCStruct p = do
    handleTypes <- peek @ExternalFenceHandleTypeFlags ((p `plusPtr` 16 :: Ptr ExternalFenceHandleTypeFlags))
    pure $ ExportFenceCreateInfo
             handleTypes

instance Storable ExportFenceCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportFenceCreateInfo where
  zero = ExportFenceCreateInfo
           zero

