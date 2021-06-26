{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_external_semaphore"
module Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore  ( ExportSemaphoreCreateInfo(..)
                                                              , StructureType(..)
                                                              , SemaphoreImportFlagBits(..)
                                                              , SemaphoreImportFlags
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
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO))
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlagBits(..))
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkExportSemaphoreCreateInfo - Structure specifying handle types that can
-- be exported from a semaphore
--
-- == Valid Usage
--
-- -   #VUID-VkExportSemaphoreCreateInfo-handleTypes-01124# The bits in
--     @handleTypes@ /must/ be supported and compatible, as reported by
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.ExternalSemaphoreProperties'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExportSemaphoreCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO'
--
-- -   #VUID-VkExportSemaphoreCreateInfo-handleTypes-parameter#
--     @handleTypes@ /must/ be a valid combination of
--     'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
--     values
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportSemaphoreCreateInfo = ExportSemaphoreCreateInfo
  { -- | @handleTypes@ is a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
    -- specifying one or more semaphore handle types the application /can/
    -- export from the resulting semaphore. The application /can/ request
    -- multiple handle types for the same semaphore.
    handleTypes :: ExternalSemaphoreHandleTypeFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportSemaphoreCreateInfo)
#endif
deriving instance Show ExportSemaphoreCreateInfo

instance ToCStruct ExportSemaphoreCreateInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportSemaphoreCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalSemaphoreHandleTypeFlags)) (handleTypes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExportSemaphoreCreateInfo where
  peekCStruct p = do
    handleTypes <- peek @ExternalSemaphoreHandleTypeFlags ((p `plusPtr` 16 :: Ptr ExternalSemaphoreHandleTypeFlags))
    pure $ ExportSemaphoreCreateInfo
             handleTypes

instance Storable ExportSemaphoreCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportSemaphoreCreateInfo where
  zero = ExportSemaphoreCreateInfo
           zero

