{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore  ( ExportSemaphoreCreateInfo(..)
                                                                       , StructureType(..)
                                                                       , SemaphoreImportFlagBits(..)
                                                                       , SemaphoreImportFlags
                                                                       ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO))
import Graphics.Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlagBits(..))
import Graphics.Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlags)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkExportSemaphoreCreateInfo - Structure specifying handle types that can
-- be exported from a semaphore
--
-- == Valid Usage
--
-- -   The bits in @handleTypes@ /must/ be supported and compatible, as
--     reported by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.ExternalSemaphoreProperties'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO'
--
-- -   @handleTypes@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data ExportSemaphoreCreateInfo = ExportSemaphoreCreateInfo
  { -- | @handleTypes@ is a bitmask of
    -- 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
    -- specifying one or more semaphore handle types the application /can/
    -- export from the resulting semaphore. The application /can/ request
    -- multiple handle types for the same semaphore.
    handleTypes :: ExternalSemaphoreHandleTypeFlags }
  deriving (Typeable)
deriving instance Show ExportSemaphoreCreateInfo

instance ToCStruct ExportSemaphoreCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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

