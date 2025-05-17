{-# language CPP #-}
-- | = Name
--
-- VK_NV_low_latency - device extension
--
-- == VK_NV_low_latency
--
-- [__Name String__]
--     @VK_NV_low_latency@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     311
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Charles Hansen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_low_latency] @cshansen%0A*Here describe the issue or question you have about the VK_NV_low_latency extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-02-10
--
-- [__Contributors__]
--
--     -   Charles Hansen, NVIDIA
--
-- == Description
--
-- This extension adds the 'QueryLowLatencySupportNV' structure, a
-- structure used to query support for NVIDIA Reflex.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo':
--
--     -   'QueryLowLatencySupportNV'
--
-- == New Enum Constants
--
-- -   'NV_LOW_LATENCY_EXTENSION_NAME'
--
-- -   'NV_LOW_LATENCY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUERY_LOW_LATENCY_SUPPORT_NV'
--
-- == Issues
--
-- 1) Why does 'QueryLowLatencySupportNV' have output parameters in an
-- input chain?
--
-- __RESOLVED__: We are stuck with this for legacy reasons - we are aware
-- this is bad behavior and this should not be used as a precedent for
-- future extensions.
--
-- == Version History
--
-- -   Revision 1, 2023-02-10 (Charles Hansen)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'QueryLowLatencySupportNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_low_latency Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_low_latency  ( QueryLowLatencySupportNV(..)
                                            , NV_LOW_LATENCY_SPEC_VERSION
                                            , pattern NV_LOW_LATENCY_SPEC_VERSION
                                            , NV_LOW_LATENCY_EXTENSION_NAME
                                            , pattern NV_LOW_LATENCY_EXTENSION_NAME
                                            ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUERY_LOW_LATENCY_SUPPORT_NV))
-- | VkQueryLowLatencySupportNV - Structure used for NVIDIA Reflex Support
--
-- = Members
--
-- This structure describes the following feature:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency VK_NV_low_latency>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data QueryLowLatencySupportNV = QueryLowLatencySupportNV
  { -- | @pQueriedLowLatencyData@ is used for NVIDIA Reflex Support.
    --
    -- #VUID-VkQueryLowLatencySupportNV-pQueriedLowLatencyData-parameter#
    -- @pQueriedLowLatencyData@ /must/ be a pointer value
    queriedLowLatencyData :: Ptr () }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueryLowLatencySupportNV)
#endif
deriving instance Show QueryLowLatencySupportNV

instance ToCStruct QueryLowLatencySupportNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueryLowLatencySupportNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUERY_LOW_LATENCY_SUPPORT_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (queriedLowLatencyData)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUERY_LOW_LATENCY_SUPPORT_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct QueryLowLatencySupportNV where
  peekCStruct p = do
    pQueriedLowLatencyData <- peek @(Ptr ()) ((p `plusPtr` 16 :: Ptr (Ptr ())))
    pure $ QueryLowLatencySupportNV
             pQueriedLowLatencyData

instance Storable QueryLowLatencySupportNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueryLowLatencySupportNV where
  zero = QueryLowLatencySupportNV
           zero


type NV_LOW_LATENCY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_LOW_LATENCY_SPEC_VERSION"
pattern NV_LOW_LATENCY_SPEC_VERSION :: forall a . Integral a => a
pattern NV_LOW_LATENCY_SPEC_VERSION = 1


type NV_LOW_LATENCY_EXTENSION_NAME = "VK_NV_low_latency"

-- No documentation found for TopLevel "VK_NV_LOW_LATENCY_EXTENSION_NAME"
pattern NV_LOW_LATENCY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_LOW_LATENCY_EXTENSION_NAME = "VK_NV_low_latency"

