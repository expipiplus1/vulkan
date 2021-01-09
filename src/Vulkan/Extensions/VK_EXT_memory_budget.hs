{-# language CPP #-}
-- | = Name
--
-- VK_EXT_memory_budget - device extension
--
-- == VK_EXT_memory_budget
--
-- [__Name String__]
--     @VK_EXT_memory_budget@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     238
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_memory_budget:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-10-08
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
-- == Description
--
-- While running a Vulkan application, other processes on the machine might
-- also be attempting to use the same device memory, which can pose
-- problems. This extension adds support for querying the amount of memory
-- used and the total memory budget for a memory heap. The values returned
-- by this query are implementation-dependent and can depend on a variety
-- of factors including operating system and system load.
--
-- The 'PhysicalDeviceMemoryBudgetPropertiesEXT'::@heapBudget@ values can
-- be used as a guideline for how much total memory from each heap the
-- __current process__ can use at any given time, before allocations may
-- start failing or causing performance degradation. The values may change
-- based on other activity in the system that is outside the scope and
-- control of the Vulkan implementation.
--
-- The 'PhysicalDeviceMemoryBudgetPropertiesEXT'::@heapUsage@ will display
-- the __current process__ estimated heap usage.
--
-- With this information, the idea is for an application at some interval
-- (once per frame, per few seconds, etc) to query @heapBudget@ and
-- @heapUsage@. From here the application can notice if it is over budget
-- and decide how it wants to handle the memory situation (free it, move to
-- host memory, changing mipmap levels, etc). This extension is designed to
-- be used in concert with
-- <VK_EXT_memory_priority.html VK_EXT_memory_priority> to help with this
-- part of memory management.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceMemoryProperties2':
--
--     -   'PhysicalDeviceMemoryBudgetPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MEMORY_BUDGET_EXTENSION_NAME'
--
-- -   'EXT_MEMORY_BUDGET_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2018-10-08 (Jeff Bolz)
--
--     -   Initial revision
--
-- = See Also
--
-- 'PhysicalDeviceMemoryBudgetPropertiesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_budget Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_memory_budget  ( PhysicalDeviceMemoryBudgetPropertiesEXT(..)
                                               , EXT_MEMORY_BUDGET_SPEC_VERSION
                                               , pattern EXT_MEMORY_BUDGET_SPEC_VERSION
                                               , EXT_MEMORY_BUDGET_EXTENSION_NAME
                                               , pattern EXT_MEMORY_BUDGET_EXTENSION_NAME
                                               ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.APIConstants (MAX_MEMORY_HEAPS)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (pattern MAX_MEMORY_HEAPS)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT))
-- | VkPhysicalDeviceMemoryBudgetPropertiesEXT - Structure specifying
-- physical device memory budget and usage
--
-- = Description
--
-- The values returned in this structure are not invariant. The
-- @heapBudget@ and @heapUsage@ values /must/ be zero for array elements
-- greater than or equal to
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceMemoryProperties'::@memoryHeapCount@.
-- The @heapBudget@ value /must/ be non-zero for array elements less than
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceMemoryProperties'::@memoryHeapCount@.
-- The @heapBudget@ value /must/ be less than or equal to
-- 'Vulkan.Core10.DeviceInitialization.MemoryHeap'::@size@ for each heap.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMemoryBudgetPropertiesEXT = PhysicalDeviceMemoryBudgetPropertiesEXT
  { -- | @heapBudget@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_MEMORY_HEAPS'
    -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' values in which memory
    -- budgets are returned, with one element for each memory heap. A heap’s
    -- budget is a rough estimate of how much memory the process /can/ allocate
    -- from that heap before allocations /may/ fail or cause performance
    -- degradation. The budget includes any currently allocated device memory.
    heapBudget :: Vector DeviceSize
  , -- | @heapUsage@ is an array of 'Vulkan.Core10.APIConstants.MAX_MEMORY_HEAPS'
    -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' values in which memory
    -- usages are returned, with one element for each memory heap. A heap’s
    -- usage is an estimate of how much memory the process is currently using
    -- in that heap.
    heapUsage :: Vector DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMemoryBudgetPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceMemoryBudgetPropertiesEXT

instance ToCStruct PhysicalDeviceMemoryBudgetPropertiesEXT where
  withCStruct x f = allocaBytesAligned 272 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMemoryBudgetPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    unless ((Data.Vector.length $ (heapBudget)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "heapBudget is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_MEMORY_HEAPS DeviceSize)))) `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (heapBudget)
    unless ((Data.Vector.length $ (heapUsage)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "heapUsage is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 144 :: Ptr (FixedArray MAX_MEMORY_HEAPS DeviceSize)))) `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (heapUsage)
    f
  cStructSize = 272
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PhysicalDeviceMemoryBudgetPropertiesEXT where
  peekCStruct p = do
    heapBudget <- generateM (MAX_MEMORY_HEAPS) (\i -> peek @DeviceSize (((lowerArrayPtr @DeviceSize ((p `plusPtr` 16 :: Ptr (FixedArray MAX_MEMORY_HEAPS DeviceSize)))) `advancePtrBytes` (8 * (i)) :: Ptr DeviceSize)))
    heapUsage <- generateM (MAX_MEMORY_HEAPS) (\i -> peek @DeviceSize (((lowerArrayPtr @DeviceSize ((p `plusPtr` 144 :: Ptr (FixedArray MAX_MEMORY_HEAPS DeviceSize)))) `advancePtrBytes` (8 * (i)) :: Ptr DeviceSize)))
    pure $ PhysicalDeviceMemoryBudgetPropertiesEXT
             heapBudget heapUsage

instance Storable PhysicalDeviceMemoryBudgetPropertiesEXT where
  sizeOf ~_ = 272
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMemoryBudgetPropertiesEXT where
  zero = PhysicalDeviceMemoryBudgetPropertiesEXT
           mempty
           mempty


type EXT_MEMORY_BUDGET_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_MEMORY_BUDGET_SPEC_VERSION"
pattern EXT_MEMORY_BUDGET_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_MEMORY_BUDGET_SPEC_VERSION = 1


type EXT_MEMORY_BUDGET_EXTENSION_NAME = "VK_EXT_memory_budget"

-- No documentation found for TopLevel "VK_EXT_MEMORY_BUDGET_EXTENSION_NAME"
pattern EXT_MEMORY_BUDGET_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_MEMORY_BUDGET_EXTENSION_NAME = "VK_EXT_memory_budget"

