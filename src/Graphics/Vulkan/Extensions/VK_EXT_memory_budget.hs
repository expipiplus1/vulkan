{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_memory_budget  ( PhysicalDeviceMemoryBudgetPropertiesEXT(..)
                                                        , EXT_MEMORY_BUDGET_SPEC_VERSION
                                                        , pattern EXT_MEMORY_BUDGET_SPEC_VERSION
                                                        , EXT_MEMORY_BUDGET_EXTENSION_NAME
                                                        , pattern EXT_MEMORY_BUDGET_EXTENSION_NAME
                                                        ) where

import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector.Storable.Sized (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.CStruct.Utils (lowerArrayPtr)
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.APIConstants (MAX_MEMORY_HEAPS)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.APIConstants (pattern MAX_MEMORY_HEAPS)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT))
-- | VkPhysicalDeviceMemoryBudgetPropertiesEXT - Structure specifying
-- physical device memory budget and usage
--
-- = Description
--
-- The values returned in this structure are not invariant. The
-- @heapBudget@ and @heapUsage@ values /must/ be zero for array elements
-- greater than or equal to
-- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceMemoryProperties'::@memoryHeapCount@.
-- The @heapBudget@ value /must/ be non-zero for array elements less than
-- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceMemoryProperties'::@memoryHeapCount@.
-- The @heapBudget@ value /must/ be less than or equal to
-- 'Graphics.Vulkan.Core10.DeviceInitialization.MemoryHeap'::@size@ for
-- each heap.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMemoryBudgetPropertiesEXT = PhysicalDeviceMemoryBudgetPropertiesEXT
  { -- | @heapBudget@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.MAX_MEMORY_HEAPS'
    -- 'Graphics.Vulkan.Core10.BaseType.DeviceSize' values in which memory
    -- budgets are returned, with one element for each memory heap. A heap’s
    -- budget is a rough estimate of how much memory the process /can/ allocate
    -- from that heap before allocations /may/ fail or cause performance
    -- degradation. The budget includes any currently allocated device memory.
    heapBudget :: Vector DeviceSize
  , -- | @heapUsage@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.MAX_MEMORY_HEAPS'
    -- 'Graphics.Vulkan.Core10.BaseType.DeviceSize' values in which memory
    -- usages are returned, with one element for each memory heap. A heap’s
    -- usage is an estimate of how much memory the process is currently using
    -- in that heap.
    heapUsage :: Vector DeviceSize
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceMemoryBudgetPropertiesEXT

instance ToCStruct PhysicalDeviceMemoryBudgetPropertiesEXT where
  withCStruct x f = allocaBytesAligned 272 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMemoryBudgetPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    unless ((Data.Vector.length $ (heapBudget)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "heapBudget is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_MEMORY_HEAPS DeviceSize)))) `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (heapBudget)
    unless ((Data.Vector.length $ (heapUsage)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "heapUsage is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 144 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_MEMORY_HEAPS DeviceSize)))) `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (heapUsage)
    f
  cStructSize = 272
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    unless ((Data.Vector.length $ (mempty)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "heapBudget is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_MEMORY_HEAPS DeviceSize)))) `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (mempty)
    unless ((Data.Vector.length $ (mempty)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "heapUsage is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 144 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_MEMORY_HEAPS DeviceSize)))) `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (mempty)
    f

instance FromCStruct PhysicalDeviceMemoryBudgetPropertiesEXT where
  peekCStruct p = do
    heapBudget <- generateM (MAX_MEMORY_HEAPS) (\i -> peek @DeviceSize (((lowerArrayPtr @DeviceSize ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_MEMORY_HEAPS DeviceSize)))) `advancePtrBytes` (8 * (i)) :: Ptr DeviceSize)))
    heapUsage <- generateM (MAX_MEMORY_HEAPS) (\i -> peek @DeviceSize (((lowerArrayPtr @DeviceSize ((p `plusPtr` 144 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_MEMORY_HEAPS DeviceSize)))) `advancePtrBytes` (8 * (i)) :: Ptr DeviceSize)))
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

