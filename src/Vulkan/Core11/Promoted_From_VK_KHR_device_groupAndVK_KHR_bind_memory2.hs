{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2"
module Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2  ( BindBufferMemoryDeviceGroupInfo(..)
                                                                              , BindImageMemoryDeviceGroupInfo(..)
                                                                              , StructureType(..)
                                                                              , ImageCreateFlagBits(..)
                                                                              , ImageCreateFlags
                                                                              ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkBindBufferMemoryDeviceGroupInfo - Structure specifying device within a
-- group to bind to
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo'
-- includes a 'BindBufferMemoryDeviceGroupInfo' structure, then that
-- structure determines how memory is bound to buffers across multiple
-- devices in a device group.
--
-- If @deviceIndexCount@ is greater than zero, then on device index i the
-- buffer is attached to the instance of @memory@ on the physical device
-- with device index @pDeviceIndices@[i].
--
-- If @deviceIndexCount@ is zero and @memory@ comes from a memory heap with
-- the
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- bit set, then it is as if @pDeviceIndices@ contains consecutive indices
-- from zero to the number of physical devices in the logical device, minus
-- one. In other words, by default each physical device attaches to its own
-- instance of @memory@.
--
-- If @deviceIndexCount@ is zero and @memory@ comes from a memory heap
-- without the
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- bit set, then it is as if @pDeviceIndices@ contains an array of zeros.
-- In other words, by default each physical device attaches to instance
-- zero.
--
-- == Valid Usage
--
-- -   #VUID-VkBindBufferMemoryDeviceGroupInfo-deviceIndexCount-01606#
--     @deviceIndexCount@ /must/ either be zero or equal to the number of
--     physical devices in the logical device
--
-- -   #VUID-VkBindBufferMemoryDeviceGroupInfo-pDeviceIndices-01607# All
--     elements of @pDeviceIndices@ /must/ be valid device indices
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindBufferMemoryDeviceGroupInfo-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO'
--
-- -   #VUID-VkBindBufferMemoryDeviceGroupInfo-pDeviceIndices-parameter# If
--     @deviceIndexCount@ is not @0@, @pDeviceIndices@ /must/ be a valid
--     pointer to an array of @deviceIndexCount@ @uint32_t@ values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BindBufferMemoryDeviceGroupInfo = BindBufferMemoryDeviceGroupInfo
  { -- | @pDeviceIndices@ is a pointer to an array of device indices.
    deviceIndices :: Vector Word32 }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindBufferMemoryDeviceGroupInfo)
#endif
deriving instance Show BindBufferMemoryDeviceGroupInfo

instance ToCStruct BindBufferMemoryDeviceGroupInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindBufferMemoryDeviceGroupInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceIndices)) :: Word32))
    pPDeviceIndices' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (deviceIndices)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (deviceIndices)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPDeviceIndices')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct BindBufferMemoryDeviceGroupInfo where
  peekCStruct p = do
    deviceIndexCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pDeviceIndices <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pDeviceIndices' <- generateM (fromIntegral deviceIndexCount) (\i -> peek @Word32 ((pDeviceIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ BindBufferMemoryDeviceGroupInfo
             pDeviceIndices'

instance Zero BindBufferMemoryDeviceGroupInfo where
  zero = BindBufferMemoryDeviceGroupInfo
           mempty


-- | VkBindImageMemoryDeviceGroupInfo - Structure specifying device within a
-- group to bind to
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo'
-- includes a 'BindImageMemoryDeviceGroupInfo' structure, then that
-- structure determines how memory is bound to images across multiple
-- devices in a device group.
--
-- If @deviceIndexCount@ is greater than zero, then on device index i
-- @image@ is attached to the instance of the memory on the physical device
-- with device index pDeviceIndices[i].
--
-- Let N be the number of physical devices in the logical device. If
-- @splitInstanceBindRegionCount@ is greater than zero, then
-- @pSplitInstanceBindRegions@ is a pointer to an array of N2 rectangles,
-- where the image region specified by the rectangle at element i*N+j in
-- resource instance i is bound to the memory instance j. The blocks of the
-- memory that are bound to each sparse image block region use an offset in
-- memory, relative to @memoryOffset@, computed as if the whole image was
-- being bound to a contiguous range of memory. In other words,
-- horizontally adjacent image blocks use consecutive blocks of memory,
-- vertically adjacent image blocks are separated by the number of bytes
-- per block multiplied by the width in blocks of @image@, and the block at
-- (0,0) corresponds to memory starting at @memoryOffset@.
--
-- If @splitInstanceBindRegionCount@ and @deviceIndexCount@ are zero and
-- the memory comes from a memory heap with the
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- bit set, then it is as if @pDeviceIndices@ contains consecutive indices
-- from zero to the number of physical devices in the logical device, minus
-- one. In other words, by default each physical device attaches to its own
-- instance of the memory.
--
-- If @splitInstanceBindRegionCount@ and @deviceIndexCount@ are zero and
-- the memory comes from a memory heap without the
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- bit set, then it is as if @pDeviceIndices@ contains an array of zeros.
-- In other words, by default each physical device attaches to instance
-- zero.
--
-- == Valid Usage
--
-- -   #VUID-VkBindImageMemoryDeviceGroupInfo-deviceIndexCount-01633# At
--     least one of @deviceIndexCount@ and @splitInstanceBindRegionCount@
--     /must/ be zero
--
-- -   #VUID-VkBindImageMemoryDeviceGroupInfo-deviceIndexCount-01634#
--     @deviceIndexCount@ /must/ either be zero or equal to the number of
--     physical devices in the logical device
--
-- -   #VUID-VkBindImageMemoryDeviceGroupInfo-pDeviceIndices-01635# All
--     elements of @pDeviceIndices@ /must/ be valid device indices
--
-- -   #VUID-VkBindImageMemoryDeviceGroupInfo-splitInstanceBindRegionCount-01636#
--     @splitInstanceBindRegionCount@ /must/ either be zero or equal to the
--     number of physical devices in the logical device squared
--
-- -   #VUID-VkBindImageMemoryDeviceGroupInfo-pSplitInstanceBindRegions-01637#
--     Elements of @pSplitInstanceBindRegions@ that correspond to the same
--     instance of an image /must/ not overlap
--
-- -   #VUID-VkBindImageMemoryDeviceGroupInfo-offset-01638# The @offset.x@
--     member of any element of @pSplitInstanceBindRegions@ /must/ be a
--     multiple of the sparse image block width
--     ('Vulkan.Core10.SparseResourceMemoryManagement.SparseImageFormatProperties'::@imageGranularity.width@)
--     of all non-metadata aspects of the image
--
-- -   #VUID-VkBindImageMemoryDeviceGroupInfo-offset-01639# The @offset.y@
--     member of any element of @pSplitInstanceBindRegions@ /must/ be a
--     multiple of the sparse image block height
--     ('Vulkan.Core10.SparseResourceMemoryManagement.SparseImageFormatProperties'::@imageGranularity.height@)
--     of all non-metadata aspects of the image
--
-- -   #VUID-VkBindImageMemoryDeviceGroupInfo-extent-01640# The
--     @extent.width@ member of any element of @pSplitInstanceBindRegions@
--     /must/ either be a multiple of the sparse image block width of all
--     non-metadata aspects of the image, or else @extent.width@ +
--     @offset.x@ /must/ equal the width of the image subresource
--
-- -   #VUID-VkBindImageMemoryDeviceGroupInfo-extent-01641# The
--     @extent.height@ member of any element of @pSplitInstanceBindRegions@
--     /must/ either be a multiple of the sparse image block height of all
--     non-metadata aspects of the image, or else @extent.height@ +
--     @offset.y@ /must/ equal the height of the image subresource
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindImageMemoryDeviceGroupInfo-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO'
--
-- -   #VUID-VkBindImageMemoryDeviceGroupInfo-pDeviceIndices-parameter# If
--     @deviceIndexCount@ is not @0@, @pDeviceIndices@ /must/ be a valid
--     pointer to an array of @deviceIndexCount@ @uint32_t@ values
--
-- -   #VUID-VkBindImageMemoryDeviceGroupInfo-pSplitInstanceBindRegions-parameter#
--     If @splitInstanceBindRegionCount@ is not @0@,
--     @pSplitInstanceBindRegions@ /must/ be a valid pointer to an array of
--     @splitInstanceBindRegionCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.FundamentalTypes.Rect2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BindImageMemoryDeviceGroupInfo = BindImageMemoryDeviceGroupInfo
  { -- | @pDeviceIndices@ is a pointer to an array of device indices.
    deviceIndices :: Vector Word32
  , -- | @pSplitInstanceBindRegions@ is a pointer to an array of
    -- 'Vulkan.Core10.FundamentalTypes.Rect2D' structures describing which
    -- regions of the image are attached to each instance of memory.
    splitInstanceBindRegions :: Vector Rect2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindImageMemoryDeviceGroupInfo)
#endif
deriving instance Show BindImageMemoryDeviceGroupInfo

instance ToCStruct BindImageMemoryDeviceGroupInfo where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindImageMemoryDeviceGroupInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceIndices)) :: Word32))
    pPDeviceIndices' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (deviceIndices)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (deviceIndices)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPDeviceIndices')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (splitInstanceBindRegions)) :: Word32))
    pPSplitInstanceBindRegions' <- ContT $ allocaBytes @Rect2D ((Data.Vector.length (splitInstanceBindRegions)) * 16)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSplitInstanceBindRegions' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (splitInstanceBindRegions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Rect2D))) (pPSplitInstanceBindRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct BindImageMemoryDeviceGroupInfo where
  peekCStruct p = do
    deviceIndexCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pDeviceIndices <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pDeviceIndices' <- generateM (fromIntegral deviceIndexCount) (\i -> peek @Word32 ((pDeviceIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    splitInstanceBindRegionCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pSplitInstanceBindRegions <- peek @(Ptr Rect2D) ((p `plusPtr` 40 :: Ptr (Ptr Rect2D)))
    pSplitInstanceBindRegions' <- generateM (fromIntegral splitInstanceBindRegionCount) (\i -> peekCStruct @Rect2D ((pSplitInstanceBindRegions `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
    pure $ BindImageMemoryDeviceGroupInfo
             pDeviceIndices' pSplitInstanceBindRegions'

instance Zero BindImageMemoryDeviceGroupInfo where
  zero = BindImageMemoryDeviceGroupInfo
           mempty
           mempty

