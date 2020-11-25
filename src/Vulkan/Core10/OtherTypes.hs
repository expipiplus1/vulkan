{-# language CPP #-}
-- No documentation found for Chapter "OtherTypes"
module Vulkan.Core10.OtherTypes  ( MemoryBarrier(..)
                                 , BufferMemoryBarrier(..)
                                 , ImageMemoryBarrier(..)
                                 , DrawIndirectCommand(..)
                                 , DrawIndexedIndirectCommand(..)
                                 , DispatchIndirectCommand(..)
                                 , BaseOutStructure(..)
                                 , BaseInStructure(..)
                                 , ObjectType(..)
                                 , VendorId(..)
                                 ) where

import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.ImageView (ImageSubresourceRange)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (SampleLocationsInfoEXT)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_BARRIER))
import Vulkan.CStruct.Extends (BaseInStructure(..))
import Vulkan.CStruct.Extends (BaseOutStructure(..))
import Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Vulkan.Core10.Enums.VendorId (VendorId(..))

-- No documentation found for TopLevel "VkMemoryBarrier"
data MemoryBarrier = MemoryBarrier
  { -- No documentation found for Nested "VkMemoryBarrier" "srcAccessMask"
    srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "VkMemoryBarrier" "dstAccessMask"
    dstAccessMask :: AccessFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryBarrier)
#endif
deriving instance Show MemoryBarrier

instance ToCStruct MemoryBarrier where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryBarrier{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_BARRIER)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccessFlags)) (srcAccessMask)
    poke ((p `plusPtr` 20 :: Ptr AccessFlags)) (dstAccessMask)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_BARRIER)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct MemoryBarrier where
  peekCStruct p = do
    srcAccessMask <- peek @AccessFlags ((p `plusPtr` 16 :: Ptr AccessFlags))
    dstAccessMask <- peek @AccessFlags ((p `plusPtr` 20 :: Ptr AccessFlags))
    pure $ MemoryBarrier
             srcAccessMask dstAccessMask


instance Storable MemoryBarrier where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryBarrier where
  zero = MemoryBarrier
           zero
           zero



-- No documentation found for TopLevel "VkBufferMemoryBarrier"
data BufferMemoryBarrier = BufferMemoryBarrier
  { -- No documentation found for Nested "VkBufferMemoryBarrier" "srcAccessMask"
    srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "dstAccessMask"
    dstAccessMask :: AccessFlags
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "srcQueueFamilyIndex"
    srcQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "dstQueueFamilyIndex"
    dstQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "buffer"
    buffer :: Buffer
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "offset"
    offset :: DeviceSize
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "size"
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferMemoryBarrier)
#endif
deriving instance Show BufferMemoryBarrier

instance ToCStruct BufferMemoryBarrier where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferMemoryBarrier{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccessFlags)) (srcAccessMask)
    poke ((p `plusPtr` 20 :: Ptr AccessFlags)) (dstAccessMask)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (srcQueueFamilyIndex)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (dstQueueFamilyIndex)
    poke ((p `plusPtr` 32 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccessFlags)) (zero)
    poke ((p `plusPtr` 20 :: Ptr AccessFlags)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BufferMemoryBarrier where
  peekCStruct p = do
    srcAccessMask <- peek @AccessFlags ((p `plusPtr` 16 :: Ptr AccessFlags))
    dstAccessMask <- peek @AccessFlags ((p `plusPtr` 20 :: Ptr AccessFlags))
    srcQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    dstQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    buffer <- peek @Buffer ((p `plusPtr` 32 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    pure $ BufferMemoryBarrier
             srcAccessMask dstAccessMask srcQueueFamilyIndex dstQueueFamilyIndex buffer offset size


instance Storable BufferMemoryBarrier where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferMemoryBarrier where
  zero = BufferMemoryBarrier
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkImageMemoryBarrier"
data ImageMemoryBarrier (es :: [Type]) = ImageMemoryBarrier
  { -- No documentation found for Nested "VkImageMemoryBarrier" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkImageMemoryBarrier" "srcAccessMask"
    srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "VkImageMemoryBarrier" "dstAccessMask"
    dstAccessMask :: AccessFlags
  , -- No documentation found for Nested "VkImageMemoryBarrier" "oldLayout"
    oldLayout :: ImageLayout
  , -- No documentation found for Nested "VkImageMemoryBarrier" "newLayout"
    newLayout :: ImageLayout
  , -- No documentation found for Nested "VkImageMemoryBarrier" "srcQueueFamilyIndex"
    srcQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkImageMemoryBarrier" "dstQueueFamilyIndex"
    dstQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkImageMemoryBarrier" "image"
    image :: Image
  , -- No documentation found for Nested "VkImageMemoryBarrier" "subresourceRange"
    subresourceRange :: ImageSubresourceRange
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageMemoryBarrier (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageMemoryBarrier es)

instance Extensible ImageMemoryBarrier where
  extensibleType = STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
  setNext x next = x{next = next}
  getNext ImageMemoryBarrier{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageMemoryBarrier e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SampleLocationsInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss ImageMemoryBarrier es, PokeChain es) => ToCStruct (ImageMemoryBarrier es) where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageMemoryBarrier{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr AccessFlags)) (srcAccessMask)
    lift $ poke ((p `plusPtr` 20 :: Ptr AccessFlags)) (dstAccessMask)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (oldLayout)
    lift $ poke ((p `plusPtr` 28 :: Ptr ImageLayout)) (newLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (srcQueueFamilyIndex)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (dstQueueFamilyIndex)
    lift $ poke ((p `plusPtr` 40 :: Ptr Image)) (image)
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageSubresourceRange)) (subresourceRange)
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr AccessFlags)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr AccessFlags)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageSubresourceRange)) (zero)
    lift $ f

instance (Extendss ImageMemoryBarrier es, PeekChain es) => FromCStruct (ImageMemoryBarrier es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    srcAccessMask <- peek @AccessFlags ((p `plusPtr` 16 :: Ptr AccessFlags))
    dstAccessMask <- peek @AccessFlags ((p `plusPtr` 20 :: Ptr AccessFlags))
    oldLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    newLayout <- peek @ImageLayout ((p `plusPtr` 28 :: Ptr ImageLayout))
    srcQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    dstQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    image <- peek @Image ((p `plusPtr` 40 :: Ptr Image))
    subresourceRange <- peekCStruct @ImageSubresourceRange ((p `plusPtr` 48 :: Ptr ImageSubresourceRange))
    pure $ ImageMemoryBarrier
             next srcAccessMask dstAccessMask oldLayout newLayout srcQueueFamilyIndex dstQueueFamilyIndex image subresourceRange

instance es ~ '[] => Zero (ImageMemoryBarrier es) where
  zero = ImageMemoryBarrier
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkDrawIndirectCommand"
data DrawIndirectCommand = DrawIndirectCommand
  { -- No documentation found for Nested "VkDrawIndirectCommand" "vertexCount"
    vertexCount :: Word32
  , -- No documentation found for Nested "VkDrawIndirectCommand" "instanceCount"
    instanceCount :: Word32
  , -- No documentation found for Nested "VkDrawIndirectCommand" "firstVertex"
    firstVertex :: Word32
  , -- No documentation found for Nested "VkDrawIndirectCommand" "firstInstance"
    firstInstance :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrawIndirectCommand)
#endif
deriving instance Show DrawIndirectCommand

instance ToCStruct DrawIndirectCommand where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrawIndirectCommand{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (vertexCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (instanceCount)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (firstVertex)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (firstInstance)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct DrawIndirectCommand where
  peekCStruct p = do
    vertexCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    instanceCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    firstVertex <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    firstInstance <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ DrawIndirectCommand
             vertexCount instanceCount firstVertex firstInstance


instance Storable DrawIndirectCommand where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrawIndirectCommand where
  zero = DrawIndirectCommand
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkDrawIndexedIndirectCommand"
data DrawIndexedIndirectCommand = DrawIndexedIndirectCommand
  { -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "indexCount"
    indexCount :: Word32
  , -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "instanceCount"
    instanceCount :: Word32
  , -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "firstIndex"
    firstIndex :: Word32
  , -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "vertexOffset"
    vertexOffset :: Int32
  , -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "firstInstance"
    firstInstance :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrawIndexedIndirectCommand)
#endif
deriving instance Show DrawIndexedIndirectCommand

instance ToCStruct DrawIndexedIndirectCommand where
  withCStruct x f = allocaBytesAligned 20 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrawIndexedIndirectCommand{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (indexCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (instanceCount)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (firstIndex)
    poke ((p `plusPtr` 12 :: Ptr Int32)) (vertexOffset)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (firstInstance)
    f
  cStructSize = 20
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DrawIndexedIndirectCommand where
  peekCStruct p = do
    indexCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    instanceCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    firstIndex <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    vertexOffset <- peek @Int32 ((p `plusPtr` 12 :: Ptr Int32))
    firstInstance <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DrawIndexedIndirectCommand
             indexCount instanceCount firstIndex vertexOffset firstInstance


instance Storable DrawIndexedIndirectCommand where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrawIndexedIndirectCommand where
  zero = DrawIndexedIndirectCommand
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkDispatchIndirectCommand"
data DispatchIndirectCommand = DispatchIndirectCommand
  { -- No documentation found for Nested "VkDispatchIndirectCommand" "x"
    x :: Word32
  , -- No documentation found for Nested "VkDispatchIndirectCommand" "y"
    y :: Word32
  , -- No documentation found for Nested "VkDispatchIndirectCommand" "z"
    z :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DispatchIndirectCommand)
#endif
deriving instance Show DispatchIndirectCommand

instance ToCStruct DispatchIndirectCommand where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DispatchIndirectCommand{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (x)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (y)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (z)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct DispatchIndirectCommand where
  peekCStruct p = do
    x <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    y <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    z <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ DispatchIndirectCommand
             x y z


instance Storable DispatchIndirectCommand where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DispatchIndirectCommand where
  zero = DispatchIndirectCommand
           zero
           zero
           zero

