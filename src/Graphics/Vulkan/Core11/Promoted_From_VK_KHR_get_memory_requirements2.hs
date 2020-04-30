{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2  ( getBufferMemoryRequirements2
                                                                             , getImageMemoryRequirements2
                                                                             , getImageSparseMemoryRequirements2
                                                                             , BufferMemoryRequirementsInfo2(..)
                                                                             , ImageMemoryRequirementsInfo2(..)
                                                                             , ImageSparseMemoryRequirementsInfo2(..)
                                                                             , MemoryRequirements2(..)
                                                                             , SparseImageMemoryRequirements2(..)
                                                                             , MemoryRequirements2KHR
                                                                             , StructureType(..)
                                                                             ) where

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetBufferMemoryRequirements2))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetImageMemoryRequirements2))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetImageSparseMemoryRequirements2))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Image)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (ImagePlaneMemoryRequirementsInfo)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation (MemoryDedicatedRequirements)
import Graphics.Vulkan.Core10.MemoryManagement (MemoryRequirements)
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement (SparseImageMemoryRequirements)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferMemoryRequirements2
  :: FunPtr (Ptr Device_T -> Ptr BufferMemoryRequirementsInfo2 -> Ptr (MemoryRequirements2 a) -> IO ()) -> Ptr Device_T -> Ptr BufferMemoryRequirementsInfo2 -> Ptr (MemoryRequirements2 a) -> IO ()

-- | vkGetBufferMemoryRequirements2 - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the buffer.
--
-- -   @pInfo@ is a pointer to a 'BufferMemoryRequirementsInfo2' structure
--     containing parameters required for the memory requirements query.
--
-- -   @pMemoryRequirements@ is a pointer to a 'MemoryRequirements2'
--     structure in which the memory requirements of the buffer object are
--     returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'BufferMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.Core10.Handles.Device', 'MemoryRequirements2'
getBufferMemoryRequirements2 :: forall a io . (PokeChain a, PeekChain a, MonadIO io) => Device -> BufferMemoryRequirementsInfo2 -> io (MemoryRequirements2 a)
getBufferMemoryRequirements2 device info = liftIO . evalContT $ do
  let vkGetBufferMemoryRequirements2' = mkVkGetBufferMemoryRequirements2 (pVkGetBufferMemoryRequirements2 (deviceCmds (device :: Device)))
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ vkGetBufferMemoryRequirements2' (deviceHandle (device)) pInfo (pPMemoryRequirements)
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageMemoryRequirements2
  :: FunPtr (Ptr Device_T -> Ptr (ImageMemoryRequirementsInfo2 a) -> Ptr (MemoryRequirements2 b) -> IO ()) -> Ptr Device_T -> Ptr (ImageMemoryRequirementsInfo2 a) -> Ptr (MemoryRequirements2 b) -> IO ()

-- | vkGetImageMemoryRequirements2 - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @pInfo@ is a pointer to a 'ImageMemoryRequirementsInfo2' structure
--     containing parameters required for the memory requirements query.
--
-- -   @pMemoryRequirements@ is a pointer to a 'MemoryRequirements2'
--     structure in which the memory requirements of the image object are
--     returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device', 'ImageMemoryRequirementsInfo2',
-- 'MemoryRequirements2'
getImageMemoryRequirements2 :: forall a b io . (PokeChain a, PokeChain b, PeekChain b, MonadIO io) => Device -> ImageMemoryRequirementsInfo2 a -> io (MemoryRequirements2 b)
getImageMemoryRequirements2 device info = liftIO . evalContT $ do
  let vkGetImageMemoryRequirements2' = mkVkGetImageMemoryRequirements2 (pVkGetImageMemoryRequirements2 (deviceCmds (device :: Device)))
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ vkGetImageMemoryRequirements2' (deviceHandle (device)) pInfo (pPMemoryRequirements)
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSparseMemoryRequirements2
  :: FunPtr (Ptr Device_T -> Ptr ImageSparseMemoryRequirementsInfo2 -> Ptr Word32 -> Ptr SparseImageMemoryRequirements2 -> IO ()) -> Ptr Device_T -> Ptr ImageSparseMemoryRequirementsInfo2 -> Ptr Word32 -> Ptr SparseImageMemoryRequirements2 -> IO ()

-- | vkGetImageSparseMemoryRequirements2 - Query the memory requirements for
-- a sparse image
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @pInfo@ is a pointer to a 'ImageSparseMemoryRequirementsInfo2'
--     structure containing parameters required for the memory requirements
--     query.
--
-- -   @pSparseMemoryRequirementCount@ is a pointer to an integer related
--     to the number of sparse memory requirements available or queried, as
--     described below.
--
-- -   @pSparseMemoryRequirements@ is either @NULL@ or a pointer to an
--     array of 'SparseImageMemoryRequirements2' structures.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'ImageSparseMemoryRequirementsInfo2' structure
--
-- -   @pSparseMemoryRequirementCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pSparseMemoryRequirementCount@ is not
--     @0@, and @pSparseMemoryRequirements@ is not @NULL@,
--     @pSparseMemoryRequirements@ /must/ be a valid pointer to an array of
--     @pSparseMemoryRequirementCount@ 'SparseImageMemoryRequirements2'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'ImageSparseMemoryRequirementsInfo2', 'SparseImageMemoryRequirements2'
getImageSparseMemoryRequirements2 :: forall io . MonadIO io => Device -> ImageSparseMemoryRequirementsInfo2 -> io (("sparseMemoryRequirements" ::: Vector SparseImageMemoryRequirements2))
getImageSparseMemoryRequirements2 device info = liftIO . evalContT $ do
  let vkGetImageSparseMemoryRequirements2' = mkVkGetImageSparseMemoryRequirements2 (pVkGetImageSparseMemoryRequirements2 (deviceCmds (device :: Device)))
  let device' = deviceHandle (device)
  pInfo <- ContT $ withCStruct (info)
  pPSparseMemoryRequirementCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ vkGetImageSparseMemoryRequirements2' device' pInfo (pPSparseMemoryRequirementCount) (nullPtr)
  pSparseMemoryRequirementCount <- lift $ peek @Word32 pPSparseMemoryRequirementCount
  pPSparseMemoryRequirements <- ContT $ bracket (callocBytes @SparseImageMemoryRequirements2 ((fromIntegral (pSparseMemoryRequirementCount)) * 64)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPSparseMemoryRequirements `advancePtrBytes` (i * 64) :: Ptr SparseImageMemoryRequirements2) . ($ ())) [0..(fromIntegral (pSparseMemoryRequirementCount)) - 1]
  lift $ vkGetImageSparseMemoryRequirements2' device' pInfo (pPSparseMemoryRequirementCount) ((pPSparseMemoryRequirements))
  pSparseMemoryRequirementCount' <- lift $ peek @Word32 pPSparseMemoryRequirementCount
  pSparseMemoryRequirements' <- lift $ generateM (fromIntegral (pSparseMemoryRequirementCount')) (\i -> peekCStruct @SparseImageMemoryRequirements2 (((pPSparseMemoryRequirements) `advancePtrBytes` (64 * (i)) :: Ptr SparseImageMemoryRequirements2)))
  pure $ (pSparseMemoryRequirements')


-- | VkBufferMemoryRequirementsInfo2 - (None)
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getBufferMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.getBufferMemoryRequirements2KHR'
data BufferMemoryRequirementsInfo2 = BufferMemoryRequirementsInfo2
  { -- | @buffer@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Buffer'
    -- handle
    buffer :: Buffer }
  deriving (Typeable)
deriving instance Show BufferMemoryRequirementsInfo2

instance ToCStruct BufferMemoryRequirementsInfo2 where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferMemoryRequirementsInfo2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (buffer)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    f

instance FromCStruct BufferMemoryRequirementsInfo2 where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    pure $ BufferMemoryRequirementsInfo2
             buffer

instance Storable BufferMemoryRequirementsInfo2 where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferMemoryRequirementsInfo2 where
  zero = BufferMemoryRequirementsInfo2
           zero


-- | VkImageMemoryRequirementsInfo2 - (None)
--
-- == Valid Usage
--
-- -   If @image@ was created with a /multi-planar/ format and the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--     flag, there /must/ be a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.ImagePlaneMemoryRequirementsInfo'
--     included in the @pNext@ chain of the 'ImageMemoryRequirementsInfo2'
--     structure
--
-- -   If @image@ was created with
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--     and with
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then there /must/ be a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.ImagePlaneMemoryRequirementsInfo'
--     included in the @pNext@ chain of the 'ImageMemoryRequirementsInfo2'
--     structure
--
-- -   If @image@ was not created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--     flag, there /must/ not be a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.ImagePlaneMemoryRequirementsInfo'
--     included in the @pNext@ chain of the 'ImageMemoryRequirementsInfo2'
--     structure
--
-- -   If @image@ was created with a single-plane format and with any
--     @tiling@ other than
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then there /must/ not be a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.ImagePlaneMemoryRequirementsInfo'
--     included in the @pNext@ chain of the 'ImageMemoryRequirementsInfo2'
--     structure
--
-- -   If @image@ was created with the
--     'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     external memory handle type, then @image@ /must/ be bound to memory
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.ImagePlaneMemoryRequirementsInfo'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @image@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image'
--     handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Image',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.getImageMemoryRequirements2KHR'
data ImageMemoryRequirementsInfo2 (es :: [Type]) = ImageMemoryRequirementsInfo2
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @image@ is the image to query.
    image :: Image
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (ImageMemoryRequirementsInfo2 es)

instance Extensible ImageMemoryRequirementsInfo2 where
  extensibleType = STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  setNext x next = x{next = next}
  getNext ImageMemoryRequirementsInfo2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageMemoryRequirementsInfo2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImagePlaneMemoryRequirementsInfo = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (ImageMemoryRequirementsInfo2 es) where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageMemoryRequirementsInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (image)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    lift $ f

instance PeekChain es => FromCStruct (ImageMemoryRequirementsInfo2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    image <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    pure $ ImageMemoryRequirementsInfo2
             next image

instance es ~ '[] => Zero (ImageMemoryRequirementsInfo2 es) where
  zero = ImageMemoryRequirementsInfo2
           ()
           zero


-- | VkImageSparseMemoryRequirementsInfo2 - (None)
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Image',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageSparseMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.getImageSparseMemoryRequirements2KHR'
data ImageSparseMemoryRequirementsInfo2 = ImageSparseMemoryRequirementsInfo2
  { -- | @image@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image' handle
    image :: Image }
  deriving (Typeable)
deriving instance Show ImageSparseMemoryRequirementsInfo2

instance ToCStruct ImageSparseMemoryRequirementsInfo2 where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSparseMemoryRequirementsInfo2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (image)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    f

instance FromCStruct ImageSparseMemoryRequirementsInfo2 where
  peekCStruct p = do
    image <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    pure $ ImageSparseMemoryRequirementsInfo2
             image

instance Storable ImageSparseMemoryRequirementsInfo2 where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSparseMemoryRequirementsInfo2 where
  zero = ImageSparseMemoryRequirementsInfo2
           zero


-- | VkMemoryRequirements2 - Structure specifying memory requirements
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedRequirements'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.MemoryRequirements',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.getAccelerationStructureMemoryRequirementsKHR',
-- 'getBufferMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.getBufferMemoryRequirements2KHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.getGeneratedCommandsMemoryRequirementsNV',
-- 'getImageMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.getImageMemoryRequirements2KHR'
data MemoryRequirements2 (es :: [Type]) = MemoryRequirements2
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @memoryRequirements@ is a
    -- 'Graphics.Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
    -- describing the memory requirements of the resource.
    memoryRequirements :: MemoryRequirements
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (MemoryRequirements2 es)

instance Extensible MemoryRequirements2 where
  extensibleType = STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  setNext x next = x{next = next}
  getNext MemoryRequirements2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends MemoryRequirements2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @MemoryDedicatedRequirements = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (MemoryRequirements2 es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryRequirements2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr MemoryRequirements)) (memoryRequirements) . ($ ())
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr MemoryRequirements)) (zero) . ($ ())
    lift $ f

instance PeekChain es => FromCStruct (MemoryRequirements2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    memoryRequirements <- peekCStruct @MemoryRequirements ((p `plusPtr` 16 :: Ptr MemoryRequirements))
    pure $ MemoryRequirements2
             next memoryRequirements

instance es ~ '[] => Zero (MemoryRequirements2 es) where
  zero = MemoryRequirements2
           ()
           zero


-- | VkSparseImageMemoryRequirements2 - (None)
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryRequirements',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageSparseMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.getImageSparseMemoryRequirements2KHR'
data SparseImageMemoryRequirements2 = SparseImageMemoryRequirements2
  { -- | @memoryRequirements@ is a
    -- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryRequirements'
    -- structure describing the memory requirements of the sparse image.
    memoryRequirements :: SparseImageMemoryRequirements }
  deriving (Typeable)
deriving instance Show SparseImageMemoryRequirements2

instance ToCStruct SparseImageMemoryRequirements2 where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageMemoryRequirements2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr SparseImageMemoryRequirements)) (memoryRequirements) . ($ ())
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr SparseImageMemoryRequirements)) (zero) . ($ ())
    lift $ f

instance FromCStruct SparseImageMemoryRequirements2 where
  peekCStruct p = do
    memoryRequirements <- peekCStruct @SparseImageMemoryRequirements ((p `plusPtr` 16 :: Ptr SparseImageMemoryRequirements))
    pure $ SparseImageMemoryRequirements2
             memoryRequirements

instance Zero SparseImageMemoryRequirements2 where
  zero = SparseImageMemoryRequirements2
           zero


-- No documentation found for TopLevel "VkMemoryRequirements2KHR"
type MemoryRequirements2KHR = MemoryRequirements2

