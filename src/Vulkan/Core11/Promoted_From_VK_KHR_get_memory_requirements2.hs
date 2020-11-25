{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_get_memory_requirements2"
module Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2  ( getBufferMemoryRequirements2
                                                                    , getImageMemoryRequirements2
                                                                    , getImageSparseMemoryRequirements2
                                                                    , BufferMemoryRequirementsInfo2(..)
                                                                    , ImageMemoryRequirementsInfo2(..)
                                                                    , ImageSparseMemoryRequirementsInfo2(..)
                                                                    , MemoryRequirements2(..)
                                                                    , SparseImageMemoryRequirements2(..)
                                                                    , StructureType(..)
                                                                    ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
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
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetBufferMemoryRequirements2))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageMemoryRequirements2))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageSparseMemoryRequirements2))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (ImagePlaneMemoryRequirementsInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation (MemoryDedicatedRequirements)
import Vulkan.Core10.MemoryManagement (MemoryRequirements)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.SparseResourceMemoryManagement (SparseImageMemoryRequirements)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferMemoryRequirements2
  :: FunPtr (Ptr Device_T -> Ptr BufferMemoryRequirementsInfo2 -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr BufferMemoryRequirementsInfo2 -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- No documentation found for TopLevel "vkGetBufferMemoryRequirements2"
getBufferMemoryRequirements2 :: forall a io
                              . (Extendss MemoryRequirements2 a, PokeChain a, PeekChain a, MonadIO io)
                             => -- No documentation found for Nested "vkGetBufferMemoryRequirements2" "device"
                                Device
                             -> -- No documentation found for Nested "vkGetBufferMemoryRequirements2" "pInfo"
                                BufferMemoryRequirementsInfo2
                             -> io (MemoryRequirements2 a)
getBufferMemoryRequirements2 device info = liftIO . evalContT $ do
  let vkGetBufferMemoryRequirements2Ptr = pVkGetBufferMemoryRequirements2 (deviceCmds (device :: Device))
  lift $ unless (vkGetBufferMemoryRequirements2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetBufferMemoryRequirements2 is null" Nothing Nothing
  let vkGetBufferMemoryRequirements2' = mkVkGetBufferMemoryRequirements2 vkGetBufferMemoryRequirements2Ptr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ vkGetBufferMemoryRequirements2' (deviceHandle (device)) pInfo (forgetExtensions (pPMemoryRequirements))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageMemoryRequirements2
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct ImageMemoryRequirementsInfo2) -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr (SomeStruct ImageMemoryRequirementsInfo2) -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- No documentation found for TopLevel "vkGetImageMemoryRequirements2"
getImageMemoryRequirements2 :: forall a b io
                             . (Extendss ImageMemoryRequirementsInfo2 a, Extendss MemoryRequirements2 b, PokeChain a, PokeChain b, PeekChain b, MonadIO io)
                            => -- No documentation found for Nested "vkGetImageMemoryRequirements2" "device"
                               Device
                            -> -- No documentation found for Nested "vkGetImageMemoryRequirements2" "pInfo"
                               (ImageMemoryRequirementsInfo2 a)
                            -> io (MemoryRequirements2 b)
getImageMemoryRequirements2 device info = liftIO . evalContT $ do
  let vkGetImageMemoryRequirements2Ptr = pVkGetImageMemoryRequirements2 (deviceCmds (device :: Device))
  lift $ unless (vkGetImageMemoryRequirements2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageMemoryRequirements2 is null" Nothing Nothing
  let vkGetImageMemoryRequirements2' = mkVkGetImageMemoryRequirements2 vkGetImageMemoryRequirements2Ptr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ vkGetImageMemoryRequirements2' (deviceHandle (device)) (forgetExtensions pInfo) (forgetExtensions (pPMemoryRequirements))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSparseMemoryRequirements2
  :: FunPtr (Ptr Device_T -> Ptr ImageSparseMemoryRequirementsInfo2 -> Ptr Word32 -> Ptr SparseImageMemoryRequirements2 -> IO ()) -> Ptr Device_T -> Ptr ImageSparseMemoryRequirementsInfo2 -> Ptr Word32 -> Ptr SparseImageMemoryRequirements2 -> IO ()

-- No documentation found for TopLevel "vkGetImageSparseMemoryRequirements2"
getImageSparseMemoryRequirements2 :: forall io
                                   . (MonadIO io)
                                  => -- No documentation found for Nested "vkGetImageSparseMemoryRequirements2" "device"
                                     Device
                                  -> -- No documentation found for Nested "vkGetImageSparseMemoryRequirements2" "pInfo"
                                     ImageSparseMemoryRequirementsInfo2
                                  -> io (("sparseMemoryRequirements" ::: Vector SparseImageMemoryRequirements2))
getImageSparseMemoryRequirements2 device info = liftIO . evalContT $ do
  let vkGetImageSparseMemoryRequirements2Ptr = pVkGetImageSparseMemoryRequirements2 (deviceCmds (device :: Device))
  lift $ unless (vkGetImageSparseMemoryRequirements2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageSparseMemoryRequirements2 is null" Nothing Nothing
  let vkGetImageSparseMemoryRequirements2' = mkVkGetImageSparseMemoryRequirements2 vkGetImageSparseMemoryRequirements2Ptr
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



-- No documentation found for TopLevel "VkBufferMemoryRequirementsInfo2"
data BufferMemoryRequirementsInfo2 = BufferMemoryRequirementsInfo2
  { -- No documentation found for Nested "VkBufferMemoryRequirementsInfo2" "buffer"
    buffer :: Buffer }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferMemoryRequirementsInfo2)
#endif
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



-- No documentation found for TopLevel "VkImageMemoryRequirementsInfo2"
data ImageMemoryRequirementsInfo2 (es :: [Type]) = ImageMemoryRequirementsInfo2
  { -- No documentation found for Nested "VkImageMemoryRequirementsInfo2" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkImageMemoryRequirementsInfo2" "image"
    image :: Image
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageMemoryRequirementsInfo2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageMemoryRequirementsInfo2 es)

instance Extensible ImageMemoryRequirementsInfo2 where
  extensibleType = STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  setNext x next = x{next = next}
  getNext ImageMemoryRequirementsInfo2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageMemoryRequirementsInfo2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImagePlaneMemoryRequirementsInfo = Just f
    | otherwise = Nothing

instance (Extendss ImageMemoryRequirementsInfo2 es, PokeChain es) => ToCStruct (ImageMemoryRequirementsInfo2 es) where
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

instance (Extendss ImageMemoryRequirementsInfo2 es, PeekChain es) => FromCStruct (ImageMemoryRequirementsInfo2 es) where
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



-- No documentation found for TopLevel "VkImageSparseMemoryRequirementsInfo2"
data ImageSparseMemoryRequirementsInfo2 = ImageSparseMemoryRequirementsInfo2
  { -- No documentation found for Nested "VkImageSparseMemoryRequirementsInfo2" "image"
    image :: Image }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageSparseMemoryRequirementsInfo2)
#endif
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



-- No documentation found for TopLevel "VkMemoryRequirements2"
data MemoryRequirements2 (es :: [Type]) = MemoryRequirements2
  { -- No documentation found for Nested "VkMemoryRequirements2" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkMemoryRequirements2" "memoryRequirements"
    memoryRequirements :: MemoryRequirements
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryRequirements2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (MemoryRequirements2 es)

instance Extensible MemoryRequirements2 where
  extensibleType = STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  setNext x next = x{next = next}
  getNext MemoryRequirements2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends MemoryRequirements2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @MemoryDedicatedRequirements = Just f
    | otherwise = Nothing

instance (Extendss MemoryRequirements2 es, PokeChain es) => ToCStruct (MemoryRequirements2 es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryRequirements2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr MemoryRequirements)) (memoryRequirements)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr MemoryRequirements)) (zero)
    lift $ f

instance (Extendss MemoryRequirements2 es, PeekChain es) => FromCStruct (MemoryRequirements2 es) where
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



-- No documentation found for TopLevel "VkSparseImageMemoryRequirements2"
data SparseImageMemoryRequirements2 = SparseImageMemoryRequirements2
  { -- No documentation found for Nested "VkSparseImageMemoryRequirements2" "memoryRequirements"
    memoryRequirements :: SparseImageMemoryRequirements }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseImageMemoryRequirements2)
#endif
deriving instance Show SparseImageMemoryRequirements2

instance ToCStruct SparseImageMemoryRequirements2 where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageMemoryRequirements2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SparseImageMemoryRequirements)) (memoryRequirements)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SparseImageMemoryRequirements)) (zero)
    f

instance FromCStruct SparseImageMemoryRequirements2 where
  peekCStruct p = do
    memoryRequirements <- peekCStruct @SparseImageMemoryRequirements ((p `plusPtr` 16 :: Ptr SparseImageMemoryRequirements))
    pure $ SparseImageMemoryRequirements2
             memoryRequirements


instance Storable SparseImageMemoryRequirements2 where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SparseImageMemoryRequirements2 where
  zero = SparseImageMemoryRequirements2
           zero

