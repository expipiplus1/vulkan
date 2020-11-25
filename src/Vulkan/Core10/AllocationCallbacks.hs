{-# language CPP #-}
-- No documentation found for Chapter "AllocationCallbacks"
module Vulkan.Core10.AllocationCallbacks  (AllocationCallbacks(..)) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (plusPtr)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.FuncPointers (PFN_vkAllocationFunction)
import Vulkan.Core10.FuncPointers (PFN_vkFreeFunction)
import Vulkan.Core10.FuncPointers (PFN_vkInternalAllocationNotification)
import Vulkan.Core10.FuncPointers (PFN_vkInternalFreeNotification)
import Vulkan.Core10.FuncPointers (PFN_vkReallocationFunction)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))

-- No documentation found for TopLevel "VkAllocationCallbacks"
data AllocationCallbacks = AllocationCallbacks
  { -- No documentation found for Nested "VkAllocationCallbacks" "pUserData"
    userData :: Ptr ()
  , -- No documentation found for Nested "VkAllocationCallbacks" "pfnAllocation"
    pfnAllocation :: PFN_vkAllocationFunction
  , -- No documentation found for Nested "VkAllocationCallbacks" "pfnReallocation"
    pfnReallocation :: PFN_vkReallocationFunction
  , -- No documentation found for Nested "VkAllocationCallbacks" "pfnFree"
    pfnFree :: PFN_vkFreeFunction
  , -- No documentation found for Nested "VkAllocationCallbacks" "pfnInternalAllocation"
    pfnInternalAllocation :: PFN_vkInternalAllocationNotification
  , -- No documentation found for Nested "VkAllocationCallbacks" "pfnInternalFree"
    pfnInternalFree :: PFN_vkInternalFreeNotification
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AllocationCallbacks)
#endif
deriving instance Show AllocationCallbacks

instance ToCStruct AllocationCallbacks where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AllocationCallbacks{..} f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr ()))) (userData)
    poke ((p `plusPtr` 8 :: Ptr PFN_vkAllocationFunction)) (pfnAllocation)
    poke ((p `plusPtr` 16 :: Ptr PFN_vkReallocationFunction)) (pfnReallocation)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkFreeFunction)) (pfnFree)
    poke ((p `plusPtr` 32 :: Ptr PFN_vkInternalAllocationNotification)) (pfnInternalAllocation)
    poke ((p `plusPtr` 40 :: Ptr PFN_vkInternalFreeNotification)) (pfnInternalFree)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 8 :: Ptr PFN_vkAllocationFunction)) (zero)
    poke ((p `plusPtr` 16 :: Ptr PFN_vkReallocationFunction)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkFreeFunction)) (zero)
    f

instance FromCStruct AllocationCallbacks where
  peekCStruct p = do
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 0 :: Ptr (Ptr ())))
    pfnAllocation <- peek @PFN_vkAllocationFunction ((p `plusPtr` 8 :: Ptr PFN_vkAllocationFunction))
    pfnReallocation <- peek @PFN_vkReallocationFunction ((p `plusPtr` 16 :: Ptr PFN_vkReallocationFunction))
    pfnFree <- peek @PFN_vkFreeFunction ((p `plusPtr` 24 :: Ptr PFN_vkFreeFunction))
    pfnInternalAllocation <- peek @PFN_vkInternalAllocationNotification ((p `plusPtr` 32 :: Ptr PFN_vkInternalAllocationNotification))
    pfnInternalFree <- peek @PFN_vkInternalFreeNotification ((p `plusPtr` 40 :: Ptr PFN_vkInternalFreeNotification))
    pure $ AllocationCallbacks
             pUserData pfnAllocation pfnReallocation pfnFree pfnInternalAllocation pfnInternalFree


instance Storable AllocationCallbacks where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AllocationCallbacks where
  zero = AllocationCallbacks
           zero
           zero
           zero
           zero
           zero
           zero

