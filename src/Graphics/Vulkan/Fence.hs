{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Fence where
import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkInternalAllocationType(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkSystemAllocationScope(..)
                             , PFN_vkFreeFunction
                             , PFN_vkInternalFreeNotification
                             )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkBool32(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

data VkFenceCreateInfo =
  VkFenceCreateInfo{ vkSType :: VkStructureType 
                   , vkPNext :: Ptr Void 
                   , vkFlags :: VkFenceCreateFlags 
                   }
  deriving (Eq)

instance Storable VkFenceCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkFenceCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFenceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFenceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkFenceCreateInfo))


-- ** vkResetFences
foreign import ccall "vkResetFences" vkResetFences :: 
  VkDevice -> Word32 -> Ptr VkFence -> IO VkResult

-- ** vkDestroyFence
foreign import ccall "vkDestroyFence" vkDestroyFence :: 
  VkDevice -> VkFence -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkWaitForFences
foreign import ccall "vkWaitForFences" vkWaitForFences :: 
  VkDevice ->
  Word32 -> Ptr VkFence -> VkBool32 -> Word64 -> IO VkResult

-- ** vkGetFenceStatus
foreign import ccall "vkGetFenceStatus" vkGetFenceStatus :: 
  VkDevice -> VkFence -> IO VkResult

-- ** VkFenceCreateFlags

newtype VkFenceCreateFlagBits = VkFenceCreateFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkFenceCreateFlagBits
type VkFenceCreateFlags = VkFenceCreateFlagBits

pattern VK_FENCE_CREATE_SIGNALED_BIT = VkFenceCreateFlagBits 0x1


-- ** vkCreateFence
foreign import ccall "vkCreateFence" vkCreateFence :: 
  VkDevice ->
  Ptr VkFenceCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult

newtype VkFence = VkFence Word64
  deriving (Eq, Storable)

