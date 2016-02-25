{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.CommandPool where
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
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

data VkCommandPoolCreateInfo =
  VkCommandPoolCreateInfo{ vkSType :: VkStructureType 
                         , vkPNext :: Ptr Void 
                         , vkFlags :: VkCommandPoolCreateFlags 
                         , vkQueueFamilyIndex :: Word32 
                         }
  deriving (Eq)

instance Storable VkCommandPoolCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkCommandPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueFamilyIndex (poked :: VkCommandPoolCreateInfo))


-- ** vkDestroyCommandPool
foreign import ccall "vkDestroyCommandPool" vkDestroyCommandPool :: 
  VkDevice -> VkCommandPool -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkResetCommandPool
foreign import ccall "vkResetCommandPool" vkResetCommandPool :: 
  VkDevice -> VkCommandPool -> VkCommandPoolResetFlags -> IO VkResult

-- ** VkCommandPoolCreateFlags

newtype VkCommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkCommandPoolCreateFlagBits
type VkCommandPoolCreateFlags = VkCommandPoolCreateFlagBits
-- | Command buffers have a short lifetime
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = VkCommandPoolCreateFlagBits 0x1
-- | Command buffers may release their memory individually
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = VkCommandPoolCreateFlagBits 0x2


-- ** vkCreateCommandPool
foreign import ccall "vkCreateCommandPool" vkCreateCommandPool :: 
  VkDevice ->
  Ptr VkCommandPoolCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkCommandPool -> IO VkResult

-- ** VkCommandPoolResetFlags

newtype VkCommandPoolResetFlagBits = VkCommandPoolResetFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkCommandPoolResetFlagBits
type VkCommandPoolResetFlags = VkCommandPoolResetFlagBits
-- | Release resources owned by the pool
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = VkCommandPoolResetFlagBits 0x1


newtype VkCommandPool = VkCommandPool Word64
  deriving (Eq, Storable)

