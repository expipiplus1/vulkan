{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.PipelineCache where

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
import Foreign.C.Types( CSize
                      , CSize(..)
                      )

-- ** vkCreatePipelineCache
foreign import ccall "vkCreatePipelineCache" vkCreatePipelineCache ::
  VkDevice ->
  Ptr VkPipelineCacheCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkPipelineCache -> IO VkResult

-- ** vkGetPipelineCacheData
foreign import ccall "vkGetPipelineCacheData" vkGetPipelineCacheData ::
  VkDevice -> VkPipelineCache -> Ptr CSize -> Ptr Void -> IO VkResult

newtype VkPipelineCache = VkPipelineCache Word64
  deriving (Eq, Ord, Storable)


data VkPipelineCacheCreateInfo =
  VkPipelineCacheCreateInfo{ vkSType :: VkStructureType 
                           , vkPNext :: Ptr Void 
                           , vkFlags :: VkPipelineCacheCreateFlags 
                           , vkInitialDataSize :: CSize 
                           , vkPInitialData :: Ptr Void 
                           }
  deriving (Eq, Ord)

instance Storable VkPipelineCacheCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPipelineCacheCreateInfo <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkInitialDataSize (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPInitialData (poked :: VkPipelineCacheCreateInfo))


-- ** vkMergePipelineCaches
foreign import ccall "vkMergePipelineCaches" vkMergePipelineCaches ::
  VkDevice ->
  VkPipelineCache -> Word32 -> Ptr VkPipelineCache -> IO VkResult

-- ** VkPipelineCacheCreateFlags
-- | Opaque flag
newtype VkPipelineCacheCreateFlags = VkPipelineCacheCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- ** vkDestroyPipelineCache
foreign import ccall "vkDestroyPipelineCache" vkDestroyPipelineCache ::
  VkDevice -> VkPipelineCache -> Ptr VkAllocationCallbacks -> IO ()

