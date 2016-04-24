{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.PipelineCache where

import Graphics.Vulkan.Device( Device(..)
                             )
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Graphics.Vulkan.Core( StructureType(..)
                           , Result(..)
                           , Flags(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

-- ** vkCreatePipelineCache
foreign import ccall "vkCreatePipelineCache" vkCreatePipelineCache ::
  Device ->
  Ptr PipelineCacheCreateInfo ->
    Ptr AllocationCallbacks -> Ptr PipelineCache -> IO Result

-- ** vkGetPipelineCacheData
foreign import ccall "vkGetPipelineCacheData" vkGetPipelineCacheData ::
  Device -> PipelineCache -> Ptr CSize -> Ptr Void -> IO Result

newtype PipelineCache = PipelineCache Word64
  deriving (Eq, Storable)


data PipelineCacheCreateInfo =
  PipelineCacheCreateInfo{ sType :: StructureType 
                         , pNext :: Ptr Void 
                         , flags :: PipelineCacheCreateFlags 
                         , initialDataSize :: CSize 
                         , pInitialData :: Ptr Void 
                         }
  deriving (Eq)

instance Storable PipelineCacheCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = PipelineCacheCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: PipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 24) (initialDataSize (poked :: PipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 32) (pInitialData (poked :: PipelineCacheCreateInfo))


-- ** vkMergePipelineCaches
foreign import ccall "vkMergePipelineCaches" vkMergePipelineCaches ::
  Device -> PipelineCache -> Word32 -> Ptr PipelineCache -> IO Result

-- ** PipelineCacheCreateFlags
-- | Opaque flag
newtype PipelineCacheCreateFlags = PipelineCacheCreateFlags Flags
  deriving (Eq, Storable)

-- ** vkDestroyPipelineCache
foreign import ccall "vkDestroyPipelineCache" vkDestroyPipelineCache ::
  Device -> PipelineCache -> Ptr AllocationCallbacks -> IO ()

