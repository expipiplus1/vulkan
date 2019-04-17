{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.PipelineCache
  ( PipelineCache
  , PipelineCacheCreateFlags
  , withCStructPipelineCacheCreateInfo
  , fromCStructPipelineCacheCreateInfo
  , PipelineCacheCreateInfo(..)
  , createPipelineCache
  , destroyPipelineCache
  , getNumPipelineCacheData
  , getPipelineCacheData
  , getAllPipelineCacheData
  , mergePipelineCaches
  , withPipelineCache
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Data.ByteString
  ( ByteString
  , packCStringLen
  )
import qualified Data.ByteString
  ( empty
  , length
  )
import Data.ByteString.Unsafe
  ( unsafeUseAsCString
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createPipelineCache
  , destroyPipelineCache
  , getPipelineCacheData
  , mergePipelineCaches
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCacheCreateFlags(..)
  , VkPipelineCacheCreateInfo(..)
  , VkPipelineCache
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "PipelineCache"
type PipelineCache = VkPipelineCache
-- No documentation found for TopLevel "PipelineCacheCreateFlags"
type PipelineCacheCreateFlags = VkPipelineCacheCreateFlags
-- No documentation found for TopLevel "PipelineCacheCreateInfo"
data PipelineCacheCreateInfo = PipelineCacheCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineCacheCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineCacheCreateInfo" "flags"
  vkFlags :: PipelineCacheCreateFlags
  -- Bytestring length valued member elided
  , -- No documentation found for Nested "PipelineCacheCreateInfo" "pInitialData"
  vkPInitialData :: ByteString
  }
  deriving (Show, Eq)
withCStructPipelineCacheCreateInfo :: PipelineCacheCreateInfo -> (VkPipelineCacheCreateInfo -> IO a) -> IO a
withCStructPipelineCacheCreateInfo from cont = unsafeUseAsCString (vkPInitialData (from :: PipelineCacheCreateInfo)) (\pInitialData -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineCacheCreateInfo)) (\pPNext -> cont (VkPipelineCacheCreateInfo VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO pPNext (vkFlags (from :: PipelineCacheCreateInfo)) (fromIntegral (Data.ByteString.length (vkPInitialData (from :: PipelineCacheCreateInfo)))) (castPtr pInitialData))))
fromCStructPipelineCacheCreateInfo :: VkPipelineCacheCreateInfo -> IO PipelineCacheCreateInfo
fromCStructPipelineCacheCreateInfo c = PipelineCacheCreateInfo <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineCacheCreateInfo)))
                                                               <*> pure (vkFlags (c :: VkPipelineCacheCreateInfo))
                                                               -- Bytestring length valued member elided
                                                               <*> packCStringLen (castPtr (vkPInitialData (c :: VkPipelineCacheCreateInfo)), fromIntegral (vkInitialDataSize (c :: VkPipelineCacheCreateInfo)))
instance Zero PipelineCacheCreateInfo where
  zero = PipelineCacheCreateInfo Nothing
                                 zero
                                 Data.ByteString.empty

-- | Wrapper for 'vkCreatePipelineCache'
createPipelineCache :: Device ->  PipelineCacheCreateInfo ->  Maybe AllocationCallbacks ->  IO (PipelineCache)
createPipelineCache = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pPipelineCache -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructPipelineCacheCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createPipelineCache commandTable device pCreateInfo pAllocator pPipelineCache >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pPipelineCache)))))

-- | Wrapper for 'vkDestroyPipelineCache'
destroyPipelineCache :: Device ->  PipelineCache ->  Maybe AllocationCallbacks ->  IO ()
destroyPipelineCache = \(Device device commandTable) -> \pipelineCache -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyPipelineCache commandTable device pipelineCache pAllocator *> (pure ()))

-- | Wrapper for 'vkGetPipelineCacheData'
getNumPipelineCacheData :: Device ->  PipelineCache ->  IO (VkResult, CSize)
getNumPipelineCacheData = \(Device device commandTable) -> \pipelineCache -> alloca (\pDataSize -> Graphics.Vulkan.C.Dynamic.getPipelineCacheData commandTable device pipelineCache pDataSize nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pDataSize)))

-- | Wrapper for 'vkGetPipelineCacheData'
getPipelineCacheData :: Device ->  PipelineCache ->  CSize ->  IO (VkResult, ByteString)
getPipelineCacheData = \(Device device commandTable) -> \pipelineCache -> \dataSize -> allocaArray (fromIntegral dataSize) (\pData -> with dataSize (\pDataSize -> Graphics.Vulkan.C.Dynamic.getPipelineCacheData commandTable device pipelineCache pDataSize (castPtr pData) >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(curry packCStringLen pData =<< (fromIntegral <$> (peek pDataSize)))))))
-- | Call 'getNumPipelineCacheData' to get the number of return values, then use that
-- number to call 'getPipelineCacheData' to get all the values.
getAllPipelineCacheData :: Device ->  PipelineCache ->  IO (ByteString)
getAllPipelineCacheData device pipelineCache =
  snd <$> getNumPipelineCacheData device pipelineCache
    >>= \num -> snd <$> getPipelineCacheData device pipelineCache num


-- | Wrapper for 'vkMergePipelineCaches'
mergePipelineCaches :: Device ->  PipelineCache ->  Vector PipelineCache ->  IO ()
mergePipelineCaches = \(Device device commandTable) -> \dstCache -> \srcCaches -> withVec (&) srcCaches (\pSrcCaches -> Graphics.Vulkan.C.Dynamic.mergePipelineCaches commandTable device dstCache (fromIntegral $ Data.Vector.length srcCaches) pSrcCaches >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))
-- | Wrapper for 'createPipelineCache' and 'destroyPipelineCache' using 'bracket'
withPipelineCache
  :: Device -> PipelineCacheCreateInfo -> Maybe (AllocationCallbacks) -> (PipelineCache -> IO a) -> IO a
withPipelineCache device pipelineCacheCreateInfo allocationCallbacks = bracket
  (createPipelineCache device pipelineCacheCreateInfo allocationCallbacks)
  (\o -> destroyPipelineCache device o allocationCallbacks)
