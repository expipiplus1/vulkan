{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.PipelineCache
  ( PipelineCache
  , PipelineCacheCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineCacheCreateInfo(..)
#endif
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
  )
import Data.ByteString
  ( ByteString
  , packCStringLen
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
  ( maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCacheCreateFlags(..)
  , VkPipelineCache
  , vkCreatePipelineCache
  , vkDestroyPipelineCache
  , vkGetPipelineCacheData
  , vkMergePipelineCaches
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "PipelineCache"
type PipelineCache = VkPipelineCache

-- No documentation found for TopLevel "PipelineCacheCreateFlags"
type PipelineCacheCreateFlags = VkPipelineCacheCreateFlags


-- No complete pragma for PipelineCacheCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineCacheCreateInfo"
data PipelineCacheCreateInfo = PipelineCacheCreateInfo
  { -- No documentation found for Nested "PipelineCacheCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineCacheCreateInfo" "flags"
  flags :: PipelineCacheCreateFlags
  , -- No documentation found for Nested "PipelineCacheCreateInfo" "pInitialData"
  initialData :: ByteString
  }
  deriving (Show, Eq)

instance Zero PipelineCacheCreateInfo where
  zero = PipelineCacheCreateInfo Nothing
                                 zero
                                 mempty

#endif


-- No documentation found for TopLevel "vkCreatePipelineCache"
createPipelineCache :: Device ->  PipelineCacheCreateInfo ->  Maybe AllocationCallbacks ->  IO (PipelineCache)
createPipelineCache = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyPipelineCache"
destroyPipelineCache :: Device ->  PipelineCache ->  Maybe AllocationCallbacks ->  IO ()
destroyPipelineCache = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetPipelineCacheData"
getNumPipelineCacheData :: Device ->  PipelineCache ->  IO (VkResult, CSize)
getNumPipelineCacheData = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPipelineCacheData"
getPipelineCacheData :: Device ->  PipelineCache ->  CSize ->  IO (VkResult, ByteString)
getPipelineCacheData = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPipelineCacheData'.
getAllPipelineCacheData :: Device ->  PipelineCache ->  IO (ByteString)
getAllPipelineCacheData device' pipelineCache' =
  snd <$> getNumPipelineCacheData device' pipelineCache'
    >>= \num -> snd <$> getPipelineCacheData device' pipelineCache' num



-- No documentation found for TopLevel "vkMergePipelineCaches"
mergePipelineCaches :: Device ->  PipelineCache ->  Vector PipelineCache ->  IO ()
mergePipelineCaches = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createPipelineCache' and 'destroyPipelineCache' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withPipelineCache
  :: Device -> PipelineCacheCreateInfo -> Maybe AllocationCallbacks -> (PipelineCache -> IO a) -> IO a
withPipelineCache device pipelineCacheCreateInfo allocationCallbacks = bracket
  (createPipelineCache device pipelineCacheCreateInfo allocationCallbacks)
  (\o -> destroyPipelineCache device o allocationCallbacks)
