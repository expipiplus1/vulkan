{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( withCStructBindBufferMemoryInfo
  , fromCStructBindBufferMemoryInfo
  , BindBufferMemoryInfo(..)
  , withCStructBindImageMemoryInfo
  , fromCStructBindImageMemoryInfo
  , BindImageMemoryInfo(..)
  , bindBufferMemory2
  , bindImageMemory2
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  , pattern VK_IMAGE_CREATE_ALIAS_BIT
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( bindBufferMemory2
  , bindImageMemory2
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , DeviceSize
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_ALIAS_BIT
  )


-- No documentation found for TopLevel "BindBufferMemoryInfo"
data BindBufferMemoryInfo = BindBufferMemoryInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "BindBufferMemoryInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindBufferMemoryInfo" "buffer"
  vkBuffer :: Buffer
  , -- No documentation found for Nested "BindBufferMemoryInfo" "memory"
  vkMemory :: DeviceMemory
  , -- No documentation found for Nested "BindBufferMemoryInfo" "memoryOffset"
  vkMemoryOffset :: DeviceSize
  }
  deriving (Show, Eq)
withCStructBindBufferMemoryInfo :: BindBufferMemoryInfo -> (VkBindBufferMemoryInfo -> IO a) -> IO a
withCStructBindBufferMemoryInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: BindBufferMemoryInfo)) (\pPNext -> cont (VkBindBufferMemoryInfo VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO pPNext (vkBuffer (from :: BindBufferMemoryInfo)) (vkMemory (from :: BindBufferMemoryInfo)) (vkMemoryOffset (from :: BindBufferMemoryInfo))))
fromCStructBindBufferMemoryInfo :: VkBindBufferMemoryInfo -> IO BindBufferMemoryInfo
fromCStructBindBufferMemoryInfo c = BindBufferMemoryInfo <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindBufferMemoryInfo)))
                                                         <*> pure (vkBuffer (c :: VkBindBufferMemoryInfo))
                                                         <*> pure (vkMemory (c :: VkBindBufferMemoryInfo))
                                                         <*> pure (vkMemoryOffset (c :: VkBindBufferMemoryInfo))
-- No documentation found for TopLevel "BindImageMemoryInfo"
data BindImageMemoryInfo = BindImageMemoryInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "BindImageMemoryInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindImageMemoryInfo" "image"
  vkImage :: Image
  , -- No documentation found for Nested "BindImageMemoryInfo" "memory"
  vkMemory :: DeviceMemory
  , -- No documentation found for Nested "BindImageMemoryInfo" "memoryOffset"
  vkMemoryOffset :: DeviceSize
  }
  deriving (Show, Eq)
withCStructBindImageMemoryInfo :: BindImageMemoryInfo -> (VkBindImageMemoryInfo -> IO a) -> IO a
withCStructBindImageMemoryInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: BindImageMemoryInfo)) (\pPNext -> cont (VkBindImageMemoryInfo VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO pPNext (vkImage (from :: BindImageMemoryInfo)) (vkMemory (from :: BindImageMemoryInfo)) (vkMemoryOffset (from :: BindImageMemoryInfo))))
fromCStructBindImageMemoryInfo :: VkBindImageMemoryInfo -> IO BindImageMemoryInfo
fromCStructBindImageMemoryInfo c = BindImageMemoryInfo <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindImageMemoryInfo)))
                                                       <*> pure (vkImage (c :: VkBindImageMemoryInfo))
                                                       <*> pure (vkMemory (c :: VkBindImageMemoryInfo))
                                                       <*> pure (vkMemoryOffset (c :: VkBindImageMemoryInfo))

-- | Wrapper for vkBindBufferMemory2
bindBufferMemory2 :: Device ->  Vector BindBufferMemoryInfo ->  IO ()
bindBufferMemory2 = \(Device device commandTable) -> \bindInfos -> withVec withCStructBindBufferMemoryInfo bindInfos (\pBindInfos -> Graphics.Vulkan.C.Dynamic.bindBufferMemory2 commandTable device (fromIntegral $ Data.Vector.length bindInfos) pBindInfos >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))

-- | Wrapper for vkBindImageMemory2
bindImageMemory2 :: Device ->  Vector BindImageMemoryInfo ->  IO ()
bindImageMemory2 = \(Device device commandTable) -> \bindInfos -> withVec withCStructBindImageMemoryInfo bindInfos (\pBindInfos -> Graphics.Vulkan.C.Dynamic.bindImageMemory2 commandTable device (fromIntegral $ Data.Vector.length bindInfos) pBindInfos >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))
