{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( withCStructBindBufferMemoryDeviceGroupInfo
  , fromCStructBindBufferMemoryDeviceGroupInfo
  , BindBufferMemoryDeviceGroupInfo(..)
  , withCStructBindImageMemoryDeviceGroupInfo
  , fromCStructBindImageMemoryDeviceGroupInfo
  , BindImageMemoryDeviceGroupInfo(..)
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( VkBindBufferMemoryDeviceGroupInfo(..)
  , VkBindImageMemoryDeviceGroupInfo(..)
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  , fromCStructRect2D
  , withCStructRect2D
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  )


-- No documentation found for TopLevel "BindBufferMemoryDeviceGroupInfo"
data BindBufferMemoryDeviceGroupInfo = BindBufferMemoryDeviceGroupInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "BindBufferMemoryDeviceGroupInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "BindBufferMemoryDeviceGroupInfo" "pDeviceIndices"
  vkPDeviceIndices :: Vector Word32
  }
  deriving (Show, Eq)
withCStructBindBufferMemoryDeviceGroupInfo :: BindBufferMemoryDeviceGroupInfo -> (VkBindBufferMemoryDeviceGroupInfo -> IO a) -> IO a
withCStructBindBufferMemoryDeviceGroupInfo from cont = withVec (&) (vkPDeviceIndices (from :: BindBufferMemoryDeviceGroupInfo)) (\pDeviceIndices -> maybeWith withSomeVkStruct (vkPNext (from :: BindBufferMemoryDeviceGroupInfo)) (\pPNext -> cont (VkBindBufferMemoryDeviceGroupInfo VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO pPNext (fromIntegral (Data.Vector.length (vkPDeviceIndices (from :: BindBufferMemoryDeviceGroupInfo)))) pDeviceIndices)))
fromCStructBindBufferMemoryDeviceGroupInfo :: VkBindBufferMemoryDeviceGroupInfo -> IO BindBufferMemoryDeviceGroupInfo
fromCStructBindBufferMemoryDeviceGroupInfo c = BindBufferMemoryDeviceGroupInfo <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindBufferMemoryDeviceGroupInfo)))
                                                                               -- Length valued member elided
                                                                               <*> (Data.Vector.generateM (fromIntegral (vkDeviceIndexCount (c :: VkBindBufferMemoryDeviceGroupInfo))) (peekElemOff (vkPDeviceIndices (c :: VkBindBufferMemoryDeviceGroupInfo))))
instance Zero BindBufferMemoryDeviceGroupInfo where
  zero = BindBufferMemoryDeviceGroupInfo Nothing
                                         Data.Vector.empty
-- No documentation found for TopLevel "BindImageMemoryDeviceGroupInfo"
data BindImageMemoryDeviceGroupInfo = BindImageMemoryDeviceGroupInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "BindImageMemoryDeviceGroupInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "BindImageMemoryDeviceGroupInfo" "pDeviceIndices"
  vkPDeviceIndices :: Vector Word32
  -- Length valued member elided
  , -- No documentation found for Nested "BindImageMemoryDeviceGroupInfo" "pSplitInstanceBindRegions"
  vkPSplitInstanceBindRegions :: Vector Rect2D
  }
  deriving (Show, Eq)
withCStructBindImageMemoryDeviceGroupInfo :: BindImageMemoryDeviceGroupInfo -> (VkBindImageMemoryDeviceGroupInfo -> IO a) -> IO a
withCStructBindImageMemoryDeviceGroupInfo from cont = withVec withCStructRect2D (vkPSplitInstanceBindRegions (from :: BindImageMemoryDeviceGroupInfo)) (\pSplitInstanceBindRegions -> withVec (&) (vkPDeviceIndices (from :: BindImageMemoryDeviceGroupInfo)) (\pDeviceIndices -> maybeWith withSomeVkStruct (vkPNext (from :: BindImageMemoryDeviceGroupInfo)) (\pPNext -> cont (VkBindImageMemoryDeviceGroupInfo VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO pPNext (fromIntegral (Data.Vector.length (vkPDeviceIndices (from :: BindImageMemoryDeviceGroupInfo)))) pDeviceIndices (fromIntegral (Data.Vector.length (vkPSplitInstanceBindRegions (from :: BindImageMemoryDeviceGroupInfo)))) pSplitInstanceBindRegions))))
fromCStructBindImageMemoryDeviceGroupInfo :: VkBindImageMemoryDeviceGroupInfo -> IO BindImageMemoryDeviceGroupInfo
fromCStructBindImageMemoryDeviceGroupInfo c = BindImageMemoryDeviceGroupInfo <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindImageMemoryDeviceGroupInfo)))
                                                                             -- Length valued member elided
                                                                             <*> (Data.Vector.generateM (fromIntegral (vkDeviceIndexCount (c :: VkBindImageMemoryDeviceGroupInfo))) (peekElemOff (vkPDeviceIndices (c :: VkBindImageMemoryDeviceGroupInfo))))
                                                                             -- Length valued member elided
                                                                             <*> (Data.Vector.generateM (fromIntegral (vkSplitInstanceBindRegionCount (c :: VkBindImageMemoryDeviceGroupInfo))) (((fromCStructRect2D <=<) . peekElemOff) (vkPSplitInstanceBindRegions (c :: VkBindImageMemoryDeviceGroupInfo))))
instance Zero BindImageMemoryDeviceGroupInfo where
  zero = BindImageMemoryDeviceGroupInfo Nothing
                                        Data.Vector.empty
                                        Data.Vector.empty
