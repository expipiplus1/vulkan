{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.PipelineLayout
  ( DescriptorSetLayout
  , PipelineLayoutCreateFlags
  , withCStructPipelineLayoutCreateInfo
  , fromCStructPipelineLayoutCreateInfo
  , PipelineLayoutCreateInfo(..)
  , withCStructPushConstantRange
  , fromCStructPushConstantRange
  , PushConstantRange(..)
  , ShaderStageFlags
  , createPipelineLayout
  , destroyPipelineLayout
  , withPipelineLayout
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
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
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createPipelineLayout
  , destroyPipelineLayout
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkPipelineLayoutCreateFlags(..)
  , VkPipelineLayoutCreateInfo(..)
  , VkPushConstantRange(..)
  , VkDescriptorSetLayout
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Pipeline
  ( PipelineLayout
  , ShaderStageFlagBits
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


-- No documentation found for TopLevel "DescriptorSetLayout"
type DescriptorSetLayout = VkDescriptorSetLayout
-- No documentation found for TopLevel "PipelineLayoutCreateFlags"
type PipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags
-- No documentation found for TopLevel "PipelineLayoutCreateInfo"
data PipelineLayoutCreateInfo = PipelineLayoutCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineLayoutCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineLayoutCreateInfo" "flags"
  vkFlags :: PipelineLayoutCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineLayoutCreateInfo" "pSetLayouts"
  vkPSetLayouts :: Vector DescriptorSetLayout
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineLayoutCreateInfo" "pPushConstantRanges"
  vkPPushConstantRanges :: Vector PushConstantRange
  }
  deriving (Show, Eq)
withCStructPipelineLayoutCreateInfo :: PipelineLayoutCreateInfo -> (VkPipelineLayoutCreateInfo -> IO a) -> IO a
withCStructPipelineLayoutCreateInfo from cont = withVec withCStructPushConstantRange (vkPPushConstantRanges (from :: PipelineLayoutCreateInfo)) (\pPushConstantRanges -> withVec (&) (vkPSetLayouts (from :: PipelineLayoutCreateInfo)) (\pSetLayouts -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineLayoutCreateInfo)) (\pPNext -> cont (VkPipelineLayoutCreateInfo VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO pPNext (vkFlags (from :: PipelineLayoutCreateInfo)) (fromIntegral (Data.Vector.length (vkPSetLayouts (from :: PipelineLayoutCreateInfo)))) pSetLayouts (fromIntegral (Data.Vector.length (vkPPushConstantRanges (from :: PipelineLayoutCreateInfo)))) pPushConstantRanges))))
fromCStructPipelineLayoutCreateInfo :: VkPipelineLayoutCreateInfo -> IO PipelineLayoutCreateInfo
fromCStructPipelineLayoutCreateInfo c = PipelineLayoutCreateInfo <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineLayoutCreateInfo)))
                                                                 <*> pure (vkFlags (c :: VkPipelineLayoutCreateInfo))
                                                                 -- Length valued member elided
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkSetLayoutCount (c :: VkPipelineLayoutCreateInfo))) (peekElemOff (vkPSetLayouts (c :: VkPipelineLayoutCreateInfo))))
                                                                 -- Length valued member elided
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkPushConstantRangeCount (c :: VkPipelineLayoutCreateInfo))) (((fromCStructPushConstantRange <=<) . peekElemOff) (vkPPushConstantRanges (c :: VkPipelineLayoutCreateInfo))))
instance Zero PipelineLayoutCreateInfo where
  zero = PipelineLayoutCreateInfo Nothing
                                  zero
                                  Data.Vector.empty
                                  Data.Vector.empty
-- No documentation found for TopLevel "PushConstantRange"
data PushConstantRange = PushConstantRange
  { -- No documentation found for Nested "PushConstantRange" "stageFlags"
  vkStageFlags :: ShaderStageFlags
  , -- No documentation found for Nested "PushConstantRange" "offset"
  vkOffset :: Word32
  , -- No documentation found for Nested "PushConstantRange" "size"
  vkSize :: Word32
  }
  deriving (Show, Eq)
withCStructPushConstantRange :: PushConstantRange -> (VkPushConstantRange -> IO a) -> IO a
withCStructPushConstantRange from cont = cont (VkPushConstantRange (vkStageFlags (from :: PushConstantRange)) (vkOffset (from :: PushConstantRange)) (vkSize (from :: PushConstantRange)))
fromCStructPushConstantRange :: VkPushConstantRange -> IO PushConstantRange
fromCStructPushConstantRange c = PushConstantRange <$> pure (vkStageFlags (c :: VkPushConstantRange))
                                                   <*> pure (vkOffset (c :: VkPushConstantRange))
                                                   <*> pure (vkSize (c :: VkPushConstantRange))
instance Zero PushConstantRange where
  zero = PushConstantRange zero
                           zero
                           zero
-- No documentation found for TopLevel "ShaderStageFlags"
type ShaderStageFlags = ShaderStageFlagBits

-- | Wrapper for 'vkCreatePipelineLayout'
createPipelineLayout :: Device ->  PipelineLayoutCreateInfo ->  Maybe AllocationCallbacks ->  IO (PipelineLayout)
createPipelineLayout = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pPipelineLayout -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructPipelineLayoutCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createPipelineLayout commandTable device pCreateInfo pAllocator pPipelineLayout >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pPipelineLayout)))))

-- | Wrapper for 'vkDestroyPipelineLayout'
destroyPipelineLayout :: Device ->  PipelineLayout ->  Maybe AllocationCallbacks ->  IO ()
destroyPipelineLayout = \(Device device commandTable) -> \pipelineLayout -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyPipelineLayout commandTable device pipelineLayout pAllocator *> (pure ()))
-- | Wrapper for 'createPipelineLayout' and 'destroyPipelineLayout' using 'bracket'
withPipelineLayout
  :: Device -> PipelineLayoutCreateInfo -> Maybe (AllocationCallbacks) -> (PipelineLayout -> IO a) -> IO a
withPipelineLayout device pipelineLayoutCreateInfo allocationCallbacks = bracket
  (createPipelineLayout device pipelineLayoutCreateInfo allocationCallbacks)
  (\o -> destroyPipelineLayout device o allocationCallbacks)
