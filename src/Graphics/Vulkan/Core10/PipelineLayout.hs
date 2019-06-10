{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.PipelineLayout
  ( DescriptorSetLayout
  , PipelineLayoutCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineLayoutCreateInfo(..)
#endif
  , PushConstantRange(..)
  , ShaderStageFlags
  , createPipelineLayout
  , destroyPipelineLayout
  , withPipelineLayout
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkPipelineLayoutCreateFlags(..)
  , VkDescriptorSetLayout
  , vkCreatePipelineLayout
  , vkDestroyPipelineLayout
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( PipelineLayout
  , ShaderStageFlagBits
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "DescriptorSetLayout"
type DescriptorSetLayout = VkDescriptorSetLayout

-- No documentation found for TopLevel "PipelineLayoutCreateFlags"
type PipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags


-- No complete pragma for PipelineLayoutCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineLayoutCreateInfo"
data PipelineLayoutCreateInfo = PipelineLayoutCreateInfo
  { -- No documentation found for Nested "PipelineLayoutCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineLayoutCreateInfo" "flags"
  flags :: PipelineLayoutCreateFlags
  , -- No documentation found for Nested "PipelineLayoutCreateInfo" "pSetLayouts"
  setLayouts :: Vector DescriptorSetLayout
  , -- No documentation found for Nested "PipelineLayoutCreateInfo" "pPushConstantRanges"
  pushConstantRanges :: Vector PushConstantRange
  }
  deriving (Show, Eq)

instance Zero PipelineLayoutCreateInfo where
  zero = PipelineLayoutCreateInfo Nothing
                                  zero
                                  mempty
                                  mempty

#endif


-- No documentation found for TopLevel "VkPushConstantRange"
data PushConstantRange = PushConstantRange
  { -- No documentation found for Nested "PushConstantRange" "stageFlags"
  stageFlags :: ShaderStageFlags
  , -- No documentation found for Nested "PushConstantRange" "offset"
  offset :: Word32
  , -- No documentation found for Nested "PushConstantRange" "size"
  size :: Word32
  }
  deriving (Show, Eq)

instance Zero PushConstantRange where
  zero = PushConstantRange zero
                           zero
                           zero


-- No documentation found for TopLevel "ShaderStageFlags"
type ShaderStageFlags = ShaderStageFlagBits


-- No documentation found for TopLevel "vkCreatePipelineLayout"
createPipelineLayout :: Device ->  PipelineLayoutCreateInfo ->  Maybe AllocationCallbacks ->  IO (PipelineLayout)
createPipelineLayout = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyPipelineLayout"
destroyPipelineLayout :: Device ->  PipelineLayout ->  Maybe AllocationCallbacks ->  IO ()
destroyPipelineLayout = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createPipelineLayout' and 'destroyPipelineLayout' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withPipelineLayout
  :: Device -> PipelineLayoutCreateInfo -> Maybe AllocationCallbacks -> (PipelineLayout -> IO a) -> IO a
withPipelineLayout device pipelineLayoutCreateInfo allocationCallbacks = bracket
  (createPipelineLayout device pipelineLayoutCreateInfo allocationCallbacks)
  (\o -> destroyPipelineLayout device o allocationCallbacks)
