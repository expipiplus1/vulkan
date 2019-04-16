{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable
  ( withCStructPhysicalDeviceDepthClipEnableFeaturesEXT
  , fromCStructPhysicalDeviceDepthClipEnableFeaturesEXT
  , PhysicalDeviceDepthClipEnableFeaturesEXT(..)
  , PipelineRasterizationDepthClipStateCreateFlagsEXT
  , withCStructPipelineRasterizationDepthClipStateCreateInfoEXT
  , fromCStructPipelineRasterizationDepthClipStateCreateInfoEXT
  , PipelineRasterizationDepthClipStateCreateInfoEXT(..)
  , pattern VK_EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION
  , pattern VK_EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_depth_clip_enable
  ( VkPhysicalDeviceDepthClipEnableFeaturesEXT(..)
  , VkPipelineRasterizationDepthClipStateCreateFlagsEXT(..)
  , VkPipelineRasterizationDepthClipStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_depth_clip_enable
  ( pattern VK_EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME
  , pattern VK_EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceDepthClipEnableFeaturesEXT"
data PhysicalDeviceDepthClipEnableFeaturesEXT = PhysicalDeviceDepthClipEnableFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceDepthClipEnableFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDepthClipEnableFeaturesEXT" "depthClipEnable"
  vkDepthClipEnable :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceDepthClipEnableFeaturesEXT :: PhysicalDeviceDepthClipEnableFeaturesEXT -> (VkPhysicalDeviceDepthClipEnableFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceDepthClipEnableFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceDepthClipEnableFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceDepthClipEnableFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT pPNext (boolToBool32 (vkDepthClipEnable (from :: PhysicalDeviceDepthClipEnableFeaturesEXT)))))
fromCStructPhysicalDeviceDepthClipEnableFeaturesEXT :: VkPhysicalDeviceDepthClipEnableFeaturesEXT -> IO PhysicalDeviceDepthClipEnableFeaturesEXT
fromCStructPhysicalDeviceDepthClipEnableFeaturesEXT c = PhysicalDeviceDepthClipEnableFeaturesEXT <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceDepthClipEnableFeaturesEXT)))
                                                                                                 <*> pure (bool32ToBool (vkDepthClipEnable (c :: VkPhysicalDeviceDepthClipEnableFeaturesEXT)))
-- No documentation found for TopLevel "PipelineRasterizationDepthClipStateCreateFlagsEXT"
type PipelineRasterizationDepthClipStateCreateFlagsEXT = VkPipelineRasterizationDepthClipStateCreateFlagsEXT
-- No documentation found for TopLevel "PipelineRasterizationDepthClipStateCreateInfoEXT"
data PipelineRasterizationDepthClipStateCreateInfoEXT = PipelineRasterizationDepthClipStateCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineRasterizationDepthClipStateCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationDepthClipStateCreateInfoEXT" "flags"
  vkFlags :: PipelineRasterizationDepthClipStateCreateFlagsEXT
  , -- No documentation found for Nested "PipelineRasterizationDepthClipStateCreateInfoEXT" "depthClipEnable"
  vkDepthClipEnable :: Bool
  }
  deriving (Show, Eq)
withCStructPipelineRasterizationDepthClipStateCreateInfoEXT :: PipelineRasterizationDepthClipStateCreateInfoEXT -> (VkPipelineRasterizationDepthClipStateCreateInfoEXT -> IO a) -> IO a
withCStructPipelineRasterizationDepthClipStateCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PipelineRasterizationDepthClipStateCreateInfoEXT)) (\pPNext -> cont (VkPipelineRasterizationDepthClipStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT pPNext (vkFlags (from :: PipelineRasterizationDepthClipStateCreateInfoEXT)) (boolToBool32 (vkDepthClipEnable (from :: PipelineRasterizationDepthClipStateCreateInfoEXT)))))
fromCStructPipelineRasterizationDepthClipStateCreateInfoEXT :: VkPipelineRasterizationDepthClipStateCreateInfoEXT -> IO PipelineRasterizationDepthClipStateCreateInfoEXT
fromCStructPipelineRasterizationDepthClipStateCreateInfoEXT c = PipelineRasterizationDepthClipStateCreateInfoEXT <$> -- Univalued Member elided
                                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineRasterizationDepthClipStateCreateInfoEXT)))
                                                                                                                 <*> pure (vkFlags (c :: VkPipelineRasterizationDepthClipStateCreateInfoEXT))
                                                                                                                 <*> pure (bool32ToBool (vkDepthClipEnable (c :: VkPipelineRasterizationDepthClipStateCreateInfoEXT)))
