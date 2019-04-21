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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
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



-- | VkPhysicalDeviceDepthClipEnableFeaturesEXT - Structure indicating
-- support for explicit enable of depth clip
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_depth_clip_enable.VkPhysicalDeviceDepthClipEnableFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_depth_clip_enable.VkPhysicalDeviceDepthClipEnableFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_depth_clip_enable.VkPhysicalDeviceDepthClipEnableFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable this
-- feature.
--
-- Unresolved directive in VkPhysicalDeviceDepthClipEnableFeaturesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceDepthClipEnableFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceDepthClipEnableFeaturesEXT = PhysicalDeviceDepthClipEnableFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceDepthClipEnableFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDepthClipEnableFeaturesEXT" "depthClipEnable"
  depthClipEnable :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceDepthClipEnableFeaturesEXT' and
-- marshal a 'PhysicalDeviceDepthClipEnableFeaturesEXT' into it. The 'VkPhysicalDeviceDepthClipEnableFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceDepthClipEnableFeaturesEXT :: PhysicalDeviceDepthClipEnableFeaturesEXT -> (VkPhysicalDeviceDepthClipEnableFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceDepthClipEnableFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceDepthClipEnableFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceDepthClipEnableFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT pPNext (boolToBool32 (depthClipEnable (marshalled :: PhysicalDeviceDepthClipEnableFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceDepthClipEnableFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceDepthClipEnableFeaturesEXT'.
fromCStructPhysicalDeviceDepthClipEnableFeaturesEXT :: VkPhysicalDeviceDepthClipEnableFeaturesEXT -> IO PhysicalDeviceDepthClipEnableFeaturesEXT
fromCStructPhysicalDeviceDepthClipEnableFeaturesEXT c = PhysicalDeviceDepthClipEnableFeaturesEXT <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceDepthClipEnableFeaturesEXT)))
                                                                                                 <*> pure (bool32ToBool (vkDepthClipEnable (c :: VkPhysicalDeviceDepthClipEnableFeaturesEXT)))

instance Zero PhysicalDeviceDepthClipEnableFeaturesEXT where
  zero = PhysicalDeviceDepthClipEnableFeaturesEXT Nothing
                                                  False


-- | VkPipelineRasterizationDepthClipStateCreateFlagsEXT - Reserved for
-- future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_depth_clip_enable.VkPipelineRasterizationDepthClipStateCreateFlagsEXT'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- No cross-references are available
type PipelineRasterizationDepthClipStateCreateFlagsEXT = VkPipelineRasterizationDepthClipStateCreateFlagsEXT


-- | VkPipelineRasterizationDepthClipStateCreateInfoEXT - Structure
-- specifying depth clipping state
--
-- = Description
--
-- Unresolved directive in
-- VkPipelineRasterizationDepthClipStateCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkPipelineRasterizationDepthClipStateCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PipelineRasterizationDepthClipStateCreateInfoEXT = PipelineRasterizationDepthClipStateCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineRasterizationDepthClipStateCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationDepthClipStateCreateInfoEXT" "flags"
  flags :: PipelineRasterizationDepthClipStateCreateFlagsEXT
  , -- No documentation found for Nested "PipelineRasterizationDepthClipStateCreateInfoEXT" "depthClipEnable"
  depthClipEnable :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineRasterizationDepthClipStateCreateInfoEXT' and
-- marshal a 'PipelineRasterizationDepthClipStateCreateInfoEXT' into it. The 'VkPipelineRasterizationDepthClipStateCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineRasterizationDepthClipStateCreateInfoEXT :: PipelineRasterizationDepthClipStateCreateInfoEXT -> (VkPipelineRasterizationDepthClipStateCreateInfoEXT -> IO a) -> IO a
withCStructPipelineRasterizationDepthClipStateCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PipelineRasterizationDepthClipStateCreateInfoEXT)) (\pPNext -> cont (VkPipelineRasterizationDepthClipStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT pPNext (flags (marshalled :: PipelineRasterizationDepthClipStateCreateInfoEXT)) (boolToBool32 (depthClipEnable (marshalled :: PipelineRasterizationDepthClipStateCreateInfoEXT)))))

-- | A function to read a 'VkPipelineRasterizationDepthClipStateCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'PipelineRasterizationDepthClipStateCreateInfoEXT'.
fromCStructPipelineRasterizationDepthClipStateCreateInfoEXT :: VkPipelineRasterizationDepthClipStateCreateInfoEXT -> IO PipelineRasterizationDepthClipStateCreateInfoEXT
fromCStructPipelineRasterizationDepthClipStateCreateInfoEXT c = PipelineRasterizationDepthClipStateCreateInfoEXT <$> -- Univalued Member elided
                                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineRasterizationDepthClipStateCreateInfoEXT)))
                                                                                                                 <*> pure (vkFlags (c :: VkPipelineRasterizationDepthClipStateCreateInfoEXT))
                                                                                                                 <*> pure (bool32ToBool (vkDepthClipEnable (c :: VkPipelineRasterizationDepthClipStateCreateInfoEXT)))

instance Zero PipelineRasterizationDepthClipStateCreateInfoEXT where
  zero = PipelineRasterizationDepthClipStateCreateInfoEXT Nothing
                                                          zero
                                                          False

