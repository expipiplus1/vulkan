{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles
  ( DiscardRectangleModeEXT
  , pattern DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT
  , pattern DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDeviceDiscardRectanglePropertiesEXT(..)
#endif
  , PipelineDiscardRectangleStateCreateFlagsEXT
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineDiscardRectangleStateCreateInfoEXT(..)
#endif
  , cmdSetDiscardRectangleEXT
  , pattern EXT_DISCARD_RECTANGLES_EXTENSION_NAME
  , pattern EXT_DISCARD_RECTANGLES_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
  , pattern DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Data.Word
  ( Word32
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( VkDiscardRectangleModeEXT(..)
  , VkPipelineDiscardRectangleStateCreateFlagsEXT(..)
  , vkCmdSetDiscardRectangleEXT
  , pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT
  , pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT
  , pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME
  , pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.Pipeline
  ( pattern DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
  )


-- No documentation found for TopLevel "DiscardRectangleModeEXT"
type DiscardRectangleModeEXT = VkDiscardRectangleModeEXT


{-# complete DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT, DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT :: DiscardRectangleModeEXT #-}


-- No documentation found for Nested "DiscardRectangleModeEXT" "DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT"
pattern DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT :: (a ~ DiscardRectangleModeEXT) => a
pattern DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT = VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT


-- No documentation found for Nested "DiscardRectangleModeEXT" "DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT"
pattern DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT :: (a ~ DiscardRectangleModeEXT) => a
pattern DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT = VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceDiscardRectanglePropertiesEXT"
data PhysicalDeviceDiscardRectanglePropertiesEXT = PhysicalDeviceDiscardRectanglePropertiesEXT
  { -- No documentation found for Nested "PhysicalDeviceDiscardRectanglePropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDiscardRectanglePropertiesEXT" "maxDiscardRectangles"
  maxDiscardRectangles :: Word32
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceDiscardRectanglePropertiesEXT where
  zero = PhysicalDeviceDiscardRectanglePropertiesEXT Nothing
                                                     zero

#endif

-- No documentation found for TopLevel "PipelineDiscardRectangleStateCreateFlagsEXT"
type PipelineDiscardRectangleStateCreateFlagsEXT = VkPipelineDiscardRectangleStateCreateFlagsEXT


-- No complete pragma for PipelineDiscardRectangleStateCreateFlagsEXT as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineDiscardRectangleStateCreateInfoEXT"
data PipelineDiscardRectangleStateCreateInfoEXT = PipelineDiscardRectangleStateCreateInfoEXT
  { -- No documentation found for Nested "PipelineDiscardRectangleStateCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineDiscardRectangleStateCreateInfoEXT" "flags"
  flags :: PipelineDiscardRectangleStateCreateFlagsEXT
  , -- No documentation found for Nested "PipelineDiscardRectangleStateCreateInfoEXT" "discardRectangleMode"
  discardRectangleMode :: DiscardRectangleModeEXT
  , -- No documentation found for Nested "PipelineDiscardRectangleStateCreateInfoEXT" "pDiscardRectangles"
  discardRectangles :: Either Word32 (Vector Rect2D)
  }
  deriving (Show, Eq)

instance Zero PipelineDiscardRectangleStateCreateInfoEXT where
  zero = PipelineDiscardRectangleStateCreateInfoEXT Nothing
                                                    zero
                                                    zero
                                                    (Left 0)

#endif


-- No documentation found for TopLevel "vkCmdSetDiscardRectangleEXT"
cmdSetDiscardRectangleEXT :: CommandBuffer ->  Word32 ->  Vector Rect2D ->  IO ()
cmdSetDiscardRectangleEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME"
pattern EXT_DISCARD_RECTANGLES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_DISCARD_RECTANGLES_EXTENSION_NAME = VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION"
pattern EXT_DISCARD_RECTANGLES_SPEC_VERSION :: Integral a => a
pattern EXT_DISCARD_RECTANGLES_SPEC_VERSION = VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION
