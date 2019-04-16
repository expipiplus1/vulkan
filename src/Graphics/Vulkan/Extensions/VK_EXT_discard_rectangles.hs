{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles
  ( DiscardRectangleModeEXT
  , withCStructPhysicalDeviceDiscardRectanglePropertiesEXT
  , fromCStructPhysicalDeviceDiscardRectanglePropertiesEXT
  , PhysicalDeviceDiscardRectanglePropertiesEXT(..)
  , PipelineDiscardRectangleStateCreateFlagsEXT
  , withCStructPipelineDiscardRectangleStateCreateInfoEXT
  , fromCStructPipelineDiscardRectangleStateCreateInfoEXT
  , PipelineDiscardRectangleStateCreateInfoEXT(..)
  , cmdSetDiscardRectangleEXT
  , pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION
  , pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
  , pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Maybe
  ( maybe
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
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
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdSetDiscardRectangleEXT
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( VkDiscardRectangleModeEXT(..)
  , VkPhysicalDeviceDiscardRectanglePropertiesEXT(..)
  , VkPipelineDiscardRectangleStateCreateFlagsEXT(..)
  , VkPipelineDiscardRectangleStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  , fromCStructRect2D
  , withCStructRect2D
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
  , pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME
  , pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION
  )


-- No documentation found for TopLevel "DiscardRectangleModeEXT"
type DiscardRectangleModeEXT = VkDiscardRectangleModeEXT
-- No documentation found for TopLevel "PhysicalDeviceDiscardRectanglePropertiesEXT"
data PhysicalDeviceDiscardRectanglePropertiesEXT = PhysicalDeviceDiscardRectanglePropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceDiscardRectanglePropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDiscardRectanglePropertiesEXT" "maxDiscardRectangles"
  vkMaxDiscardRectangles :: Word32
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceDiscardRectanglePropertiesEXT :: PhysicalDeviceDiscardRectanglePropertiesEXT -> (VkPhysicalDeviceDiscardRectanglePropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceDiscardRectanglePropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceDiscardRectanglePropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceDiscardRectanglePropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT pPNext (vkMaxDiscardRectangles (from :: PhysicalDeviceDiscardRectanglePropertiesEXT))))
fromCStructPhysicalDeviceDiscardRectanglePropertiesEXT :: VkPhysicalDeviceDiscardRectanglePropertiesEXT -> IO PhysicalDeviceDiscardRectanglePropertiesEXT
fromCStructPhysicalDeviceDiscardRectanglePropertiesEXT c = PhysicalDeviceDiscardRectanglePropertiesEXT <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceDiscardRectanglePropertiesEXT)))
                                                                                                       <*> pure (vkMaxDiscardRectangles (c :: VkPhysicalDeviceDiscardRectanglePropertiesEXT))
-- No documentation found for TopLevel "PipelineDiscardRectangleStateCreateFlagsEXT"
type PipelineDiscardRectangleStateCreateFlagsEXT = VkPipelineDiscardRectangleStateCreateFlagsEXT
-- No documentation found for TopLevel "PipelineDiscardRectangleStateCreateInfoEXT"
data PipelineDiscardRectangleStateCreateInfoEXT = PipelineDiscardRectangleStateCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineDiscardRectangleStateCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineDiscardRectangleStateCreateInfoEXT" "flags"
  vkFlags :: PipelineDiscardRectangleStateCreateFlagsEXT
  , -- No documentation found for Nested "PipelineDiscardRectangleStateCreateInfoEXT" "discardRectangleMode"
  vkDiscardRectangleMode :: DiscardRectangleModeEXT
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineDiscardRectangleStateCreateInfoEXT" "pDiscardRectangles"
  vkPDiscardRectangles :: Maybe (Vector Rect2D)
  }
  deriving (Show, Eq)
withCStructPipelineDiscardRectangleStateCreateInfoEXT :: PipelineDiscardRectangleStateCreateInfoEXT -> (VkPipelineDiscardRectangleStateCreateInfoEXT -> IO a) -> IO a
withCStructPipelineDiscardRectangleStateCreateInfoEXT from cont = maybeWith (withVec withCStructRect2D) (vkPDiscardRectangles (from :: PipelineDiscardRectangleStateCreateInfoEXT)) (\pDiscardRectangles -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineDiscardRectangleStateCreateInfoEXT)) (\pPNext -> cont (VkPipelineDiscardRectangleStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT pPNext (vkFlags (from :: PipelineDiscardRectangleStateCreateInfoEXT)) (vkDiscardRectangleMode (from :: PipelineDiscardRectangleStateCreateInfoEXT)) (maybe 0 (fromIntegral . Data.Vector.length) (vkPDiscardRectangles (from :: PipelineDiscardRectangleStateCreateInfoEXT))) pDiscardRectangles)))
fromCStructPipelineDiscardRectangleStateCreateInfoEXT :: VkPipelineDiscardRectangleStateCreateInfoEXT -> IO PipelineDiscardRectangleStateCreateInfoEXT
fromCStructPipelineDiscardRectangleStateCreateInfoEXT c = PipelineDiscardRectangleStateCreateInfoEXT <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineDiscardRectangleStateCreateInfoEXT)))
                                                                                                     <*> pure (vkFlags (c :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                                                                                                     <*> pure (vkDiscardRectangleMode (c :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                                                                                                     -- Optional length valued member elided
                                                                                                     <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkDiscardRectangleCount (c :: VkPipelineDiscardRectangleStateCreateInfoEXT))) (((fromCStructRect2D <=<) . peekElemOff) p)) (vkPDiscardRectangles (c :: VkPipelineDiscardRectangleStateCreateInfoEXT))

-- | Wrapper for vkCmdSetDiscardRectangleEXT
cmdSetDiscardRectangleEXT :: CommandBuffer ->  Word32 ->  Vector Rect2D ->  IO ()
cmdSetDiscardRectangleEXT = \(CommandBuffer commandBuffer commandTable) -> \firstDiscardRectangle -> \discardRectangles -> withVec withCStructRect2D discardRectangles (\pDiscardRectangles -> Graphics.Vulkan.C.Dynamic.cmdSetDiscardRectangleEXT commandTable commandBuffer firstDiscardRectangle (fromIntegral $ Data.Vector.length discardRectangles) pDiscardRectangles *> (pure ()))
