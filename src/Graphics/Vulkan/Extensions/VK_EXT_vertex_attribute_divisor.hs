{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor
  ( withCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT
  , fromCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT
  , PhysicalDeviceVertexAttributeDivisorFeaturesEXT(..)
  , withCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT
  , fromCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT
  , PhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  , withCStructPipelineVertexInputDivisorStateCreateInfoEXT
  , fromCStructPipelineVertexInputDivisorStateCreateInfoEXT
  , PipelineVertexInputDivisorStateCreateInfoEXT(..)
  , withCStructVertexInputBindingDivisorDescriptionEXT
  , fromCStructVertexInputBindingDivisorDescriptionEXT
  , VertexInputBindingDivisorDescriptionEXT(..)
  , pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
  , pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT
  ) where

import Control.Monad
  ( (<=<)
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
import Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor
  ( VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT(..)
  , VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  , VkPipelineVertexInputDivisorStateCreateInfoEXT(..)
  , VkVertexInputBindingDivisorDescriptionEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor
  ( pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
  , pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
  )



-- | VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT - Structure describing
-- if fetching of vertex attribute may be repeated for instanced rendering
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor.VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating the implementation-dependent
-- behavior.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor.VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT'
-- /can/ also be used in @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable the
-- feature.
--
-- Unresolved directive in
-- VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceVertexAttributeDivisorFeaturesEXT = PhysicalDeviceVertexAttributeDivisorFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorFeaturesEXT" "vertexAttributeInstanceRateDivisor"
  vertexAttributeInstanceRateDivisor :: Bool
  , -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorFeaturesEXT" "vertexAttributeInstanceRateZeroDivisor"
  vertexAttributeInstanceRateZeroDivisor :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT' and
-- marshal a 'PhysicalDeviceVertexAttributeDivisorFeaturesEXT' into it. The 'VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT :: PhysicalDeviceVertexAttributeDivisorFeaturesEXT -> (VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceVertexAttributeDivisorFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT pPNext (boolToBool32 (vertexAttributeInstanceRateDivisor (marshalled :: PhysicalDeviceVertexAttributeDivisorFeaturesEXT))) (boolToBool32 (vertexAttributeInstanceRateZeroDivisor (marshalled :: PhysicalDeviceVertexAttributeDivisorFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceVertexAttributeDivisorFeaturesEXT'.
fromCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT :: VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT -> IO PhysicalDeviceVertexAttributeDivisorFeaturesEXT
fromCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT c = PhysicalDeviceVertexAttributeDivisorFeaturesEXT <$> -- Univalued Member elided
                                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT)))
                                                                                                               <*> pure (bool32ToBool (vkVertexAttributeInstanceRateDivisor (c :: VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT)))
                                                                                                               <*> pure (bool32ToBool (vkVertexAttributeInstanceRateZeroDivisor (c :: VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT)))

instance Zero PhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  zero = PhysicalDeviceVertexAttributeDivisorFeaturesEXT Nothing
                                                         False
                                                         False



-- | VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT - Structure
-- describing max value of vertex attribute divisor that can be supported
-- by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor.VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor.VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in
-- VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceVertexAttributeDivisorPropertiesEXT = PhysicalDeviceVertexAttributeDivisorPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorPropertiesEXT" "maxVertexAttribDivisor"
  maxVertexAttribDivisor :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT' and
-- marshal a 'PhysicalDeviceVertexAttributeDivisorPropertiesEXT' into it. The 'VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT :: PhysicalDeviceVertexAttributeDivisorPropertiesEXT -> (VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceVertexAttributeDivisorPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT pPNext (maxVertexAttribDivisor (marshalled :: PhysicalDeviceVertexAttributeDivisorPropertiesEXT))))

-- | A function to read a 'VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceVertexAttributeDivisorPropertiesEXT'.
fromCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT :: VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT -> IO PhysicalDeviceVertexAttributeDivisorPropertiesEXT
fromCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT c = PhysicalDeviceVertexAttributeDivisorPropertiesEXT <$> -- Univalued Member elided
                                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT)))
                                                                                                                   <*> pure (vkMaxVertexAttribDivisor (c :: VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT))

instance Zero PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  zero = PhysicalDeviceVertexAttributeDivisorPropertiesEXT Nothing
                                                           zero



-- | VkPipelineVertexInputDivisorStateCreateInfoEXT - Structure specifying
-- vertex attributes assignment during instanced rendering
--
-- = Description
--
-- Unresolved directive in
-- VkPipelineVertexInputDivisorStateCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkPipelineVertexInputDivisorStateCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PipelineVertexInputDivisorStateCreateInfoEXT = PipelineVertexInputDivisorStateCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineVertexInputDivisorStateCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineVertexInputDivisorStateCreateInfoEXT" "pVertexBindingDivisors"
  vertexBindingDivisors :: Vector VertexInputBindingDivisorDescriptionEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineVertexInputDivisorStateCreateInfoEXT' and
-- marshal a 'PipelineVertexInputDivisorStateCreateInfoEXT' into it. The 'VkPipelineVertexInputDivisorStateCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineVertexInputDivisorStateCreateInfoEXT :: PipelineVertexInputDivisorStateCreateInfoEXT -> (VkPipelineVertexInputDivisorStateCreateInfoEXT -> IO a) -> IO a
withCStructPipelineVertexInputDivisorStateCreateInfoEXT marshalled cont = withVec withCStructVertexInputBindingDivisorDescriptionEXT (vertexBindingDivisors (marshalled :: PipelineVertexInputDivisorStateCreateInfoEXT)) (\pPVertexBindingDivisors -> maybeWith withSomeVkStruct (next (marshalled :: PipelineVertexInputDivisorStateCreateInfoEXT)) (\pPNext -> cont (VkPipelineVertexInputDivisorStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT pPNext (fromIntegral (Data.Vector.length (vertexBindingDivisors (marshalled :: PipelineVertexInputDivisorStateCreateInfoEXT)))) pPVertexBindingDivisors)))

-- | A function to read a 'VkPipelineVertexInputDivisorStateCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'PipelineVertexInputDivisorStateCreateInfoEXT'.
fromCStructPipelineVertexInputDivisorStateCreateInfoEXT :: VkPipelineVertexInputDivisorStateCreateInfoEXT -> IO PipelineVertexInputDivisorStateCreateInfoEXT
fromCStructPipelineVertexInputDivisorStateCreateInfoEXT c = PipelineVertexInputDivisorStateCreateInfoEXT <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineVertexInputDivisorStateCreateInfoEXT)))
                                                                                                         -- Length valued member elided
                                                                                                         <*> (Data.Vector.generateM (fromIntegral (vkVertexBindingDivisorCount (c :: VkPipelineVertexInputDivisorStateCreateInfoEXT))) (((fromCStructVertexInputBindingDivisorDescriptionEXT <=<) . peekElemOff) (vkPVertexBindingDivisors (c :: VkPipelineVertexInputDivisorStateCreateInfoEXT))))

instance Zero PipelineVertexInputDivisorStateCreateInfoEXT where
  zero = PipelineVertexInputDivisorStateCreateInfoEXT Nothing
                                                      Data.Vector.empty



-- | VkVertexInputBindingDivisorDescriptionEXT - Structure specifying a
-- divisor used in instanced rendering
--
-- = Description
--
-- If this structure is not used to define a divisor value for an attribute
-- then the divisor has a logical default value of 1.
--
-- == Valid Usage
--
-- -   @binding@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   If the @vertexAttributeInstanceRateZeroDivisor@ feature is not
--     enabled, @divisor@ /must/ not be @0@
--
-- -   If the @vertexAttributeInstanceRateDivisor@ feature is not enabled,
--     @divisor@ /must/ be @1@
--
-- -   @divisor@ /must/ be a value between @0@ and
--     'Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor.VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT'::@maxVertexAttribDivisor@,
--     inclusive.
--
-- -   'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputBindingDescription'::@inputRate@
--     /must/ be of type
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_VERTEX_INPUT_RATE_INSTANCE'
--     for this @binding@.
--
-- Unresolved directive in VkVertexInputBindingDivisorDescriptionEXT.txt -
-- include::{generated}\/validity\/structs\/VkVertexInputBindingDivisorDescriptionEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VertexInputBindingDivisorDescriptionEXT = VertexInputBindingDivisorDescriptionEXT
  { -- No documentation found for Nested "VertexInputBindingDivisorDescriptionEXT" "binding"
  binding :: Word32
  , -- No documentation found for Nested "VertexInputBindingDivisorDescriptionEXT" "divisor"
  divisor :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkVertexInputBindingDivisorDescriptionEXT' and
-- marshal a 'VertexInputBindingDivisorDescriptionEXT' into it. The 'VkVertexInputBindingDivisorDescriptionEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructVertexInputBindingDivisorDescriptionEXT :: VertexInputBindingDivisorDescriptionEXT -> (VkVertexInputBindingDivisorDescriptionEXT -> IO a) -> IO a
withCStructVertexInputBindingDivisorDescriptionEXT marshalled cont = cont (VkVertexInputBindingDivisorDescriptionEXT (binding (marshalled :: VertexInputBindingDivisorDescriptionEXT)) (divisor (marshalled :: VertexInputBindingDivisorDescriptionEXT)))

-- | A function to read a 'VkVertexInputBindingDivisorDescriptionEXT' and all additional
-- structures in the pointer chain into a 'VertexInputBindingDivisorDescriptionEXT'.
fromCStructVertexInputBindingDivisorDescriptionEXT :: VkVertexInputBindingDivisorDescriptionEXT -> IO VertexInputBindingDivisorDescriptionEXT
fromCStructVertexInputBindingDivisorDescriptionEXT c = VertexInputBindingDivisorDescriptionEXT <$> pure (vkBinding (c :: VkVertexInputBindingDivisorDescriptionEXT))
                                                                                               <*> pure (vkDivisor (c :: VkVertexInputBindingDivisorDescriptionEXT))

instance Zero VertexInputBindingDivisorDescriptionEXT where
  zero = VertexInputBindingDivisorDescriptionEXT zero
                                                 zero

