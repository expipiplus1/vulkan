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


-- No documentation found for TopLevel "PhysicalDeviceVertexAttributeDivisorFeaturesEXT"
data PhysicalDeviceVertexAttributeDivisorFeaturesEXT = PhysicalDeviceVertexAttributeDivisorFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorFeaturesEXT" "vertexAttributeInstanceRateDivisor"
  vkVertexAttributeInstanceRateDivisor :: Bool
  , -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorFeaturesEXT" "vertexAttributeInstanceRateZeroDivisor"
  vkVertexAttributeInstanceRateZeroDivisor :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT :: PhysicalDeviceVertexAttributeDivisorFeaturesEXT -> (VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceVertexAttributeDivisorFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT pPNext (boolToBool32 (vkVertexAttributeInstanceRateDivisor (from :: PhysicalDeviceVertexAttributeDivisorFeaturesEXT))) (boolToBool32 (vkVertexAttributeInstanceRateZeroDivisor (from :: PhysicalDeviceVertexAttributeDivisorFeaturesEXT)))))
fromCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT :: VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT -> IO PhysicalDeviceVertexAttributeDivisorFeaturesEXT
fromCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT c = PhysicalDeviceVertexAttributeDivisorFeaturesEXT <$> -- Univalued Member elided
                                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT)))
                                                                                                               <*> pure (bool32ToBool (vkVertexAttributeInstanceRateDivisor (c :: VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT)))
                                                                                                               <*> pure (bool32ToBool (vkVertexAttributeInstanceRateZeroDivisor (c :: VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT)))
instance Zero PhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  zero = PhysicalDeviceVertexAttributeDivisorFeaturesEXT Nothing
                                                         False
                                                         False
-- No documentation found for TopLevel "PhysicalDeviceVertexAttributeDivisorPropertiesEXT"
data PhysicalDeviceVertexAttributeDivisorPropertiesEXT = PhysicalDeviceVertexAttributeDivisorPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorPropertiesEXT" "maxVertexAttribDivisor"
  vkMaxVertexAttribDivisor :: Word32
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT :: PhysicalDeviceVertexAttributeDivisorPropertiesEXT -> (VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceVertexAttributeDivisorPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT pPNext (vkMaxVertexAttribDivisor (from :: PhysicalDeviceVertexAttributeDivisorPropertiesEXT))))
fromCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT :: VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT -> IO PhysicalDeviceVertexAttributeDivisorPropertiesEXT
fromCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT c = PhysicalDeviceVertexAttributeDivisorPropertiesEXT <$> -- Univalued Member elided
                                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT)))
                                                                                                                   <*> pure (vkMaxVertexAttribDivisor (c :: VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT))
instance Zero PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  zero = PhysicalDeviceVertexAttributeDivisorPropertiesEXT Nothing
                                                           zero
-- No documentation found for TopLevel "PipelineVertexInputDivisorStateCreateInfoEXT"
data PipelineVertexInputDivisorStateCreateInfoEXT = PipelineVertexInputDivisorStateCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineVertexInputDivisorStateCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineVertexInputDivisorStateCreateInfoEXT" "pVertexBindingDivisors"
  vkPVertexBindingDivisors :: Vector VertexInputBindingDivisorDescriptionEXT
  }
  deriving (Show, Eq)
withCStructPipelineVertexInputDivisorStateCreateInfoEXT :: PipelineVertexInputDivisorStateCreateInfoEXT -> (VkPipelineVertexInputDivisorStateCreateInfoEXT -> IO a) -> IO a
withCStructPipelineVertexInputDivisorStateCreateInfoEXT from cont = withVec withCStructVertexInputBindingDivisorDescriptionEXT (vkPVertexBindingDivisors (from :: PipelineVertexInputDivisorStateCreateInfoEXT)) (\pVertexBindingDivisors -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineVertexInputDivisorStateCreateInfoEXT)) (\pPNext -> cont (VkPipelineVertexInputDivisorStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT pPNext (fromIntegral (Data.Vector.length (vkPVertexBindingDivisors (from :: PipelineVertexInputDivisorStateCreateInfoEXT)))) pVertexBindingDivisors)))
fromCStructPipelineVertexInputDivisorStateCreateInfoEXT :: VkPipelineVertexInputDivisorStateCreateInfoEXT -> IO PipelineVertexInputDivisorStateCreateInfoEXT
fromCStructPipelineVertexInputDivisorStateCreateInfoEXT c = PipelineVertexInputDivisorStateCreateInfoEXT <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineVertexInputDivisorStateCreateInfoEXT)))
                                                                                                         -- Length valued member elided
                                                                                                         <*> (Data.Vector.generateM (fromIntegral (vkVertexBindingDivisorCount (c :: VkPipelineVertexInputDivisorStateCreateInfoEXT))) (((fromCStructVertexInputBindingDivisorDescriptionEXT <=<) . peekElemOff) (vkPVertexBindingDivisors (c :: VkPipelineVertexInputDivisorStateCreateInfoEXT))))
instance Zero PipelineVertexInputDivisorStateCreateInfoEXT where
  zero = PipelineVertexInputDivisorStateCreateInfoEXT Nothing
                                                      Data.Vector.empty
-- No documentation found for TopLevel "VertexInputBindingDivisorDescriptionEXT"
data VertexInputBindingDivisorDescriptionEXT = VertexInputBindingDivisorDescriptionEXT
  { -- No documentation found for Nested "VertexInputBindingDivisorDescriptionEXT" "binding"
  vkBinding :: Word32
  , -- No documentation found for Nested "VertexInputBindingDivisorDescriptionEXT" "divisor"
  vkDivisor :: Word32
  }
  deriving (Show, Eq)
withCStructVertexInputBindingDivisorDescriptionEXT :: VertexInputBindingDivisorDescriptionEXT -> (VkVertexInputBindingDivisorDescriptionEXT -> IO a) -> IO a
withCStructVertexInputBindingDivisorDescriptionEXT from cont = cont (VkVertexInputBindingDivisorDescriptionEXT (vkBinding (from :: VertexInputBindingDivisorDescriptionEXT)) (vkDivisor (from :: VertexInputBindingDivisorDescriptionEXT)))
fromCStructVertexInputBindingDivisorDescriptionEXT :: VkVertexInputBindingDivisorDescriptionEXT -> IO VertexInputBindingDivisorDescriptionEXT
fromCStructVertexInputBindingDivisorDescriptionEXT c = VertexInputBindingDivisorDescriptionEXT <$> pure (vkBinding (c :: VkVertexInputBindingDivisorDescriptionEXT))
                                                                                               <*> pure (vkDivisor (c :: VkVertexInputBindingDivisorDescriptionEXT))
instance Zero VertexInputBindingDivisorDescriptionEXT where
  zero = VertexInputBindingDivisorDescriptionEXT zero
                                                 zero
