{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_inline_uniform_block
  ( withCStructDescriptorPoolInlineUniformBlockCreateInfoEXT
  , fromCStructDescriptorPoolInlineUniformBlockCreateInfoEXT
  , DescriptorPoolInlineUniformBlockCreateInfoEXT(..)
  , withCStructPhysicalDeviceInlineUniformBlockFeaturesEXT
  , fromCStructPhysicalDeviceInlineUniformBlockFeaturesEXT
  , PhysicalDeviceInlineUniformBlockFeaturesEXT(..)
  , withCStructPhysicalDeviceInlineUniformBlockPropertiesEXT
  , fromCStructPhysicalDeviceInlineUniformBlockPropertiesEXT
  , PhysicalDeviceInlineUniformBlockPropertiesEXT(..)
  , withCStructWriteDescriptorSetInlineUniformBlockEXT
  , fromCStructWriteDescriptorSetInlineUniformBlockEXT
  , WriteDescriptorSetInlineUniformBlockEXT(..)
  , pattern VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
  , pattern VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
  , pattern VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
  ) where

import Data.ByteString
  ( ByteString
  , packCStringLen
  )
import qualified Data.ByteString
  ( length
  )
import Data.ByteString.Unsafe
  ( unsafeUseAsCString
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


import Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block
  ( VkDescriptorPoolInlineUniformBlockCreateInfoEXT(..)
  , VkPhysicalDeviceInlineUniformBlockFeaturesEXT(..)
  , VkPhysicalDeviceInlineUniformBlockPropertiesEXT(..)
  , VkWriteDescriptorSetInlineUniformBlockEXT(..)
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block
  ( pattern VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
  , pattern VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
  , pattern VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
  )


-- No documentation found for TopLevel "DescriptorPoolInlineUniformBlockCreateInfoEXT"
data DescriptorPoolInlineUniformBlockCreateInfoEXT = DescriptorPoolInlineUniformBlockCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DescriptorPoolInlineUniformBlockCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorPoolInlineUniformBlockCreateInfoEXT" "maxInlineUniformBlockBindings"
  vkMaxInlineUniformBlockBindings :: Word32
  }
  deriving (Show, Eq)
withCStructDescriptorPoolInlineUniformBlockCreateInfoEXT :: DescriptorPoolInlineUniformBlockCreateInfoEXT -> (VkDescriptorPoolInlineUniformBlockCreateInfoEXT -> IO a) -> IO a
withCStructDescriptorPoolInlineUniformBlockCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: DescriptorPoolInlineUniformBlockCreateInfoEXT)) (\pPNext -> cont (VkDescriptorPoolInlineUniformBlockCreateInfoEXT VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT pPNext (vkMaxInlineUniformBlockBindings (from :: DescriptorPoolInlineUniformBlockCreateInfoEXT))))
fromCStructDescriptorPoolInlineUniformBlockCreateInfoEXT :: VkDescriptorPoolInlineUniformBlockCreateInfoEXT -> IO DescriptorPoolInlineUniformBlockCreateInfoEXT
fromCStructDescriptorPoolInlineUniformBlockCreateInfoEXT c = DescriptorPoolInlineUniformBlockCreateInfoEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorPoolInlineUniformBlockCreateInfoEXT)))
                                                                                                           <*> pure (vkMaxInlineUniformBlockBindings (c :: VkDescriptorPoolInlineUniformBlockCreateInfoEXT))
-- No documentation found for TopLevel "PhysicalDeviceInlineUniformBlockFeaturesEXT"
data PhysicalDeviceInlineUniformBlockFeaturesEXT = PhysicalDeviceInlineUniformBlockFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockFeaturesEXT" "inlineUniformBlock"
  vkInlineUniformBlock :: Bool
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockFeaturesEXT" "descriptorBindingInlineUniformBlockUpdateAfterBind"
  vkDescriptorBindingInlineUniformBlockUpdateAfterBind :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceInlineUniformBlockFeaturesEXT :: PhysicalDeviceInlineUniformBlockFeaturesEXT -> (VkPhysicalDeviceInlineUniformBlockFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceInlineUniformBlockFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceInlineUniformBlockFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceInlineUniformBlockFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT pPNext (boolToBool32 (vkInlineUniformBlock (from :: PhysicalDeviceInlineUniformBlockFeaturesEXT))) (boolToBool32 (vkDescriptorBindingInlineUniformBlockUpdateAfterBind (from :: PhysicalDeviceInlineUniformBlockFeaturesEXT)))))
fromCStructPhysicalDeviceInlineUniformBlockFeaturesEXT :: VkPhysicalDeviceInlineUniformBlockFeaturesEXT -> IO PhysicalDeviceInlineUniformBlockFeaturesEXT
fromCStructPhysicalDeviceInlineUniformBlockFeaturesEXT c = PhysicalDeviceInlineUniformBlockFeaturesEXT <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceInlineUniformBlockFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkInlineUniformBlock (c :: VkPhysicalDeviceInlineUniformBlockFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkDescriptorBindingInlineUniformBlockUpdateAfterBind (c :: VkPhysicalDeviceInlineUniformBlockFeaturesEXT)))
-- No documentation found for TopLevel "PhysicalDeviceInlineUniformBlockPropertiesEXT"
data PhysicalDeviceInlineUniformBlockPropertiesEXT = PhysicalDeviceInlineUniformBlockPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxInlineUniformBlockSize"
  vkMaxInlineUniformBlockSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxPerStageDescriptorInlineUniformBlocks"
  vkMaxPerStageDescriptorInlineUniformBlocks :: Word32
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks"
  vkMaxPerStageDescriptorUpdateAfterBindInlineUniformBlocks :: Word32
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxDescriptorSetInlineUniformBlocks"
  vkMaxDescriptorSetInlineUniformBlocks :: Word32
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxDescriptorSetUpdateAfterBindInlineUniformBlocks"
  vkMaxDescriptorSetUpdateAfterBindInlineUniformBlocks :: Word32
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceInlineUniformBlockPropertiesEXT :: PhysicalDeviceInlineUniformBlockPropertiesEXT -> (VkPhysicalDeviceInlineUniformBlockPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceInlineUniformBlockPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceInlineUniformBlockPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceInlineUniformBlockPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT pPNext (vkMaxInlineUniformBlockSize (from :: PhysicalDeviceInlineUniformBlockPropertiesEXT)) (vkMaxPerStageDescriptorInlineUniformBlocks (from :: PhysicalDeviceInlineUniformBlockPropertiesEXT)) (vkMaxPerStageDescriptorUpdateAfterBindInlineUniformBlocks (from :: PhysicalDeviceInlineUniformBlockPropertiesEXT)) (vkMaxDescriptorSetInlineUniformBlocks (from :: PhysicalDeviceInlineUniformBlockPropertiesEXT)) (vkMaxDescriptorSetUpdateAfterBindInlineUniformBlocks (from :: PhysicalDeviceInlineUniformBlockPropertiesEXT))))
fromCStructPhysicalDeviceInlineUniformBlockPropertiesEXT :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT -> IO PhysicalDeviceInlineUniformBlockPropertiesEXT
fromCStructPhysicalDeviceInlineUniformBlockPropertiesEXT c = PhysicalDeviceInlineUniformBlockPropertiesEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT)))
                                                                                                           <*> pure (vkMaxInlineUniformBlockSize (c :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                                                                                                           <*> pure (vkMaxPerStageDescriptorInlineUniformBlocks (c :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                                                                                                           <*> pure (vkMaxPerStageDescriptorUpdateAfterBindInlineUniformBlocks (c :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                                                                                                           <*> pure (vkMaxDescriptorSetInlineUniformBlocks (c :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                                                                                                           <*> pure (vkMaxDescriptorSetUpdateAfterBindInlineUniformBlocks (c :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
-- No documentation found for TopLevel "WriteDescriptorSetInlineUniformBlockEXT"
data WriteDescriptorSetInlineUniformBlockEXT = WriteDescriptorSetInlineUniformBlockEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "WriteDescriptorSetInlineUniformBlockEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Bytestring length valued member elided
  , -- No documentation found for Nested "WriteDescriptorSetInlineUniformBlockEXT" "pData"
  vkPData :: ByteString
  }
  deriving (Show, Eq)
withCStructWriteDescriptorSetInlineUniformBlockEXT :: WriteDescriptorSetInlineUniformBlockEXT -> (VkWriteDescriptorSetInlineUniformBlockEXT -> IO a) -> IO a
withCStructWriteDescriptorSetInlineUniformBlockEXT from cont = unsafeUseAsCString (vkPData (from :: WriteDescriptorSetInlineUniformBlockEXT)) (\pData -> maybeWith withSomeVkStruct (vkPNext (from :: WriteDescriptorSetInlineUniformBlockEXT)) (\pPNext -> cont (VkWriteDescriptorSetInlineUniformBlockEXT VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT pPNext (fromIntegral (Data.ByteString.length (vkPData (from :: WriteDescriptorSetInlineUniformBlockEXT)))) (castPtr pData))))
fromCStructWriteDescriptorSetInlineUniformBlockEXT :: VkWriteDescriptorSetInlineUniformBlockEXT -> IO WriteDescriptorSetInlineUniformBlockEXT
fromCStructWriteDescriptorSetInlineUniformBlockEXT c = WriteDescriptorSetInlineUniformBlockEXT <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWriteDescriptorSetInlineUniformBlockEXT)))
                                                                                               -- Bytestring length valued member elided
                                                                                               <*> packCStringLen (castPtr (vkPData (c :: VkWriteDescriptorSetInlineUniformBlockEXT)), fromIntegral (vkDataSize (c :: VkWriteDescriptorSetInlineUniformBlockEXT)))
