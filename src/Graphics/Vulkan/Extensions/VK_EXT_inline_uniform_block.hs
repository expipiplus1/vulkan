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
  , pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
  , pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
  , pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
  , pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
  ) where

import Data.ByteString
  ( ByteString
  , packCStringLen
  )
import qualified Data.ByteString
  ( empty
  , length
  )
import Data.ByteString.Unsafe
  ( unsafeUseAsCString
  )
import Data.String
  ( IsString
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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block
  ( VkDescriptorPoolInlineUniformBlockCreateInfoEXT(..)
  , VkPhysicalDeviceInlineUniformBlockFeaturesEXT(..)
  , VkPhysicalDeviceInlineUniformBlockPropertiesEXT(..)
  , VkWriteDescriptorSetInlineUniformBlockEXT(..)
  , pattern VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
  , pattern VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
  )



-- | VkDescriptorPoolInlineUniformBlockCreateInfoEXT - Structure specifying
-- the maximum number of inline uniform block bindings of a newly created
-- descriptor pool
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data DescriptorPoolInlineUniformBlockCreateInfoEXT = DescriptorPoolInlineUniformBlockCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DescriptorPoolInlineUniformBlockCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorPoolInlineUniformBlockCreateInfoEXT" "maxInlineUniformBlockBindings"
  maxInlineUniformBlockBindings :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDescriptorPoolInlineUniformBlockCreateInfoEXT' and
-- marshal a 'DescriptorPoolInlineUniformBlockCreateInfoEXT' into it. The 'VkDescriptorPoolInlineUniformBlockCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorPoolInlineUniformBlockCreateInfoEXT :: DescriptorPoolInlineUniformBlockCreateInfoEXT -> (VkDescriptorPoolInlineUniformBlockCreateInfoEXT -> IO a) -> IO a
withCStructDescriptorPoolInlineUniformBlockCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DescriptorPoolInlineUniformBlockCreateInfoEXT)) (\pPNext -> cont (VkDescriptorPoolInlineUniformBlockCreateInfoEXT VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT pPNext (maxInlineUniformBlockBindings (marshalled :: DescriptorPoolInlineUniformBlockCreateInfoEXT))))

-- | A function to read a 'VkDescriptorPoolInlineUniformBlockCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'DescriptorPoolInlineUniformBlockCreateInfoEXT'.
fromCStructDescriptorPoolInlineUniformBlockCreateInfoEXT :: VkDescriptorPoolInlineUniformBlockCreateInfoEXT -> IO DescriptorPoolInlineUniformBlockCreateInfoEXT
fromCStructDescriptorPoolInlineUniformBlockCreateInfoEXT c = DescriptorPoolInlineUniformBlockCreateInfoEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorPoolInlineUniformBlockCreateInfoEXT)))
                                                                                                           <*> pure (vkMaxInlineUniformBlockBindings (c :: VkDescriptorPoolInlineUniformBlockCreateInfoEXT))

instance Zero DescriptorPoolInlineUniformBlockCreateInfoEXT where
  zero = DescriptorPoolInlineUniformBlockCreateInfoEXT Nothing
                                                       zero



-- | VkPhysicalDeviceInlineUniformBlockFeaturesEXT - Structure describing
-- inline uniform block features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkPhysicalDeviceInlineUniformBlockFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkPhysicalDeviceInlineUniformBlockFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkPhysicalDeviceInlineUniformBlockFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceInlineUniformBlockFeaturesEXT = PhysicalDeviceInlineUniformBlockFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockFeaturesEXT" "inlineUniformBlock"
  inlineUniformBlock :: Bool
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockFeaturesEXT" "descriptorBindingInlineUniformBlockUpdateAfterBind"
  descriptorBindingInlineUniformBlockUpdateAfterBind :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceInlineUniformBlockFeaturesEXT' and
-- marshal a 'PhysicalDeviceInlineUniformBlockFeaturesEXT' into it. The 'VkPhysicalDeviceInlineUniformBlockFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceInlineUniformBlockFeaturesEXT :: PhysicalDeviceInlineUniformBlockFeaturesEXT -> (VkPhysicalDeviceInlineUniformBlockFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceInlineUniformBlockFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceInlineUniformBlockFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceInlineUniformBlockFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT pPNext (boolToBool32 (inlineUniformBlock (marshalled :: PhysicalDeviceInlineUniformBlockFeaturesEXT))) (boolToBool32 (descriptorBindingInlineUniformBlockUpdateAfterBind (marshalled :: PhysicalDeviceInlineUniformBlockFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceInlineUniformBlockFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceInlineUniformBlockFeaturesEXT'.
fromCStructPhysicalDeviceInlineUniformBlockFeaturesEXT :: VkPhysicalDeviceInlineUniformBlockFeaturesEXT -> IO PhysicalDeviceInlineUniformBlockFeaturesEXT
fromCStructPhysicalDeviceInlineUniformBlockFeaturesEXT c = PhysicalDeviceInlineUniformBlockFeaturesEXT <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceInlineUniformBlockFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkInlineUniformBlock (c :: VkPhysicalDeviceInlineUniformBlockFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkDescriptorBindingInlineUniformBlockUpdateAfterBind (c :: VkPhysicalDeviceInlineUniformBlockFeaturesEXT)))

instance Zero PhysicalDeviceInlineUniformBlockFeaturesEXT where
  zero = PhysicalDeviceInlineUniformBlockFeaturesEXT Nothing
                                                     False
                                                     False



-- | VkPhysicalDeviceInlineUniformBlockPropertiesEXT - Structure describing
-- inline uniform block properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkPhysicalDeviceInlineUniformBlockPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkPhysicalDeviceInlineUniformBlockPropertiesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceInlineUniformBlockPropertiesEXT = PhysicalDeviceInlineUniformBlockPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxInlineUniformBlockSize"
  maxInlineUniformBlockSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxPerStageDescriptorInlineUniformBlocks"
  maxPerStageDescriptorInlineUniformBlocks :: Word32
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks"
  maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks :: Word32
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxDescriptorSetInlineUniformBlocks"
  maxDescriptorSetInlineUniformBlocks :: Word32
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxDescriptorSetUpdateAfterBindInlineUniformBlocks"
  maxDescriptorSetUpdateAfterBindInlineUniformBlocks :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceInlineUniformBlockPropertiesEXT' and
-- marshal a 'PhysicalDeviceInlineUniformBlockPropertiesEXT' into it. The 'VkPhysicalDeviceInlineUniformBlockPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceInlineUniformBlockPropertiesEXT :: PhysicalDeviceInlineUniformBlockPropertiesEXT -> (VkPhysicalDeviceInlineUniformBlockPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceInlineUniformBlockPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceInlineUniformBlockPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceInlineUniformBlockPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT pPNext (maxInlineUniformBlockSize (marshalled :: PhysicalDeviceInlineUniformBlockPropertiesEXT)) (maxPerStageDescriptorInlineUniformBlocks (marshalled :: PhysicalDeviceInlineUniformBlockPropertiesEXT)) (maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks (marshalled :: PhysicalDeviceInlineUniformBlockPropertiesEXT)) (maxDescriptorSetInlineUniformBlocks (marshalled :: PhysicalDeviceInlineUniformBlockPropertiesEXT)) (maxDescriptorSetUpdateAfterBindInlineUniformBlocks (marshalled :: PhysicalDeviceInlineUniformBlockPropertiesEXT))))

-- | A function to read a 'VkPhysicalDeviceInlineUniformBlockPropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceInlineUniformBlockPropertiesEXT'.
fromCStructPhysicalDeviceInlineUniformBlockPropertiesEXT :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT -> IO PhysicalDeviceInlineUniformBlockPropertiesEXT
fromCStructPhysicalDeviceInlineUniformBlockPropertiesEXT c = PhysicalDeviceInlineUniformBlockPropertiesEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT)))
                                                                                                           <*> pure (vkMaxInlineUniformBlockSize (c :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                                                                                                           <*> pure (vkMaxPerStageDescriptorInlineUniformBlocks (c :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                                                                                                           <*> pure (vkMaxPerStageDescriptorUpdateAfterBindInlineUniformBlocks (c :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                                                                                                           <*> pure (vkMaxDescriptorSetInlineUniformBlocks (c :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                                                                                                           <*> pure (vkMaxDescriptorSetUpdateAfterBindInlineUniformBlocks (c :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))

instance Zero PhysicalDeviceInlineUniformBlockPropertiesEXT where
  zero = PhysicalDeviceInlineUniformBlockPropertiesEXT Nothing
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero



-- | VkWriteDescriptorSetInlineUniformBlockEXT - Structure specifying inline
-- uniform block data
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data WriteDescriptorSetInlineUniformBlockEXT = WriteDescriptorSetInlineUniformBlockEXT
  { -- Univalued member elided
  -- No documentation found for Nested "WriteDescriptorSetInlineUniformBlockEXT" "pNext"
  next :: Maybe SomeVkStruct
  -- Bytestring length valued member elided
  , -- No documentation found for Nested "WriteDescriptorSetInlineUniformBlockEXT" "pData"
  data' :: ByteString
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkWriteDescriptorSetInlineUniformBlockEXT' and
-- marshal a 'WriteDescriptorSetInlineUniformBlockEXT' into it. The 'VkWriteDescriptorSetInlineUniformBlockEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructWriteDescriptorSetInlineUniformBlockEXT :: WriteDescriptorSetInlineUniformBlockEXT -> (VkWriteDescriptorSetInlineUniformBlockEXT -> IO a) -> IO a
withCStructWriteDescriptorSetInlineUniformBlockEXT marshalled cont = unsafeUseAsCString (data' (marshalled :: WriteDescriptorSetInlineUniformBlockEXT)) (\pPData -> maybeWith withSomeVkStruct (next (marshalled :: WriteDescriptorSetInlineUniformBlockEXT)) (\pPNext -> cont (VkWriteDescriptorSetInlineUniformBlockEXT VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT pPNext (fromIntegral (Data.ByteString.length (data' (marshalled :: WriteDescriptorSetInlineUniformBlockEXT)))) (castPtr pPData))))

-- | A function to read a 'VkWriteDescriptorSetInlineUniformBlockEXT' and all additional
-- structures in the pointer chain into a 'WriteDescriptorSetInlineUniformBlockEXT'.
fromCStructWriteDescriptorSetInlineUniformBlockEXT :: VkWriteDescriptorSetInlineUniformBlockEXT -> IO WriteDescriptorSetInlineUniformBlockEXT
fromCStructWriteDescriptorSetInlineUniformBlockEXT c = WriteDescriptorSetInlineUniformBlockEXT <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWriteDescriptorSetInlineUniformBlockEXT)))
                                                                                               -- Bytestring length valued member elided
                                                                                               <*> packCStringLen (castPtr (vkPData (c :: VkWriteDescriptorSetInlineUniformBlockEXT)), fromIntegral (vkDataSize (c :: VkWriteDescriptorSetInlineUniformBlockEXT)))

instance Zero WriteDescriptorSetInlineUniformBlockEXT where
  zero = WriteDescriptorSetInlineUniformBlockEXT Nothing
                                                 Data.ByteString.empty


-- No documentation found for TopLevel "VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME"
pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME = VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION"
pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION :: Integral a => a
pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION = VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
