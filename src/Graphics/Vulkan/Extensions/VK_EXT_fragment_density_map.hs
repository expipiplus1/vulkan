{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map
  ( withCStructPhysicalDeviceFragmentDensityMapFeaturesEXT
  , fromCStructPhysicalDeviceFragmentDensityMapFeaturesEXT
  , PhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , withCStructPhysicalDeviceFragmentDensityMapPropertiesEXT
  , fromCStructPhysicalDeviceFragmentDensityMapPropertiesEXT
  , PhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , withCStructRenderPassFragmentDensityMapCreateInfoEXT
  , fromCStructRenderPassFragmentDensityMapCreateInfoEXT
  , RenderPassFragmentDensityMapCreateInfoEXT(..)
  , pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
  , pattern EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
  , pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT
  , pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
  , pattern ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
  , pattern FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
  , pattern PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
  , pattern SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
  , pattern SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
  ) where

import Data.String
  ( IsString
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
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( VkPhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , VkPhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , VkRenderPassFragmentDensityMapCreateInfoEXT(..)
  , pattern VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
  , pattern VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.Pass
  ( AttachmentReference(..)
  , fromCStructAttachmentReference
  , withCStructAttachmentReference
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , fromCStructExtent2D
  , withCStructExtent2D
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT
  , pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
  )
import Graphics.Vulkan.Core10.Image
  ( pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
  )
import Graphics.Vulkan.Core10.ImageView
  ( pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
  )
import Graphics.Vulkan.Core10.Queue
  ( pattern PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
  )
import Graphics.Vulkan.Core10.Sampler
  ( pattern SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
  , pattern SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
  )



-- | VkPhysicalDeviceFragmentDensityMapFeaturesEXT - Structure describing
-- fragment density map features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VkPhysicalDeviceFragmentDensityMapFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VkPhysicalDeviceFragmentDensityMapFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VkPhysicalDeviceFragmentDensityMapFeaturesEXT'
-- /can/ also be used in @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable the
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceFragmentDensityMapFeaturesEXT = PhysicalDeviceFragmentDensityMapFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMap"
  fragmentDensityMap :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMapDynamic"
  fragmentDensityMapDynamic :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapFeaturesEXT" "fragmentDensityMapNonSubsampledImages"
  fragmentDensityMapNonSubsampledImages :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceFragmentDensityMapFeaturesEXT' and
-- marshal a 'PhysicalDeviceFragmentDensityMapFeaturesEXT' into it. The 'VkPhysicalDeviceFragmentDensityMapFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceFragmentDensityMapFeaturesEXT :: PhysicalDeviceFragmentDensityMapFeaturesEXT -> (VkPhysicalDeviceFragmentDensityMapFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceFragmentDensityMapFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceFragmentDensityMapFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceFragmentDensityMapFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT pPNext (boolToBool32 (fragmentDensityMap (marshalled :: PhysicalDeviceFragmentDensityMapFeaturesEXT))) (boolToBool32 (fragmentDensityMapDynamic (marshalled :: PhysicalDeviceFragmentDensityMapFeaturesEXT))) (boolToBool32 (fragmentDensityMapNonSubsampledImages (marshalled :: PhysicalDeviceFragmentDensityMapFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceFragmentDensityMapFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceFragmentDensityMapFeaturesEXT'.
fromCStructPhysicalDeviceFragmentDensityMapFeaturesEXT :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT -> IO PhysicalDeviceFragmentDensityMapFeaturesEXT
fromCStructPhysicalDeviceFragmentDensityMapFeaturesEXT c = PhysicalDeviceFragmentDensityMapFeaturesEXT <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkFragmentDensityMap (c :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkFragmentDensityMapDynamic (c :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkFragmentDensityMapNonSubsampledImages (c :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT)))

instance Zero PhysicalDeviceFragmentDensityMapFeaturesEXT where
  zero = PhysicalDeviceFragmentDensityMapFeaturesEXT Nothing
                                                     False
                                                     False
                                                     False



-- | VkPhysicalDeviceFragmentDensityMapPropertiesEXT - Structure describing
-- fragment density map properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VkPhysicalDeviceFragmentDensityMapPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- == Valid Usage (Implicit)
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VkPhysicalDeviceFragmentDensityMapPropertiesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2KHR',
-- it is filled with the implementation-dependent limits and properties.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceFragmentDensityMapPropertiesEXT = PhysicalDeviceFragmentDensityMapPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapPropertiesEXT" "minFragmentDensityTexelSize"
  minFragmentDensityTexelSize :: Extent2D
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapPropertiesEXT" "maxFragmentDensityTexelSize"
  maxFragmentDensityTexelSize :: Extent2D
  , -- No documentation found for Nested "PhysicalDeviceFragmentDensityMapPropertiesEXT" "fragmentDensityInvocations"
  fragmentDensityInvocations :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceFragmentDensityMapPropertiesEXT' and
-- marshal a 'PhysicalDeviceFragmentDensityMapPropertiesEXT' into it. The 'VkPhysicalDeviceFragmentDensityMapPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceFragmentDensityMapPropertiesEXT :: PhysicalDeviceFragmentDensityMapPropertiesEXT -> (VkPhysicalDeviceFragmentDensityMapPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceFragmentDensityMapPropertiesEXT marshalled cont = withCStructExtent2D (maxFragmentDensityTexelSize (marshalled :: PhysicalDeviceFragmentDensityMapPropertiesEXT)) (\maxFragmentDensityTexelSize'' -> withCStructExtent2D (minFragmentDensityTexelSize (marshalled :: PhysicalDeviceFragmentDensityMapPropertiesEXT)) (\minFragmentDensityTexelSize'' -> maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceFragmentDensityMapPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceFragmentDensityMapPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT pPNext minFragmentDensityTexelSize'' maxFragmentDensityTexelSize'' (boolToBool32 (fragmentDensityInvocations (marshalled :: PhysicalDeviceFragmentDensityMapPropertiesEXT)))))))

-- | A function to read a 'VkPhysicalDeviceFragmentDensityMapPropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceFragmentDensityMapPropertiesEXT'.
fromCStructPhysicalDeviceFragmentDensityMapPropertiesEXT :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT -> IO PhysicalDeviceFragmentDensityMapPropertiesEXT
fromCStructPhysicalDeviceFragmentDensityMapPropertiesEXT c = PhysicalDeviceFragmentDensityMapPropertiesEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT)))
                                                                                                           <*> (fromCStructExtent2D (vkMinFragmentDensityTexelSize (c :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT)))
                                                                                                           <*> (fromCStructExtent2D (vkMaxFragmentDensityTexelSize (c :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT)))
                                                                                                           <*> pure (bool32ToBool (vkFragmentDensityInvocations (c :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT)))

instance Zero PhysicalDeviceFragmentDensityMapPropertiesEXT where
  zero = PhysicalDeviceFragmentDensityMapPropertiesEXT Nothing
                                                       zero
                                                       zero
                                                       False



-- | VkRenderPassFragmentDensityMapCreateInfoEXT - Structure containing
-- fragment density map attachment for render pass
--
-- = Description
--
-- The fragment density map attachment is read at an
-- implementation-dependent time either by the host during
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass' if
-- the attachment’s image view was not created with @flags@ containing
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT',
-- or by the device when drawing commands in the renderpass execute
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'.
--
-- If this structure is not present, it is as if
-- @fragmentDensityMapAttachment@ was given as
-- 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'.
--
-- == Valid Usage
--
-- -   If @fragmentDensityMapAttachment@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED',
--     @fragmentDensityMapAttachment@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo'::@attachmentCount@
--
-- -   If @fragmentDensityMapAttachment@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED',
--     @fragmentDensityMapAttachment@ /must/ not be an element of
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription'::@pInputAttachments@,
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription'::@pColorAttachments@,
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription'::@pResolveAttachments@,
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription'::@pDepthStencilAttachment@,
--     or
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription'::@pPreserveAttachments@
--     for any subpass
--
-- -   If @fragmentDensityMapAttachment@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', @layout@
--     /must/ be equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT',
--     or 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   If @fragmentDensityMapAttachment@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED',
--     @fragmentDensityMapAttachment@ /must/ reference an attachment with a
--     @loadOp@ equal to
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_LOAD' or
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_DONT_CARE'.
--
-- -   If @fragmentDensityMapAttachment@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED',
--     @fragmentDensityMapAttachment@ /must/ reference an attachment with a
--     @storeOp@ equal to
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_STORE_OP_DONT_CARE'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT'
--
-- -   @fragmentDensityMapAttachment@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' structure
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data RenderPassFragmentDensityMapCreateInfoEXT = RenderPassFragmentDensityMapCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "RenderPassFragmentDensityMapCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassFragmentDensityMapCreateInfoEXT" "fragmentDensityMapAttachment"
  fragmentDensityMapAttachment :: AttachmentReference
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkRenderPassFragmentDensityMapCreateInfoEXT' and
-- marshal a 'RenderPassFragmentDensityMapCreateInfoEXT' into it. The 'VkRenderPassFragmentDensityMapCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructRenderPassFragmentDensityMapCreateInfoEXT :: RenderPassFragmentDensityMapCreateInfoEXT -> (VkRenderPassFragmentDensityMapCreateInfoEXT -> IO a) -> IO a
withCStructRenderPassFragmentDensityMapCreateInfoEXT marshalled cont = withCStructAttachmentReference (fragmentDensityMapAttachment (marshalled :: RenderPassFragmentDensityMapCreateInfoEXT)) (\fragmentDensityMapAttachment'' -> maybeWith withSomeVkStruct (next (marshalled :: RenderPassFragmentDensityMapCreateInfoEXT)) (\pPNext -> cont (VkRenderPassFragmentDensityMapCreateInfoEXT VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT pPNext fragmentDensityMapAttachment'')))

-- | A function to read a 'VkRenderPassFragmentDensityMapCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'RenderPassFragmentDensityMapCreateInfoEXT'.
fromCStructRenderPassFragmentDensityMapCreateInfoEXT :: VkRenderPassFragmentDensityMapCreateInfoEXT -> IO RenderPassFragmentDensityMapCreateInfoEXT
fromCStructRenderPassFragmentDensityMapCreateInfoEXT c = RenderPassFragmentDensityMapCreateInfoEXT <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRenderPassFragmentDensityMapCreateInfoEXT)))
                                                                                                   <*> (fromCStructAttachmentReference (vkFragmentDensityMapAttachment (c :: VkRenderPassFragmentDensityMapCreateInfoEXT)))

instance Zero RenderPassFragmentDensityMapCreateInfoEXT where
  zero = RenderPassFragmentDensityMapCreateInfoEXT Nothing
                                                   zero


-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME"
pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME = VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION"
pattern EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION :: Integral a => a
pattern EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION = VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
