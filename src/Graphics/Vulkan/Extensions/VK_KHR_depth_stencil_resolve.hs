{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_KHR_depth_stencil_resolve
  ( withCStructPhysicalDeviceDepthStencilResolvePropertiesKHR
  , fromCStructPhysicalDeviceDepthStencilResolvePropertiesKHR
  , PhysicalDeviceDepthStencilResolvePropertiesKHR(..)
  , ResolveModeFlagBitsKHR
  , pattern RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR
  , pattern RESOLVE_MODE_AVERAGE_BIT_KHR
  , pattern RESOLVE_MODE_MIN_BIT_KHR
  , pattern RESOLVE_MODE_MAX_BIT_KHR
  , pattern RESOLVE_MODE_NONE_KHR
  , ResolveModeFlagsKHR
  , withCStructSubpassDescriptionDepthStencilResolveKHR
  , fromCStructSubpassDescriptionDepthStencilResolveKHR
  , SubpassDescriptionDepthStencilResolveKHR(..)
  , pattern VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION
  , pattern VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR
  ) where

import Control.Monad
  ( (<=<)
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve
  ( VkPhysicalDeviceDepthStencilResolvePropertiesKHR(..)
  , VkResolveModeFlagBitsKHR(..)
  , VkSubpassDescriptionDepthStencilResolveKHR(..)
  , pattern VK_RESOLVE_MODE_AVERAGE_BIT_KHR
  , pattern VK_RESOLVE_MODE_MAX_BIT_KHR
  , pattern VK_RESOLVE_MODE_MIN_BIT_KHR
  , pattern VK_RESOLVE_MODE_NONE_KHR
  , pattern VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2
  ( AttachmentReference2KHR(..)
  , fromCStructAttachmentReference2KHR
  , withCStructAttachmentReference2KHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve
  ( pattern VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME
  , pattern VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION
  )



-- | VkPhysicalDeviceDepthStencilResolvePropertiesKHR - Structure describing
-- depth\/stencil resolve properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkPhysicalDeviceDepthStencilResolvePropertiesKHR'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- Unresolved directive in
-- VkPhysicalDeviceDepthStencilResolvePropertiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceDepthStencilResolvePropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceDepthStencilResolvePropertiesKHR = PhysicalDeviceDepthStencilResolvePropertiesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "supportedDepthResolveModes"
  supportedDepthResolveModes :: ResolveModeFlagsKHR
  , -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "supportedStencilResolveModes"
  supportedStencilResolveModes :: ResolveModeFlagsKHR
  , -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "independentResolveNone"
  independentResolveNone :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "independentResolve"
  independentResolve :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceDepthStencilResolvePropertiesKHR' and
-- marshal a 'PhysicalDeviceDepthStencilResolvePropertiesKHR' into it. The 'VkPhysicalDeviceDepthStencilResolvePropertiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceDepthStencilResolvePropertiesKHR :: PhysicalDeviceDepthStencilResolvePropertiesKHR -> (VkPhysicalDeviceDepthStencilResolvePropertiesKHR -> IO a) -> IO a
withCStructPhysicalDeviceDepthStencilResolvePropertiesKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceDepthStencilResolvePropertiesKHR)) (\pPNext -> cont (VkPhysicalDeviceDepthStencilResolvePropertiesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR pPNext (supportedDepthResolveModes (marshalled :: PhysicalDeviceDepthStencilResolvePropertiesKHR)) (supportedStencilResolveModes (marshalled :: PhysicalDeviceDepthStencilResolvePropertiesKHR)) (boolToBool32 (independentResolveNone (marshalled :: PhysicalDeviceDepthStencilResolvePropertiesKHR))) (boolToBool32 (independentResolve (marshalled :: PhysicalDeviceDepthStencilResolvePropertiesKHR)))))

-- | A function to read a 'VkPhysicalDeviceDepthStencilResolvePropertiesKHR' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceDepthStencilResolvePropertiesKHR'.
fromCStructPhysicalDeviceDepthStencilResolvePropertiesKHR :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR -> IO PhysicalDeviceDepthStencilResolvePropertiesKHR
fromCStructPhysicalDeviceDepthStencilResolvePropertiesKHR c = PhysicalDeviceDepthStencilResolvePropertiesKHR <$> -- Univalued Member elided
                                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR)))
                                                                                                             <*> pure (vkSupportedDepthResolveModes (c :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
                                                                                                             <*> pure (vkSupportedStencilResolveModes (c :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
                                                                                                             <*> pure (bool32ToBool (vkIndependentResolveNone (c :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR)))
                                                                                                             <*> pure (bool32ToBool (vkIndependentResolve (c :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR)))

instance Zero PhysicalDeviceDepthStencilResolvePropertiesKHR where
  zero = PhysicalDeviceDepthStencilResolvePropertiesKHR Nothing
                                                        zero
                                                        zero
                                                        False
                                                        False


-- | VkResolveModeFlagBitsKHR - Bitmask indicating supported depth and
-- stencil resolve modes
--
-- = See Also
--
-- No cross-references are available
type ResolveModeFlagBitsKHR = VkResolveModeFlagBitsKHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR'
-- indicates that result of the resolve operation is equal to the value of
-- sample 0.
pattern RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR :: (a ~ ResolveModeFlagBitsKHR) => a
pattern RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR = VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VK_RESOLVE_MODE_AVERAGE_BIT_KHR'
-- indicates that result of the resolve operation is the average of the
-- sample values.
pattern RESOLVE_MODE_AVERAGE_BIT_KHR :: (a ~ ResolveModeFlagBitsKHR) => a
pattern RESOLVE_MODE_AVERAGE_BIT_KHR = VK_RESOLVE_MODE_AVERAGE_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VK_RESOLVE_MODE_MIN_BIT_KHR'
-- indicates that result of the resolve operation is the minimum of the
-- sample values.
pattern RESOLVE_MODE_MIN_BIT_KHR :: (a ~ ResolveModeFlagBitsKHR) => a
pattern RESOLVE_MODE_MIN_BIT_KHR = VK_RESOLVE_MODE_MIN_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VK_RESOLVE_MODE_MAX_BIT_KHR'
-- indicates that result of the resolve operation is the maximum of the
-- sample values.
pattern RESOLVE_MODE_MAX_BIT_KHR :: (a ~ ResolveModeFlagBitsKHR) => a
pattern RESOLVE_MODE_MAX_BIT_KHR = VK_RESOLVE_MODE_MAX_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VK_RESOLVE_MODE_NONE_KHR'
-- indicates that no resolve operation is done.
pattern RESOLVE_MODE_NONE_KHR :: (a ~ ResolveModeFlagBitsKHR) => a
pattern RESOLVE_MODE_NONE_KHR = VK_RESOLVE_MODE_NONE_KHR

-- | VkResolveModeFlagsKHR - Bitmask of VkResolveModeFlagBitsKHR
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkResolveModeFlagsKHR'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkResolveModeFlagBitsKHR'.
--
-- = See Also
--
-- No cross-references are available
type ResolveModeFlagsKHR = ResolveModeFlagBitsKHR


-- | VkSubpassDescriptionDepthStencilResolveKHR - Structure specifying
-- depth\/stencil resolve operations for a subpass
--
-- == Valid Usage
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED',
--     @pDepthStencilAttachment@ /must/ not have the value
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED',
--     @depthResolveMode@ and @stencilResolveMode@ /must/ not both be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VK_RESOLVE_MODE_NONE_KHR'
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED',
--     @pDepthStencilAttachment@ /must/ not have a sample count of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED',
--     @pDepthStencilResolveAttachment@ /must/ have a sample count of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'
--     then it /must/ have a format whose features contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   If the 'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     @pDepthStencilResolveAttachment@ has a depth component, then the
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     @pDepthStencilAttachment@ /must/ have a depth component with the
--     same number of bits and numerical type
--
-- -   If the 'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     @pDepthStencilResolveAttachment@ has a stencil component, then the
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     @pDepthStencilAttachment@ /must/ have a stencil component with the
--     same number of bits and numerical type
--
-- -   The value of @depthResolveMode@ /must/ be one of the bits set in
--     'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkPhysicalDeviceDepthStencilResolvePropertiesKHR'::@supportedDepthResolveModes@
--     or
--     'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VK_RESOLVE_MODE_NONE_KHR'
--
-- -   The value of @stencilResolveMode@ /must/ be one of the bits set in
--     'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkPhysicalDeviceDepthStencilResolvePropertiesKHR'::@supportedStencilResolveModes@
--     or
--     'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VK_RESOLVE_MODE_NONE_KHR'
--
-- -   If the 'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     @pDepthStencilResolveAttachment@ has both depth and stencil
--     components,
--     'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkPhysicalDeviceDepthStencilResolvePropertiesKHR'::@independentResolve@
--     is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', and
--     'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkPhysicalDeviceDepthStencilResolvePropertiesKHR'::@independentResolveNone@
--     is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then the values of
--     @depthResolveMode@ and @stencilResolveMode@ /must/ be identical
--
-- -   If the 'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     @pDepthStencilResolveAttachment@ has both depth and stencil
--     components,
--     'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkPhysicalDeviceDepthStencilResolvePropertiesKHR'::@independentResolve@
--     is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE' and
--     'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkPhysicalDeviceDepthStencilResolvePropertiesKHR'::@independentResolveNone@
--     is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', then the values of
--     @depthResolveMode@ and @stencilResolveMode@ /must/ be identical or
--     one of them /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VK_RESOLVE_MODE_NONE_KHR'
--
-- Unresolved directive in VkSubpassDescriptionDepthStencilResolveKHR.txt -
-- include::{generated}\/validity\/structs\/VkSubpassDescriptionDepthStencilResolveKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data SubpassDescriptionDepthStencilResolveKHR = SubpassDescriptionDepthStencilResolveKHR
  { -- Univalued member elided
  -- No documentation found for Nested "SubpassDescriptionDepthStencilResolveKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SubpassDescriptionDepthStencilResolveKHR" "depthResolveMode"
  depthResolveMode :: ResolveModeFlagBitsKHR
  , -- No documentation found for Nested "SubpassDescriptionDepthStencilResolveKHR" "stencilResolveMode"
  stencilResolveMode :: ResolveModeFlagBitsKHR
  , -- No documentation found for Nested "SubpassDescriptionDepthStencilResolveKHR" "pDepthStencilResolveAttachment"
  depthStencilResolveAttachment :: Maybe AttachmentReference2KHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSubpassDescriptionDepthStencilResolveKHR' and
-- marshal a 'SubpassDescriptionDepthStencilResolveKHR' into it. The 'VkSubpassDescriptionDepthStencilResolveKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSubpassDescriptionDepthStencilResolveKHR :: SubpassDescriptionDepthStencilResolveKHR -> (VkSubpassDescriptionDepthStencilResolveKHR -> IO a) -> IO a
withCStructSubpassDescriptionDepthStencilResolveKHR marshalled cont = maybeWith (\a -> withCStructAttachmentReference2KHR a . flip with) (depthStencilResolveAttachment (marshalled :: SubpassDescriptionDepthStencilResolveKHR)) (\pPDepthStencilResolveAttachment -> maybeWith withSomeVkStruct (next (marshalled :: SubpassDescriptionDepthStencilResolveKHR)) (\pPNext -> cont (VkSubpassDescriptionDepthStencilResolveKHR VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR pPNext (depthResolveMode (marshalled :: SubpassDescriptionDepthStencilResolveKHR)) (stencilResolveMode (marshalled :: SubpassDescriptionDepthStencilResolveKHR)) pPDepthStencilResolveAttachment)))

-- | A function to read a 'VkSubpassDescriptionDepthStencilResolveKHR' and all additional
-- structures in the pointer chain into a 'SubpassDescriptionDepthStencilResolveKHR'.
fromCStructSubpassDescriptionDepthStencilResolveKHR :: VkSubpassDescriptionDepthStencilResolveKHR -> IO SubpassDescriptionDepthStencilResolveKHR
fromCStructSubpassDescriptionDepthStencilResolveKHR c = SubpassDescriptionDepthStencilResolveKHR <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSubpassDescriptionDepthStencilResolveKHR)))
                                                                                                 <*> pure (vkDepthResolveMode (c :: VkSubpassDescriptionDepthStencilResolveKHR))
                                                                                                 <*> pure (vkStencilResolveMode (c :: VkSubpassDescriptionDepthStencilResolveKHR))
                                                                                                 <*> maybePeek (fromCStructAttachmentReference2KHR <=< peek) (vkPDepthStencilResolveAttachment (c :: VkSubpassDescriptionDepthStencilResolveKHR))

instance Zero SubpassDescriptionDepthStencilResolveKHR where
  zero = SubpassDescriptionDepthStencilResolveKHR Nothing
                                                  zero
                                                  zero
                                                  Nothing

