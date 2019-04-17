{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_depth_stencil_resolve
  ( withCStructPhysicalDeviceDepthStencilResolvePropertiesKHR
  , fromCStructPhysicalDeviceDepthStencilResolvePropertiesKHR
  , PhysicalDeviceDepthStencilResolvePropertiesKHR(..)
  , ResolveModeFlagBitsKHR
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


-- No documentation found for TopLevel "PhysicalDeviceDepthStencilResolvePropertiesKHR"
data PhysicalDeviceDepthStencilResolvePropertiesKHR = PhysicalDeviceDepthStencilResolvePropertiesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "supportedDepthResolveModes"
  vkSupportedDepthResolveModes :: ResolveModeFlagsKHR
  , -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "supportedStencilResolveModes"
  vkSupportedStencilResolveModes :: ResolveModeFlagsKHR
  , -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "independentResolveNone"
  vkIndependentResolveNone :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "independentResolve"
  vkIndependentResolve :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceDepthStencilResolvePropertiesKHR :: PhysicalDeviceDepthStencilResolvePropertiesKHR -> (VkPhysicalDeviceDepthStencilResolvePropertiesKHR -> IO a) -> IO a
withCStructPhysicalDeviceDepthStencilResolvePropertiesKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceDepthStencilResolvePropertiesKHR)) (\pPNext -> cont (VkPhysicalDeviceDepthStencilResolvePropertiesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR pPNext (vkSupportedDepthResolveModes (from :: PhysicalDeviceDepthStencilResolvePropertiesKHR)) (vkSupportedStencilResolveModes (from :: PhysicalDeviceDepthStencilResolvePropertiesKHR)) (boolToBool32 (vkIndependentResolveNone (from :: PhysicalDeviceDepthStencilResolvePropertiesKHR))) (boolToBool32 (vkIndependentResolve (from :: PhysicalDeviceDepthStencilResolvePropertiesKHR)))))
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
-- No documentation found for TopLevel "ResolveModeFlagBitsKHR"
type ResolveModeFlagBitsKHR = VkResolveModeFlagBitsKHR
-- No documentation found for TopLevel "ResolveModeFlagsKHR"
type ResolveModeFlagsKHR = ResolveModeFlagBitsKHR
-- No documentation found for TopLevel "SubpassDescriptionDepthStencilResolveKHR"
data SubpassDescriptionDepthStencilResolveKHR = SubpassDescriptionDepthStencilResolveKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "SubpassDescriptionDepthStencilResolveKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SubpassDescriptionDepthStencilResolveKHR" "depthResolveMode"
  vkDepthResolveMode :: ResolveModeFlagBitsKHR
  , -- No documentation found for Nested "SubpassDescriptionDepthStencilResolveKHR" "stencilResolveMode"
  vkStencilResolveMode :: ResolveModeFlagBitsKHR
  , -- No documentation found for Nested "SubpassDescriptionDepthStencilResolveKHR" "pDepthStencilResolveAttachment"
  vkPDepthStencilResolveAttachment :: Maybe AttachmentReference2KHR
  }
  deriving (Show, Eq)
withCStructSubpassDescriptionDepthStencilResolveKHR :: SubpassDescriptionDepthStencilResolveKHR -> (VkSubpassDescriptionDepthStencilResolveKHR -> IO a) -> IO a
withCStructSubpassDescriptionDepthStencilResolveKHR from cont = maybeWith (\a -> withCStructAttachmentReference2KHR a . flip with) (vkPDepthStencilResolveAttachment (from :: SubpassDescriptionDepthStencilResolveKHR)) (\pDepthStencilResolveAttachment -> maybeWith withSomeVkStruct (vkPNext (from :: SubpassDescriptionDepthStencilResolveKHR)) (\pPNext -> cont (VkSubpassDescriptionDepthStencilResolveKHR VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR pPNext (vkDepthResolveMode (from :: SubpassDescriptionDepthStencilResolveKHR)) (vkStencilResolveMode (from :: SubpassDescriptionDepthStencilResolveKHR)) pDepthStencilResolveAttachment)))
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
