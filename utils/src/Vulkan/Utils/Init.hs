{-# LANGUAGE CPP #-}

-- | Small init helpers for building a Vulkan 'Instance'. These compose with
-- 'Vulkan.Utils.Initialization' and the windowing-backend packages
-- (@vulkan-init-sdl2@, @vulkan-init-glfw@).
module Vulkan.Utils.Init
  ( -- * macOS portability
    portabilityRequirements
  , portabilityFlags
    -- * Instance creation
  , withVulkanInstance
  ) where

import           Control.Monad.Trans.Resource     ( MonadResource )
import           Data.ByteString                  ( ByteString )
import           Data.Vector                      ( Vector )
import           Vulkan.Core10
import           Vulkan.Requirement               ( InstanceRequirement(..) )
import           Vulkan.Utils.Initialization      ( createInstanceFromRequirements )
import           Vulkan.Zero                      ( zero )

#if defined(darwin_HOST_OS)
import           Vulkan.Core10.Enums.InstanceCreateFlagBits
                                                  ( pattern INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR )
import           Vulkan.Extensions.VK_KHR_portability_enumeration
                                                  ( pattern KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME )
#endif

-- | Instance requirements needed on macOS to enumerate non-conformant drivers
-- such as MoltenVK. Empty on every other platform.
portabilityRequirements :: [InstanceRequirement]

-- | Instance create flag bits that pair with 'portabilityRequirements'.
-- 'zero' on every non-macOS platform.
portabilityFlags :: InstanceCreateFlags

#if defined(darwin_HOST_OS)
portabilityRequirements =
  [ RequireInstanceExtension
      { instanceExtensionLayerName  = Nothing
      , instanceExtensionName       = KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
      , instanceExtensionMinVersion = minBound
      }
  ]
portabilityFlags = INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
#else
portabilityRequirements = []
portabilityFlags        = zero
#endif

-- | Build a Vulkan 'Instance' from a backend-supplied extension list plus
-- caller-supplied requirements. Automatically merges 'portabilityRequirements'
-- into the required list and 'portabilityFlags' into the create flags so
-- macOS apps work without per-call plumbing.
--
-- Pass 'mempty' for the extension list when running headless; or call
-- 'Vulkan.Utils.Init.Headless.withInstance' which does so.
withVulkanInstance
  :: MonadResource m
  => Vector ByteString
     -- ^ Backend-required instance extensions (e.g. from
     -- @Vulkan.Utils.Init.SDL2.getRequiredInstanceExtensions@). 'mempty' for
     -- headless.
  -> Maybe ApplicationInfo
  -> [InstanceRequirement]
     -- ^ Caller's required requirements
  -> [InstanceRequirement]
     -- ^ Caller's optional requirements
  -> m Instance
withVulkanInstance exts appInfo reqs optReqs =
  createInstanceFromRequirements
    (portabilityRequirements <> reqs)
    optReqs
    zero
      { applicationInfo       = appInfo
      , enabledExtensionNames = exts
      , flags                 = portabilityFlags
      }
