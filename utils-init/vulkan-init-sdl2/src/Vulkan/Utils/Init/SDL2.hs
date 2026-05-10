{-| Vulkan initialization glue for SDL2 windows. Compose with
'Vulkan.Utils.Init.withVulkanInstance' (or just call 'withInstance' here)
and the rest of @vulkan-utils@ to get a ready-to-render setup.
-}
module Vulkan.Utils.Init.SDL2
  ( -- * Required extensions
    getRequiredInstanceExtensions
  , getRequiredDeviceExtensions

    -- * Surface
  , createSurface
  , destroySurface
  , withSurface

    -- * Instance
  , withInstance
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Vector (Vector)
import qualified Data.Vector as V
import Foreign.Ptr (castPtr)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Vulkan.Core10
  ( ApplicationInfo
  , Instance
  , instanceHandle
  )
import Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR (..)
  , destroySurfaceKHR
  )
import Vulkan.Extensions.VK_KHR_swapchain
  ( pattern KHR_SWAPCHAIN_EXTENSION_NAME
  )
import Vulkan.Requirement (InstanceRequirement)
import Vulkan.Utils.Initialization (withVulkanInstance)

-- | Vulkan instance extensions the SDL2 window requires for presentation.
getRequiredInstanceExtensions :: (MonadIO m) => SDL.Window -> m (Vector ByteString)
getRequiredInstanceExtensions w =
  liftIO $
    V.fromList <$> (traverse BS.packCString =<< SDL.vkGetInstanceExtensions w)

{- | Device extensions an SDL2-presenting application needs. Currently just
@VK_KHR_swapchain@.
-}
getRequiredDeviceExtensions :: [ByteString]
getRequiredDeviceExtensions = [KHR_SWAPCHAIN_EXTENSION_NAME]

-- | Create a 'SurfaceKHR' for the given SDL window.
createSurface :: Instance -> SDL.Window -> IO SurfaceKHR
createSurface inst w =
  SurfaceKHR <$> SDL.vkCreateSurface w (castPtr (instanceHandle inst))

-- | Destroy a 'SurfaceKHR' previously created with 'createSurface'.
destroySurface :: Instance -> SurfaceKHR -> IO ()
destroySurface inst s = destroySurfaceKHR inst s Nothing

-- | Bracketed surface creation in 'MonadResource'.
withSurface :: (MonadResource m) => Instance -> SDL.Window -> m SurfaceKHR
withSurface inst w =
  snd <$> allocate (createSurface inst w) (destroySurface inst)

{- | Build a Vulkan 'Instance' wired up with the SDL window's required
extensions. Composes 'getRequiredInstanceExtensions' and
'Vulkan.Utils.Init.withVulkanInstance'.
-}
withInstance
  :: (MonadResource m)
  => SDL.Window
  -> Maybe ApplicationInfo
  -> [InstanceRequirement]
  -> [InstanceRequirement]
  -> m Instance
withInstance w appInfo reqs optReqs = do
  exts <- getRequiredInstanceExtensions w
  withVulkanInstance exts appInfo reqs optReqs
