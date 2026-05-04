-- | Vulkan initialization glue for GLFW windows. Compose with
-- 'Vulkan.Utils.Init.withVulkanInstance' (or just call 'withInstance' here)
-- and the rest of @vulkan-utils@ to get a ready-to-render setup.
module Vulkan.Utils.Init.GLFW
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

import           Control.Exception                ( throwIO )
import           Control.Monad                    ( when )
import           Control.Monad.IO.Class           ( MonadIO, liftIO )
import           Control.Monad.Trans.Resource     ( MonadResource, allocate )
import qualified Data.ByteString                 as BS
import           Data.ByteString                  ( ByteString )
import qualified Data.Vector                     as V
import           Data.Vector                      ( Vector )
import           Data.Int                         ( Int32 )
import           Foreign.Marshal.Alloc            ( alloca )
import           Foreign.Ptr                      ( nullPtr )
import           Foreign.Storable                 ( peek )
import qualified Graphics.UI.GLFW                as GLFW
import           Vulkan.Core10                    ( Instance, ApplicationInfo
                                                  , instanceHandle )
import           Vulkan.Core10.Enums.Result       ( Result(..) )
import           Vulkan.Exception                 ( VulkanException(..) )
import           Vulkan.Extensions.VK_KHR_surface ( SurfaceKHR(..)
                                                  , destroySurfaceKHR )
import           Vulkan.Extensions.VK_KHR_swapchain
                                                  ( pattern KHR_SWAPCHAIN_EXTENSION_NAME )
import           Vulkan.Requirement               ( InstanceRequirement )
import           Vulkan.Utils.Initialization      ( withVulkanInstance )

-- | Vulkan instance extensions GLFW requires. The window argument is unused
-- (GLFW's API is global) but kept for symmetry with the SDL2 module.
getRequiredInstanceExtensions :: MonadIO m => GLFW.Window -> m (Vector ByteString)
getRequiredInstanceExtensions _ = liftIO $
  V.fromList <$> (traverse BS.packCString =<< GLFW.getRequiredInstanceExtensions)

-- | Device extensions a GLFW-presenting application needs. Currently just
-- @VK_KHR_swapchain@.
getRequiredDeviceExtensions :: [ByteString]
getRequiredDeviceExtensions = [KHR_SWAPCHAIN_EXTENSION_NAME]

-- | Create a 'SurfaceKHR' for the given GLFW window. Throws 'VulkanException'
-- if GLFW reports a non-success result.
createSurface :: Instance -> GLFW.Window -> IO SurfaceKHR
createSurface inst w = alloca $ \surfPtr -> do
  r <- GLFW.createWindowSurface (instanceHandle inst) w nullPtr surfPtr :: IO Int32
  let result = Result r
  when (result /= SUCCESS) (throwIO (VulkanException result))
  peek surfPtr

-- | Destroy a 'SurfaceKHR' previously created with 'createSurface'.
destroySurface :: Instance -> SurfaceKHR -> IO ()
destroySurface inst s = destroySurfaceKHR inst s Nothing

-- | Bracketed surface creation in 'MonadResource'.
withSurface :: MonadResource m => Instance -> GLFW.Window -> m SurfaceKHR
withSurface inst w =
  snd <$> allocate (createSurface inst w) (destroySurface inst)

-- | Build a Vulkan 'Instance' wired up with GLFW's required extensions.
-- Composes 'getRequiredInstanceExtensions' and
-- 'Vulkan.Utils.Init.withVulkanInstance'.
withInstance
  :: MonadResource m
  => GLFW.Window
  -> Maybe ApplicationInfo
  -> [InstanceRequirement]
  -> [InstanceRequirement]
  -> m Instance
withInstance w appInfo reqs optReqs = do
  exts <- getRequiredInstanceExtensions w
  withVulkanInstance exts appInfo reqs optReqs
