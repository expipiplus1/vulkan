{-| GLFW windowing helpers used by the @glfw@ triangle example. The
generic helpers (window creation, drawable size, event polling) live
upstream in "Vulkan.Utils.Init.GLFW.Window"; this module adds the
'createSurface' wrapper that the example's 'WindowAdapter' shape
expects.
-}
module Window.GLFW
  ( withGLFW
  , createWindow
  , createSurface
  , showWindow
  , drawableSize
  , shouldQuit
  , glfwAdapter
  ) where

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import qualified Graphics.UI.GLFW as GLFW
import Vulkan.Core10 (Instance)
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR)
import qualified Vulkan.Utils.Init.GLFW as Init
import Vulkan.Utils.Init.GLFW.Window
  ( createWindow
  , drawableSize
  , shouldQuit
  , showWindow
  , withGLFW
  )
import WindowedBoot (WindowAdapter (..))

{- | Bracketed surface creation. Mirrors 'Window.SDL2.createSurface' so
callers can swap backends with a one-line module change.
-}
createSurface
  :: (MonadResource m) => Instance -> GLFW.Window -> m (ReleaseKey, SurfaceKHR)
createSurface inst window =
  allocate (Init.createSurface inst window) (Init.destroySurface inst)

-- | Bridge for 'WindowedBoot.withWindowedVk'.
glfwAdapter :: (MonadResource m) => GLFW.Window -> WindowAdapter m
glfwAdapter w =
  WindowAdapter
    { waWithInstance = Init.withInstance w
    , waWithSurface = \i -> createSurface i w
    , waDrawableSize = drawableSize w
    }
