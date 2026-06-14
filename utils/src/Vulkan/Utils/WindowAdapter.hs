{-| Bridge between a boot sequence and a particular window-library
backend. A boot helper written against 'WindowAdapter' stays oblivious
to which library is in use; the window packages each provide a
constructor for their own window type — @glfwAdapter@ in
"Vulkan.Utils.Init.GLFW.Window" (@vulkan-init-glfw@) and @sdl2Adapter@
in "Vulkan.Utils.Init.SDL2.Window" (@vulkan-init-sdl2@).
-}
module Vulkan.Utils.WindowAdapter
  ( WindowAdapter (..)
  ) where

import qualified Vulkan.Core10 as Vk
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR)
import Vulkan.Requirement (InstanceRequirement)

-- | The window-library operations a boot sequence needs.
data WindowAdapter m = WindowAdapter
  { waWithInstance
      :: Maybe Vk.ApplicationInfo
      -> [InstanceRequirement]
      -> [InstanceRequirement]
      -> m Vk.Instance
  {- ^ Create an instance satisfying the window library's requirements
  plus the given required and optional ones.
  -}
  , waWithSurface :: Vk.Instance -> m SurfaceKHR
  -- ^ Create a surface for the window, destroyed with the resource scope.
  , waDrawableSize :: m Vk.Extent2D
  -- ^ The window's current drawable size, for the swapchain extent.
  }
