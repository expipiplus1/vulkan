module Window.SDL2
  ( withSDL
  , createWindow
  , createSurface
  , drawableSize
  , showWindow
  , shouldQuit
  , sdl2Adapter
  ) where

import Control.Monad.Trans.Resource
import Foreign.Ptr (castPtr)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface
import qualified Vulkan.Utils.Init.SDL2 as Init
import Vulkan.Utils.Init.SDL2.Window
  ( createWindow
  , drawableSize
  , shouldQuit
  , showWindow
  , withSDL
  )
import WindowedBoot (WindowAdapter (..))

createSurface
  :: (MonadResource m) => Instance -> SDL.Window -> m (ReleaseKey, SurfaceKHR)
createSurface inst window =
  allocate
    (SurfaceKHR <$> SDL.vkCreateSurface window (castPtr (instanceHandle inst)))
    (\s -> destroySurfaceKHR inst s Nothing)

-- | Bridge for 'WindowedBoot.withWindowedVk'.
sdl2Adapter :: (MonadResource m) => SDL.Window -> WindowAdapter m
sdl2Adapter w =
  WindowAdapter
    { waWithInstance = Init.withInstance w
    , waWithSurface = \i -> createSurface i w
    , waDrawableSize = drawableSize w
    }
