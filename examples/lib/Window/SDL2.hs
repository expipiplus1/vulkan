module Window.SDL2
  ( withSDL
  , createWindow
  , createSurface
  , drawableSize
  , showWindow
  , shouldQuit
  , sdl2Adapter
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Text (Text)
import Foreign.Ptr (castPtr)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface
import qualified Vulkan.Utils.Init.SDL2 as Init
import WindowedBoot (WindowAdapter (..))

withSDL :: (MonadResource m) => m ()
withSDL = void $ allocate_ (SDL.initialize @[] [SDL.InitEvents]) SDL.quit

-- | The caller is responsible to initializing SDL
createWindow
  :: (MonadResource m)
  => Text
  -- ^ Title
  -> Int
  -- ^ Width
  -> Int
  -- ^ Height
  -> m SDL.Window
createWindow title width height = do
  SDL.initialize @[] [SDL.InitVideo]
  _ <- allocate_ (SDL.vkLoadLibrary Nothing) SDL.vkUnloadLibrary
  (_, window) <-
    allocate
      ( SDL.createWindow
          title
          ( SDL.defaultWindow
              { SDL.windowInitialSize =
                  SDL.V2
                    (fromIntegral width)
                    (fromIntegral height)
              , SDL.windowGraphicsContext = SDL.VulkanContext
              , SDL.windowResizable = True
              , SDL.windowHighDPI = True
              , SDL.windowVisible = False
              }
          )
      )
      SDL.destroyWindow
  pure window

createSurface
  :: (MonadResource m) => Instance -> SDL.Window -> m (ReleaseKey, SurfaceKHR)
createSurface inst window =
  allocate
    (SurfaceKHR <$> SDL.vkCreateSurface window (castPtr (instanceHandle inst)))
    (\s -> destroySurfaceKHR inst s Nothing)

-- | Current drawable size, suitable as the swapchain extent fallback.
drawableSize :: (MonadIO m) => SDL.Window -> m Extent2D
drawableSize win = do
  SDL.V2 w h <- SDL.vkGetDrawableSize win
  pure $ Extent2D (fromIntegral w) (fromIntegral h)

{- | Make the window visible. The window is created hidden so the swapchain
can be brought up first.
-}
showWindow :: (MonadIO m) => SDL.Window -> m ()
showWindow = SDL.showWindow

-- | Bridge for 'WindowedBoot.withWindowedVk'.
sdl2Adapter :: (MonadResource m) => SDL.Window -> WindowAdapter m
sdl2Adapter w =
  WindowAdapter
    { waWithInstance = Init.withInstance w
    , waWithSurface = \i -> createSurface i w
    , waDrawableSize = drawableSize w
    }

----------------------------------------------------------------
-- SDL helpers
----------------------------------------------------------------

{- | Poll the event queue and report whether the user requested to quit
(window close, Q, or Escape). The window argument is unused — SDL's event
queue is global — but kept for symmetry with the GLFW backend.
-}
shouldQuit :: (MonadIO m) => SDL.Window -> m Bool
shouldQuit _ = any isQuitEvent <$> SDL.pollEvents
  where
    isQuitEvent :: SDL.Event -> Bool
    isQuitEvent = \case
      (SDL.Event _ SDL.QuitEvent) -> True
      SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released False (SDL.Keysym _ code _)))
        | code == SDL.KeycodeQ || code == SDL.KeycodeEscape ->
            True
      _ -> False
