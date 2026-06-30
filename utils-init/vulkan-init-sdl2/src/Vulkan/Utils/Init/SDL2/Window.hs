{-# LANGUAGE TypeApplications #-}

{-| Convenience helpers for opening an SDL2 window suitable for Vulkan
rendering, polling for quit events, and querying the current drawable
size for swapchain recreation.

These wrap a small set of opinionated defaults — Vulkan graphics
context, resizable, high-DPI, hidden until the caller has the swapchain
ready — sufficient for examples and prototypes. Applications with
different needs should call SDL directly.
-}
module Vulkan.Utils.Init.SDL2.Window
  ( withSDL
  , createWindow
  , drawableSize
  , showWindow
  , shouldQuit
  , sdl2Adapter
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Text (Text)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Vulkan.Core10 (Extent2D (..))
import qualified Vulkan.Utils.Init.SDL2 as Init
import Vulkan.Utils.WindowAdapter (WindowAdapter (..))

-- | Bring SDL up for the duration of the resource scope.
withSDL :: (MonadResource m) => m ()
withSDL = void $ allocate_ (SDL.initialize @[] [SDL.InitEvents]) SDL.quit

-- | Create an SDL2 window configured for Vulkan rendering.
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

-- | The window's 'WindowAdapter', for backend-agnostic boot helpers.
sdl2Adapter :: (MonadResource m) => SDL.Window -> WindowAdapter m
sdl2Adapter w =
  WindowAdapter
    { waAllocateInstance = Init.allocateInstance w
    , waAllocateSurface = \i -> Init.allocateSurface i w
    , waDrawableSize = drawableSize w
    }

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
