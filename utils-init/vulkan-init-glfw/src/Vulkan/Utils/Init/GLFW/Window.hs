{-| Convenience helpers for opening a GLFW window suitable for Vulkan
rendering, polling for close/quit events, and querying the current
framebuffer size for swapchain recreation.

These wrap a small set of opinionated defaults — no client API,
resizable, hidden until the caller has the swapchain ready — sufficient
for examples and prototypes. Applications with different needs should
call GLFW directly.
-}
module Vulkan.Utils.Init.GLFW.Window
  ( withGLFW
  , createWindow
  , drawableSize
  , showWindow
  , shouldQuit
  , glfwAdapter
  ) where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, allocate, allocate_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import Vulkan.Core10 (Extent2D (..))
import qualified Vulkan.Utils.Init.GLFW as Init
import Vulkan.Utils.WindowAdapter (WindowAdapter (..))

-- | Initialise GLFW and tear it down with the resource scope.
withGLFW :: (MonadResource m) => m ()
withGLFW = void $ allocate_ initGLFW GLFW.terminate
  where
    initGLFW = do
      ok <- GLFW.init
      unless ok (fail "GLFW.init failed")

{- | Create a GLFW window configured for Vulkan rendering. The window is
created hidden so the caller can call 'showWindow' once the swapchain is
ready.
-}
createWindow
  :: (MonadResource m)
  => Text
  -- ^ Title
  -> Int
  -- ^ Width
  -> Int
  -- ^ Height
  -> m GLFW.Window
createWindow title width height = do
  liftIO $ do
    GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
    GLFW.windowHint (GLFW.WindowHint'Resizable True)
    GLFW.windowHint (GLFW.WindowHint'Visible False)
  (_, mWin) <-
    allocate
      (GLFW.createWindow width height (T.unpack title) Nothing Nothing)
      (maybe (pure ()) GLFW.destroyWindow)
  case mWin of
    Just w -> pure w
    Nothing -> liftIO (fail "GLFW.createWindow returned Nothing")

showWindow :: (MonadIO m) => GLFW.Window -> m ()
showWindow = liftIO . GLFW.showWindow

-- | Current framebuffer size, suitable as the swapchain extent fallback.
drawableSize :: (MonadIO m) => GLFW.Window -> m Extent2D
drawableSize win = do
  (w, h) <- liftIO $ GLFW.getFramebufferSize win
  pure $ Extent2D (fromIntegral w) (fromIntegral h)

-- | The window's 'WindowAdapter', for backend-agnostic boot helpers.
glfwAdapter :: (MonadResource m) => GLFW.Window -> WindowAdapter m
glfwAdapter w =
  WindowAdapter
    { waWithInstance = Init.withInstance w
    , waWithSurface = \i -> Init.withSurface i w
    , waDrawableSize = drawableSize w
    }

{- | Poll events and report whether the user requested to close the window
(X button, Q, or Escape).
-}
shouldQuit :: (MonadIO m) => GLFW.Window -> m Bool
shouldQuit win = liftIO $ do
  GLFW.pollEvents
  closeRequested <- GLFW.windowShouldClose win
  qPressed <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey win GLFW.Key'Q
  escPressed <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey win GLFW.Key'Escape
  pure (closeRequested || qPressed || escPressed)
