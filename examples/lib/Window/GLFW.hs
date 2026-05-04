-- | GLFW windowing helpers used by the @glfw@ triangle example. Mirrors
-- the SDL2 helpers in "Window".
module Window.GLFW
  ( withGLFW
  , createWindow
  , showWindow
  , shouldQuit
  ) where

import           Control.Monad                  ( unless, void )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Resource   ( MonadResource
                                                , allocate
                                                , allocate_
                                                )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Graphics.UI.GLFW              as GLFW

-- | Initialise GLFW and tear it down with the resource scope.
withGLFW :: MonadResource m => m ()
withGLFW = void $ allocate_ initGLFW GLFW.terminate
 where
  initGLFW = do
    ok <- GLFW.init
    unless ok (fail "GLFW.init failed")

-- | Create a GLFW window configured for Vulkan rendering. The window is
-- created hidden so the caller can call 'showWindow' once the swapchain is
-- ready.
createWindow
  :: MonadResource m
  => Text  -- ^ Title
  -> Int   -- ^ Width
  -> Int   -- ^ Height
  -> m GLFW.Window
createWindow title width height = do
  liftIO $ do
    GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
    GLFW.windowHint (GLFW.WindowHint'Resizable True)
    GLFW.windowHint (GLFW.WindowHint'Visible False)
  (_, mWin) <- allocate
    (GLFW.createWindow width height (T.unpack title) Nothing Nothing)
    (maybe (pure ()) GLFW.destroyWindow)
  case mWin of
    Just w  -> pure w
    Nothing -> liftIO (fail "GLFW.createWindow returned Nothing")

showWindow :: GLFW.Window -> IO ()
showWindow = GLFW.showWindow

-- | Poll events and report whether the user requested to close the window
-- (X button, Q, or Escape).
shouldQuit :: GLFW.Window -> IO Bool
shouldQuit win = do
  GLFW.pollEvents
  closeRequested <- GLFW.windowShouldClose win
  qPressed       <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey win GLFW.Key'Q
  escPressed     <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey win GLFW.Key'Escape
  pure (closeRequested || qPressed || escPressed)
