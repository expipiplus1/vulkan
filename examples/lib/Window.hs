module Window
  ( withSDL
  , createWindow
  , createSurface
  , RefreshLimit(..)
  , shouldQuit
  ) where

import           Control.Monad                  ( void )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Maybe                     ( maybeToList )
import           Data.Text                      ( Text )
import           Foreign.Ptr                    ( castPtr )
import qualified SDL
import qualified SDL.Video.Vulkan              as SDL
import           Vulkan.Core10
import           Vulkan.Extensions.VK_KHR_surface

withSDL :: MonadResource m => m ()
withSDL = void $ allocate_ (SDL.initialize @[] [SDL.InitEvents]) SDL.quit

-- | The caller is responsible to initializing SDL
createWindow
  :: MonadResource m
  => Text
  -- ^ Title
  -> Int
  -- ^ Width
  -> Int
  -- ^ Height
  -> m SDL.Window
createWindow title width height = do
  SDL.initialize @[] [SDL.InitVideo]
  _           <- allocate_ (SDL.vkLoadLibrary Nothing) SDL.vkUnloadLibrary
  (_, window) <- allocate
    (SDL.createWindow
      title
      (SDL.defaultWindow
        { SDL.windowInitialSize     = SDL.V2 (fromIntegral width)
                                             (fromIntegral height)
        , SDL.windowGraphicsContext = SDL.VulkanContext
        , SDL.windowResizable       = True
        , SDL.windowHighDPI         = True
        , SDL.windowVisible         = False
        }
      )
    )
    SDL.destroyWindow
  pure window

createSurface
  :: MonadResource m => Instance -> SDL.Window -> m (ReleaseKey, SurfaceKHR)
createSurface inst window = allocate
  (SurfaceKHR <$> SDL.vkCreateSurface window (castPtr (instanceHandle inst)))
  (\s -> destroySurfaceKHR inst s Nothing)

----------------------------------------------------------------
-- SDL helpers
----------------------------------------------------------------

data RefreshLimit
  = NoLimit
  | TimeLimit Int -- ^ Time in ms
  | EventLimit -- ^ Indefinite timeout

-- | Consumes all events in the queue and reports if any of them instruct the
-- application to quit.
shouldQuit :: MonadIO m => RefreshLimit -> m Bool
shouldQuit limit = any isQuitEvent <$> awaitSDLEvents limit
 where
  isQuitEvent :: SDL.Event -> Bool
  isQuitEvent = \case
    (SDL.Event _ SDL.QuitEvent) -> True
    SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released False (SDL.Keysym _ code _)))
      | code == SDL.KeycodeQ || code == SDL.KeycodeEscape
      -> True
    _ -> False

-- | Return the SDL events which have become available
--
-- Optionally wait for a timeout or forever.
awaitSDLEvents :: MonadIO m => RefreshLimit -> m [SDL.Event]
awaitSDLEvents limit = do
  first <- case limit of
    NoLimit      -> pure Nothing
    TimeLimit ms -> SDL.waitEventTimeout (fromIntegral ms)
    EventLimit   -> Just <$> SDL.waitEvent
  next <- SDL.pollEvents
  pure $ maybeToList first <> next
