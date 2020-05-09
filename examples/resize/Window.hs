{-# LANGUAGE OverloadedLists #-}

module Window
  ( createWindow
  , createSurface
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString               as BS
import           Data.ByteString                ( ByteString )
import           Data.Text               hiding ( maximum )
import           Foreign.Ptr                    ( castPtr )
import qualified SDL
import qualified SDL.Video.Vulkan              as SDL

import           Vulkan.Core10
import           Vulkan.Extensions.VK_KHR_surface

-- | The caller is responsible to initializing SDL
createWindow
  :: MonadResource m
  => Text
  -- ^ Title
  -> Int
  -- ^ Width
  -> Int
  -- ^ Height
  -> m ([ByteString], SDL.Window)
  -- ^ (Required 'Instance' extensions required to use this window as a
  -- surface, The window)
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
        , SDL.windowVisible         = False
        }
      )
    )
    SDL.destroyWindow
  windowExtensions <-
    liftIO $ traverse BS.packCString =<< SDL.vkGetInstanceExtensions window
  pure (windowExtensions, window)

createSurface :: MonadResource m => Instance -> SDL.Window -> m SurfaceKHR
createSurface inst window = snd <$> allocate
  (SurfaceKHR <$> SDL.vkCreateSurface window (castPtr (instanceHandle inst)))
  (\s -> destroySurfaceKHR inst s Nothing)
