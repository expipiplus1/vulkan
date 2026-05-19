{-| Per-frame window loop shared by windowed applications.

The skeleton — read swapchain, run a frame inside @runFrame@, recreate on
'Vulkan.Utils.Swapchain.threwSwapchainError', advance — is the same for any
windowed Vulkan app. Each consumer only varies in:

* the per-swapchain state it holds (framebuffers, descriptor sets, …),
* the per-frame render action, and
* the "what to do on exit / per-frame metric" hooks.

'runWindowLoop' takes those four points as fields of a 'WindowLoop' record.
-}
module Vulkan.Utils.WindowLoop
  ( WindowLoop (..)
  , runWindowLoop
  , noWindowState
  , noOnFrame
  , noOnExit
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
  ( ReleaseKey
  , ResourceT
  , register
  , release
  )
import Data.IORef
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Frame (Frame (..), advanceFrame, drainFrames, initialFrame, runFrame)
import Vulkan.Utils.Swapchain (Swapchain, recreateSwapchain, threwSwapchainError)
import Vulkan.Utils.VulkanContext (VulkanContext (..))

data WindowLoop s = WindowLoop
  { wlMkState :: Swapchain -> ResourceT IO (s, ReleaseKey)
  {- ^ Build per-swapchain state. The release key is fired when the
  swapchain is recreated and a fresh state replaces this one.
  -}
  , wlRender :: s -> Frame -> ResourceT IO ()
  -- ^ Per-frame render action; runs inside @runFrame@.
  , wlOnFrame :: Word64 -> Word64 -> ResourceT IO ()
  {- ^ Optional metric hook with start/end nanoseconds around 'runFrame'.
  Use 'noOnFrame' if you don't care.
  -}
  , wlOnExit :: Frame -> ResourceT IO ()
  -- ^ Fired once when the window closes. Use 'noOnExit' if you don't care.
  }

runWindowLoop
  :: VulkanContext
  -> Swapchain
  -> IO Vk.Extent2D
  -- ^ Get current drawable size (called on resize)
  -> IO Bool
  -- ^ Per-frame poller; 'True' means quit
  -> WindowLoop s
  -> ResourceT IO ()
runWindowLoop vc initialSC getSize shouldQuit WindowLoop{..} = do
  initialState <- wlMkState initialSC
  scRef <- liftIO $ newIORef initialSC
  stRef <- liftIO $ newIORef initialState
  initial <- initialFrame vc initialSC
  let
    perFrame f = do
      currentSC <- liftIO $ readIORef scRef
      (st, _) <- liftIO $ readIORef stRef
      let f' = f{fSwapchain = currentSC}
      startNs <- liftIO getMonotonicTimeNSec
      needsNew <-
        liftIO . threwSwapchainError $
          runFrame vc f' (wlRender st f')
      endNs <- liftIO getMonotonicTimeNSec
      wlOnFrame startNs endNs
      sc' <-
        if needsNew
          then do
            newSize <- liftIO getSize
            sc' <- recreateSwapchain (vcPhysicalDevice vc) (vcDevice vc) newSize currentSC
            (newSt, newKey) <- wlMkState sc'
            (_, oldKey) <- liftIO $ readIORef stRef
            release oldKey
            liftIO $ writeIORef scRef sc'
            liftIO $ writeIORef stRef (newSt, newKey)
            pure sc'
          else pure currentSC
      advanceFrame vc sc' f'

    loop f =
      liftIO shouldQuit >>= \case
        True -> do
          Vk.deviceWaitIdle (vcDevice vc)
          wlOnExit f
          liftIO $ drainFrames vc f
          pure Nothing
        False -> Just <$> perFrame f
  loopJust loop initial

-- | 'wlMkState' for callers that have no per-swapchain state.
noWindowState :: Swapchain -> ResourceT IO ((), ReleaseKey)
noWindowState _ = do
  key <- register (pure ())
  pure ((), key)

noOnFrame :: Word64 -> Word64 -> ResourceT IO ()
noOnFrame _ _ = pure ()

noOnExit :: Frame -> ResourceT IO ()
noOnExit _ = pure ()

loopJust :: (Monad m) => (a -> m (Maybe a)) -> a -> m ()
loopJust f x =
  f x >>= \case
    Nothing -> pure ()
    Just x' -> loopJust f x'
