{-| Per-frame window loop shared by the windowed examples.

The skeleton — read swapchain, run a frame inside @runFrame@, recreate on
'Swapchain.threwSwapchainError', advance — is identical across triangle,
hlsl, rays, resize. Each example only varies in:

* the per-swapchain state it holds (framebuffers, descriptor sets, …),
* the per-frame render action, and
* the "what to do on exit / per-frame metric" hooks.

'runWindowLoop' takes those four points as fields of a 'WindowLoop' record.
-}
module WindowLoop
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
import Frame (Frame (..), advanceFrame, initialFrame, runFrame)
import GHC.Clock (getMonotonicTimeNSec)
import Swapchain (Swapchain, recreateSwapchain, threwSwapchainError)
import Utils (loopJust)
import VkResources (VkResources)
import qualified Vulkan.Core10 as Vk

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
  :: VkResources
  -> Swapchain
  -> IO Vk.Extent2D
  -- ^ Get current drawable size (called on resize)
  -> IO Bool
  -- ^ Per-frame poller; 'True' means quit
  -> WindowLoop s
  -> ResourceT IO ()
runWindowLoop vr initialSC getSize shouldQuit WindowLoop{..} = do
  initialState <- wlMkState initialSC
  scRef <- liftIO $ newIORef initialSC
  stRef <- liftIO $ newIORef initialState
  initial <- initialFrame vr initialSC
  let
    perFrame f = do
      currentSC <- liftIO $ readIORef scRef
      (st, _) <- liftIO $ readIORef stRef
      let f' = f{fSwapchain = currentSC}
      startNs <- liftIO getMonotonicTimeNSec
      needsNew <-
        threwSwapchainError $
          liftIO $
            runFrame vr f' (wlRender st f')
      endNs <- liftIO getMonotonicTimeNSec
      wlOnFrame startNs endNs
      sc' <-
        if needsNew
          then do
            newSize <- liftIO getSize
            sc' <- recreateSwapchain vr newSize currentSC
            (newSt, newKey) <- wlMkState sc'
            (_, oldKey) <- liftIO $ readIORef stRef
            release oldKey
            liftIO $ writeIORef scRef sc'
            liftIO $ writeIORef stRef (newSt, newKey)
            pure sc'
          else pure currentSC
      advanceFrame vr sc' f'

    loop f =
      liftIO shouldQuit >>= \case
        True -> do
          wlOnExit f
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
