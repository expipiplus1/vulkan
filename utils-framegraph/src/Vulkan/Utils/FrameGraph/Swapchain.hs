{-| Swapchain images as frame-graph imports.

Every windowed frame graph ends the same way: some pass writes the acquired
swapchain image and it must reach @PRESENT_SRC@ before the present.
'newSwapchainImages' builds the persistent per-image 'ManagedImage' table,
'importSwapchain' imports the acquired image from it each frame, and
'presentSwapchain' appends the terminal pass. Skipping the terminal pass (or
reading instead of writing) presents the image stuck in the last writer's
layout, with nothing in the types to catch it.
-}
module Vulkan.Utils.FrameGraph.Swapchain
  ( newSwapchainImages
  , importSwapchain
  , presentSwapchain
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)

import Fragr qualified as FG
import Vulkan.Core10 qualified as Vk
import Vulkan.Extensions.VK_KHR_surface qualified
import Vulkan.Utils.FrameGraph.Image (ManagedImage, Usage (Present), describedImage, importManagedImage)
import Vulkan.Utils.FrameGraph.Recorder (Recorder)
import Vulkan.Utils.Swapchain (Swapchain (..))

{- | Wrap each swapchain image in a layout-tracked 'ManagedImage'.

The table persists for the swapchain's lifetime — the tracked layouts carry
across frames — so build it once per (re)created swapchain, next to the other
per-swapchain bindings. The images belong to the swapchain; the wrappers need
no release.
-}
newSwapchainImages :: (MonadIO m) => Swapchain -> m (Vector ManagedImage)
newSwapchainImages sc =
  traverse
    (\image -> describedImage sc.sFormat.format sc.sExtent image Vk.IMAGE_ASPECT_COLOR_BIT)
    sc.sImages

{- | Import the acquired image (as @swapchain@) into this frame's graph.

Returns the handle for the graph's writes alongside the wrapper, whose
@.image@ the writing pass records into.
-}
importSwapchain :: (MonadIO m) => FG.FrameGraph Recorder () -> Vector ManagedImage -> Word32 -> m (FG.Handle ManagedImage, ManagedImage)
importSwapchain graph swapImages imageIndex = do
  let mi = swapImages V.! fromIntegral imageIndex
  h <- importManagedImage graph "swapchain" mi
  pure (h, mi)

{- | Terminal present pass over the written swapchain handle.

'FG.finalize' registers a side-effecting pass on the writer's queue, so the
chain survives demand culling and the write hook brings the image to
@PRESENT_SRC@ — a no-op barrier when it is already there (an idle re-present).
-}
presentSwapchain :: (MonadIO m) => FG.FrameGraph Recorder () -> FG.Handle ManagedImage -> m ()
presentSwapchain graph h = FG.finalize graph h Present
