-- | One-shot setup submits outside the frame graph.
module Driver
  ( oneShot
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import Data.Word (Word32)
import HeadlessBoot (submitAndWaitFor)
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.FrameGraph.Driver (allocateCommandPool, allocatePrimary)

-- | Record @record@ into a fresh primary buffer and submit it, blocking until done.
oneShot :: Vk.Device -> (Vk.Queue, Word32) -> (Vk.CommandBuffer -> ResourceT IO ()) -> ResourceT IO ()
oneShot dev (queue, family) record = do
  pool <- allocateCommandPool dev family
  cb <- allocatePrimary dev pool
  record cb
  Vk.endCommandBuffer cb
  -- Generous budget: the setup one-shot carries the voxel + knot generation.
  submitAndWaitFor (30 * 1000 * 1000 * 1000) dev queue cb "Timed out in a oneShot submit"
