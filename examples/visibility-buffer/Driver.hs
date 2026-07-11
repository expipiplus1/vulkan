{-# LANGUAGE OverloadedLists #-}

-- | Command-pool and command-buffer plumbing shared by both drivers.
module Driver
  ( commandPool
  , beginPrimary
  , oneShot
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import qualified Data.Vector as V
import Data.Word (Word32)
import HeadlessBoot (submitAndWaitFor)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)

commandPool :: Vk.Device -> Word32 -> ResourceT IO Vk.CommandPool
commandPool dev family = do
  (_, pool) <- Vk.withCommandPool dev zero{CommandPoolCreateInfo.queueFamilyIndex = family} Nothing allocate
  pure pool

-- | Allocate a primary command buffer from the pool and begin it, one-time-submit.
beginPrimary :: Vk.Device -> Vk.CommandPool -> ResourceT IO Vk.CommandBuffer
beginPrimary dev pool = do
  (_, cbs) <-
    Vk.withCommandBuffers
      dev
      zero{Vk.commandPool = pool, Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY, Vk.commandBufferCount = 1}
      allocate
  let cb = V.head cbs
  Vk.beginCommandBuffer cb zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
  pure cb

-- | Record @record@ into a fresh primary buffer and submit it, blocking until done.
oneShot :: Vk.Device -> (Vk.Queue, Word32) -> (Vk.CommandBuffer -> ResourceT IO ()) -> ResourceT IO ()
oneShot dev (queue, family) record = do
  pool <- commandPool dev family
  cb <- beginPrimary dev pool
  record cb
  Vk.endCommandBuffer cb
  -- Generous budget: the setup one-shot carries the voxel + knot generation.
  submitAndWaitFor (30 * 1000 * 1000 * 1000) dev queue cb "Timed out in a oneShot submit"
