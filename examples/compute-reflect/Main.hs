{-| A compute example whose shader interface is derived from the quasiquoted
shader in "Julia.Shader" at build time by @vulkan-utils-spirv@ — see "Julia"
for the reflected pipeline and "Render" for the frame.
-}
module Main
  ( main
  )
where

import Control.Monad.Trans.Resource (runResourceT)
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), withHeadlessVk)
import ImageReadback (savePng)
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Zero (zero)

import qualified Render

main :: IO ()
main = runResourceT $ do
  HeadlessVk{..} <-
    withHeadlessVk
      HeadlessConfig
        { appName = "Haskell Vulkan compute-reflect example"
        , instanceReqs = []
        , deviceReqs = []
        , vmaFlags = zero
        }
  let QueueFamilyIndex computeQueueFamilyIndex = fst (qCompute queues)

  image <- Render.render allocator device computeQueueFamilyIndex
  Vk.deviceWaitIdle device
  savePng "julia-reflect.png" image
