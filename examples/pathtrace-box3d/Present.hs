{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The fullscreen presentation pipeline: a triangle drawing the tracer's texel
buffer straight onto the swapchain image with dynamic rendering — no blit, so
any surface format works.

The tracer's texels are gamma-2.0-encoded, so a UNORM swapchain shows them
exactly as the headless PNGs; an sRGB-only surface would re-encode on
attachment write, which the fragment shader undoes when the @SRGB_SWAPCHAIN@
specialization constant is set (see 'srgbFormat').
-}
module Present
  ( deviceRequirements
  , srgbFormat
  , Pipeline (..)
  , allocatePipeline
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import Data.Word (Word32)
import qualified Vulkan.Core10 as Vk
import Vulkan.Requirement (DeviceRequirement)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.SpirV.Pipeline (allocateGraphicsPipeline, allocateReflectedLayout)
import qualified Vulkan.Utils.SpirV.Pipeline
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Zero (zero)

import qualified Present.Shader as Shader

-- | Dynamic rendering is all the pass needs beyond a color attachment.
deviceRequirements :: [DeviceRequirement]
deviceRequirements = Dynamic.dynamicRenderingRequirements

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , pipelineLayout :: Vk.PipelineLayout
  , setLayouts :: [(Word32, Vk.DescriptorSetLayout)]
  }

{- | The pipeline, targeting the given swapchain format; layout and vertex
input come from reflecting the "Present.Shader" stages.
-}
allocatePipeline :: Vk.Device -> Vk.Format -> ResourceT IO Pipeline
allocatePipeline dev format = do
  vertModule <- reflectBytes Shader.vertCode
  fragModule <- reflectBytes Shader.fragCode
  (_, layout) <- allocateReflectedLayout dev [vertModule, fragModule]
  (_, pipeline) <-
    allocateGraphicsPipeline
      dev
      layout
      zero{Dynamic.colorFormats = [format]}
      (srgbFormat format)
      [(vertModule, Shader.vertCode), (fragModule, Shader.fragCode)]
  pure
    Pipeline
      { pipeline = pipeline
      , pipelineLayout = layout.pipelineLayout
      , setLayouts = layout.setLayouts
      }

{- | Whether attachment writes to the format apply the sRGB transfer encoding —
the present shader must then feed it linear values, not the tracer's
gamma-2.0-encoded ones. Prefer a format this rejects (e.g. via
@scSurfaceFormatPreferences@) for a presentation identical to the headless
PNGs.
-}
srgbFormat :: Vk.Format -> Bool
srgbFormat f = f `elem` srgbFormats
  where
    srgbFormats :: [Vk.Format]
    srgbFormats =
      [ Vk.FORMAT_R8G8B8A8_SRGB
      , Vk.FORMAT_B8G8R8A8_SRGB
      , Vk.FORMAT_A8B8G8R8_SRGB_PACK32
      ]
