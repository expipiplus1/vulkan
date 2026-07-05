{-# LANGUAGE OverloadedRecordDot #-}

{-| A single-frame, procedural compute path tracer whose /entire/ shader
interface is derived from the quasiquoted shader at build time by
@vulkan-utils-spirv@ — see "Pathtracer" for the reflected pipeline,
"Pathtracer.Shader" for the shader, "Scene" for the host-built scene and BVH,
and "Render" for the banded dispatch.

The scene (a "Ray Tracing in One Weekend" arrangement) is generated on the
host from a command-line seed: the spheres go into the @Scene@ buffer and a
BVH over them into a separate device-address-linked node buffer.
-}
module Main
  ( main
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), withHeadlessVk)
import ImageReadback (savePng)
import Options.Applicative
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import qualified VulkanMemoryAllocator as VMA

import qualified Pathtracer
import Render (Options (..))
import qualified Render

main :: IO ()
main = do
  opts <- execParser optionsInfo
  runResourceT $ do
    HeadlessVk{..} <-
      withHeadlessVk
        HeadlessConfig
          { appName = "Haskell Vulkan pathtrace-reflect example"
          , instanceReqs = []
          , deviceReqs = Pathtracer.deviceRequirements
          , -- the BVH node buffer is reached by device address
            vmaFlags = VMA.ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT
          }
    let QueueFamilyIndex computeQueueFamilyIndex = fst (qCompute queues)

    image <- Render.render allocator device computeQueueFamilyIndex opts
    Vk.deviceWaitIdle device
    liftIO $ savePng opts.output image
    liftIO $ putStrLn ("Wrote " <> opts.output)

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> header "pathtrace-reflect - reflection-driven compute path tracer"
        <> progDesc
          "Render one frame of a procedural sphere scene with a compute path \
          \tracer whose entire interface is derived from SPIR-V reflection."
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option auto (long "width" <> metavar "N" <> value 600 <> showDefault <> help "Image width")
    <*> option auto (long "height" <> metavar "N" <> value 400 <> showDefault <> help "Image height")
    <*> option auto (long "samples" <> metavar "N" <> value 32 <> showDefault <> help "Samples per pixel (spec constant)")
    <*> option auto (long "bounces" <> metavar "N" <> value 12 <> showDefault <> help "Max ray bounces (spec constant)")
    <*> option auto (long "spheres" <> metavar "N" <> value 64 <> showDefault <> help "Random spheres in the scene")
    <*> option auto (long "seed" <> metavar "N" <> value 1 <> showDefault <> help "Scene and sampling seed")
    <*> option auto (long "fov" <> metavar "DEG" <> value 20 <> showDefault <> help "Vertical field of view")
    <*> option auto (long "timeout" <> metavar "SEC" <> value 60 <> showDefault <> help "GPU wait budget in seconds")
    <*> strOption (long "output" <> metavar "FILE" <> value "pathtrace-reflect.png" <> showDefault <> help "Output PNG path")
