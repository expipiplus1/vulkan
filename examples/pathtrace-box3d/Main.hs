{-# LANGUAGE OverloadedRecordDot #-}

{-| The pathtrace-reflect scene, animated by the Box3D physics engine — see
"Pathtracer" for the reflected tracer pipeline, "Scene" for the sphere scene
and its BVH, "Physics" for the rigid-body world and the kicks, and "Render"
for the two frame loops.

Each sphere of the "Ray Tracing in One Weekend" arrangement becomes a dynamic
rigid body dropped from a random height onto the (static, physical) ground
sphere. Per frame the loop interleaves the three domains:

  * physics  — Box3D advances the world by the frame delta;
  * transfer — the freshly read body positions overwrite the @Scene@ SSBO and
    a rebuilt BVH overwrites the node buffer (both host-mapped);
  * compute  — the reflected path-tracing kernel renders the frame in tiles
    (@--tile@; one bounded submission each, out of the GPU watchdog's reach
    at any resolution or sample count), which is read back and written as a
    numbered PNG.

The camera slowly orbits the scene, and an optional periodic explosion keeps
the pile lively. The sky is a simple analytic day/night model driven by a
time-of-day clock (@--time-of-day@, @--day-length@): the sun sweeps over the
scene, sets, and hands the stage to emissive "glowy" spheres whose glow the
shader ramps in as the sky darkens — the sky is the only other light source,
so the ramp is what keeps the night from going black. Stitch the frames with
e.g.

> ffmpeg -framerate 30 -i frames/frame-%05d.png -c:v libx264 -pix_fmt yuv420p out.mp4

or pass @--out-ffmpeg out.mp4@ to have the example run that recipe itself
after rendering.

With @--window@ the same simulation runs live in an SDL2 window instead: the
mouse orbits the camera, left click tosses the balls, right click sets off an
explosion. Physics then steps by wall-clock delta, the host-written buffers
are doubled to match the two frames in flight, and the "Present" pass draws
the tracer's texel buffer straight onto the swapchain. Everything stays on
the graphics queue with plain barriers — see the @resize@ example for the
async-compute handover version of that dance.
-}
module Main
  ( main
  )
where

import Control.Exception (handle)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Foldable (for_)
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), withHeadlessVk)
import Options.Applicative
import qualified SDL
import Say (sayErrString)
import System.FilePath ((</>))
import System.Process (callProcess)
import UnliftIO.Exception (displayException)
import qualified Vulkan.Core10 as Vk
import Vulkan.Exception (VulkanException (..))
import qualified Vulkan.Extensions.VK_KHR_surface as KHR
import Vulkan.Utils.Init.SDL2.Window (createWindow, sdl2Adapter, withSDL)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.Swapchain (SwapchainConfig (..), defaultSwapchainConfig)
import qualified VulkanMemoryAllocator as VMA
import WindowedBoot (WindowedConfig (..), withWindowedVk)

import qualified Pathtracer
import qualified Present
import Render (Options (..))
import qualified Render

main :: IO ()
main = do
  opts <- execParser optionsInfo
  if opts.window
    then windowedMain opts
    else headlessMain opts

headlessMain :: Options -> IO ()
headlessMain opts = do
  runResourceT $ do
    HeadlessVk{..} <-
      withHeadlessVk
        HeadlessConfig
          { appName = "Haskell Vulkan pathtrace-box3d example"
          , instanceReqs = []
          , deviceReqs = Pathtracer.deviceRequirements
          , vmaFlags = VMA.ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT
          }
    let QueueFamilyIndex computeQueueFamilyIndex = fst (qCompute queues)
    Render.animate allocator device computeQueueFamilyIndex opts
    Vk.deviceWaitIdle device
  for_ opts.outFfmpeg (stitchFrames opts)

windowedMain :: Options -> IO ()
windowedMain opts = prettyError . runResourceT $ do
  withSDL
  sdlWindow <- createWindow "pathtrace-box3d" opts.width opts.height
  (vc, vma, initialSC) <- withWindowedVk windowConfig (sdl2Adapter sdlWindow)
  SDL.showWindow sdlWindow
  Render.windowed vc vma initialSC sdlWindow opts

windowConfig :: WindowedConfig
windowConfig =
  WindowedConfig
    { appName = "pathtrace-box3d"
    , instanceReqs = []
    , deviceReqs = Pathtracer.deviceRequirements <> Present.deviceRequirements
    , vmaFlags = VMA.ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT
    , swapchainConfig =
        defaultSwapchainConfig
          { -- A UNORM format presents the tracer's gamma-2.0 texels untouched
            -- (exactly the headless PNGs); on an sRGB-only surface the present
            -- shader decodes instead (see 'Present.srgbFormat').
            scSurfaceFormatPreferences = [not . Present.srgbFormat . KHR.format]
          }
    }

-- | Run the module-doc ffmpeg recipe over the rendered frames.
stitchFrames :: Options -> FilePath -> IO ()
stitchFrames opts out = do
  sayErrString ("Stitching " <> out)
  callProcess
    "ffmpeg"
    [ "-y"
    , "-loglevel"
    , "error"
    , "-framerate"
    , show opts.fps
    , "-i"
    , opts.outDir </> "frame-%05d.png"
    , "-c:v"
    , "libx264"
    , "-pix_fmt"
    , "yuv420p"
    , out
    ]

prettyError :: IO () -> IO ()
prettyError =
  handle (\e@(VulkanException _) -> sayErrString (displayException e))

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> header "pathtrace-box3d - Box3D-animated compute path tracer"
        <> progDesc
          "Drop the pathtrace-reflect sphere scene as Box3D rigid bodies and \
          \render the simulation frame by frame into numbered PNGs."
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option auto (long "width" <> metavar "N" <> value 640 <> showDefault <> help "Image width")
    <*> option auto (long "height" <> metavar "N" <> value 360 <> showDefault <> help "Image height")
    <*> optional (option auto (long "samples" <> metavar "N" <> help "Samples per pixel (spec constant; default: 512 headless, 16 windowed)"))
    <*> option auto (long "bounces" <> metavar "N" <> value 8 <> showDefault <> help "Max ray bounces (spec constant)")
    <*> option auto (long "spheres" <> metavar "N" <> value 64 <> showDefault <> help "Random spheres in the scene")
    <*> option auto (long "seed" <> metavar "N" <> value 1 <> showDefault <> help "Scene and sampling seed")
    <*> option auto (long "fov" <> metavar "DEG" <> value 20 <> showDefault <> help "Vertical field of view")
    <*> option auto (long "fps" <> metavar "N" <> value 30 <> showDefault <> help "Frames per second (also the physics rate)")
    <*> option auto (long "duration" <> metavar "SEC" <> value 5 <> showDefault <> help "Animation length in seconds")
    <*> option auto (long "orbit" <> metavar "DEG/S" <> value 6 <> showDefault <> help "Camera orbit speed")
    <*> option auto (long "explode-every" <> metavar "SEC" <> value 0 <> showDefault <> help "Explosion period (0 = off)")
    <*> option auto (long "popcorn-every" <> metavar "SEC" <> value 0 <> showDefault <> help "Toss random small balls this often (0 = off)")
    <*> option auto (long "pop-impulse" <> metavar "NS/M2" <> value 1200 <> showDefault <> help "Popcorn impulse per projected area (a 0.2 m plastic ball gets ~5 m/s)")
    <*> option auto (long "boom-impulse" <> metavar "NS/M2" <> value 2000 <> showDefault <> help "Explosion impulse per projected area")
    <*> option auto (long "boom-radius" <> metavar "M" <> value 6 <> showDefault <> help "Explosion full-strength radius")
    <*> option auto (long "time-of-day" <> metavar "HOUR" <> value 9 <> showDefault <> help "Starting hour of the 24-hour day")
    <*> option auto (long "day-length" <> metavar "SEC" <> value 60 <> showDefault <> help "Seconds of animation per 24-hour day")
    <*> option auto (long "settle" <> metavar "SEC" <> value 60 <> showDefault <> help "Let the physics settle for this many virtual seconds before the first frame")
    <*> option auto (long "timeout" <> metavar "SEC" <> value 60 <> showDefault <> help "GPU wait budget in seconds")
    <*> option auto (long "tile" <> metavar "PX" <> value 0 <> help "Tile edge for headless rendering; one tile is one GPU submission (default: auto — shrinks as --samples grows)")
    <*> strOption (long "out-dir" <> metavar "DIR" <> value "box3d-frames" <> showDefault <> help "Frame output directory")
    <*> optional (strOption (long "out-ffmpeg" <> metavar "FILE" <> help "Stitch the frames into this video with ffmpeg after rendering"))
    <*> switch (long "window" <> help "Run interactively in an SDL2 window (mouse orbits, left click tosses, right click explodes)")
