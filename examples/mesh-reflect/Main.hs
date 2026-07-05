{-| Headless demonstration of type-verified pipeline assembly from reflected
SPIR-V — see "Mesh" for the pipeline family (and the compile-time composition
check), "Mesh.Shader" for the quasiquoted shaders, and "Render" for the frame:

  * a __depth-only__ pipeline (vertex stage alone, no colour attachment) — a
    z-prepass whose depth buffer is read back and checked (near depth written at
    the centre, the cleared far value at the corner).

  * a __depth+colour__ pipeline (vertex + fragment) — the fragment stage shades
    the surface with a Lambert (N·L) light from the shared @Scene@ UBO, producing
    a top-bright/bottom-dark gradient that is read back and checked.

Exits non-zero on mismatch.
-}
module Main where

import qualified Codec.Picture as JP
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), withHeadlessVk)
import ImageReadback (savePng)
import System.Exit (exitFailure)
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Zero (zero)

import qualified Mesh
import Render (height, width)
import qualified Render

main :: IO ()
main = runResourceT $ do
  HeadlessVk{..} <-
    withHeadlessVk
      HeadlessConfig
        { appName = "Haskell Vulkan type-verified pipeline assembly (headless)"
        , instanceReqs = []
        , deviceReqs = Dynamic.dynamicRenderingRequirements
        , vmaFlags = zero
        }
  liftIO $ putStrLn $ "vertex+fragment pipeline composes (compile-time): " <> show Mesh.pipelineComposes
  let QueueFamilyIndex graphicsQueueFamilyIndex = fst (qGraphics queues)

  (depthCentre, depthCorner, colorImage) <-
    Render.render allocator device graphicsQueueFamilyIndex
  Vk.deviceWaitIdle device

  savePng "mesh-reflect-color.png" colorImage
  let
    pixel x y = JP.pixelAt colorImage x y
    lum (JP.PixelRGBA8 r g b _) = fromIntegral r + fromIntegral g + fromIntegral b :: Int
    cx = fromIntegral width `div` 2
    upper = lum (pixel cx (fromIntegral height `div` 4)) -- near the lit apex
    lower = lum (pixel cx (3 * fromIntegral height `div` 4)) -- near the dark base
    centre = lum (pixel cx (fromIntegral height `div` 2))
    corner = lum (pixel 4 4) -- background
  liftIO $ do
    putStrLn $ "depth-only:  centre=" <> show depthCentre <> " corner=" <> show depthCorner
    putStrLn $
      "depth+color: luminance upper="
        <> show upper
        <> " lower="
        <> show lower
        <> " centre="
        <> show centre
        <> " corner="
        <> show corner

  let
    checks :: [(String, Bool)]
    checks =
      [ ("depth-only wrote near depth at the centre", depthCentre < 0.9)
      , ("depth-only left the corner at the far plane", depthCorner > 0.99)
      , ("depth+color lit the geometry above the background", centre > corner + 60)
      , ("Lambert shading is brighter near the light (top) than away (bottom)", upper > lower + 60)
      ]
  liftIO $ do
    mapM_ (\(label, ok) -> putStrLn $ "[" <> (if ok then "PASS" else "FAIL") <> "] " <> label) checks
    unless (all snd checks) exitFailure
    putStrLn "All type-verified pipeline-assembly checks passed."
