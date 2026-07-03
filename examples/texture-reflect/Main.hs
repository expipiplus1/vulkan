{-# LANGUAGE BangPatterns #-}

{-| Headless demonstration of __using a colour attachment as a texture__
(render-to-texture), driven by reflected SPIR-V. Two pipelines run back to
back in one command buffer — see "Tri" (the offscreen RGB triangle), "Cube"
(the spinning cube sampling it) and "Render" (the frame and the shared-set-0
wiring).

Both pipelines share a @Globals@ UBO at __set 0, binding 0__ (a @time@). The
set 0 descriptor-set layout is a single object reused by both pipeline
layouts, so the layouts are /compatible for set 0/: the UBO is bound __once__
before the offscreen pass and is never rebound — the cube pass only binds its
sampler at set 1. (The cross-pipeline layout match is not type-enforced here;
each pipeline's own vertex↔fragment composition still is, via
'Vulkan.Utils.SpirV.Stage.MatchInterface' \/
'Vulkan.Utils.SpirV.Stage.CompatibleResources'.)
-}
module Main where

import qualified Codec.Picture as JP
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.List (foldl')
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), withHeadlessVk)
import ImageReadback (savePng)
import System.Exit (exitFailure)
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Zero (zero)

import qualified Cube
import Render (height, width)
import qualified Render
import qualified Tri

main :: IO ()
main = runResourceT $ do
  HeadlessVk{..} <-
    withHeadlessVk
      HeadlessConfig
        { appName = "Haskell Vulkan colour-attachment-as-texture (headless)"
        , instanceReqs = []
        , deviceReqs = Dynamic.dynamicRenderingRequirements
        , vmaFlags = zero
        }
  liftIO $ do
    putStrLn $ "offscreen pipeline composes (compile-time): " <> show Tri.composes
    putStrLn $ "cube pipeline composes (compile-time):      " <> show Cube.composes
  let QueueFamilyIndex graphicsQueueFamilyIndex = fst (qGraphics queues)

  colorImage <- Render.render allocator device graphicsQueueFamilyIndex
  Vk.deviceWaitIdle device

  savePng "texture-reflect.png" colorImage
  let
    pixel x y = JP.pixelAt colorImage x y
    allPixels = [pixel x y | y <- [0 .. fromIntegral height - 1], x <- [0 .. fromIntegral width - 1]]
    -- Brightest sampled R/G/B across the image, in a single pass.
    (maxR, maxG, maxB) =
      foldl'
        (\(!r, !g, !b) (JP.PixelRGBA8 pr pg pb _) -> (max r (fromIntegral pr), max g (fromIntegral pg), max b (fromIntegral pb)))
        ((0, 0, 0) :: (Int, Int, Int))
        allPixels
    JP.PixelRGBA8 cr cg cb _ = pixel 2 2
    cornerChannels = [fromIntegral cr, fromIntegral cg, fromIntegral cb] :: [Int]
    cornerLum = sum cornerChannels
    cornerSaturation = maximum cornerChannels - minimum cornerChannels
  liftIO $
    putStrLn $
      "sampled max channels: R="
        <> show maxR
        <> " G="
        <> show maxG
        <> " B="
        <> show maxB
        <> "; background corner luminance="
        <> show cornerLum
        <> " saturation="
        <> show cornerSaturation

  let
    checks :: [(String, Bool)]
    checks =
      [ ("offscreen red was sampled onto the cube", maxR > 150)
      , ("offscreen green was sampled onto the cube", maxG > 150)
      , ("offscreen blue was sampled onto the cube", maxB > 150)
      , ("background is the neutral clear colour, not the texture", cornerLum > 80 && cornerSaturation < 40)
      ]
  liftIO $ do
    mapM_ (\(label, ok) -> putStrLn $ "[" <> (if ok then "PASS" else "FAIL") <> "] " <> label) checks
    unless (all snd checks) exitFailure
    putStrLn "All render-to-texture checks passed."
