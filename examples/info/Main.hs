module Main
  where

import           Control.Exception
import           Data.Foldable
import           Graphics.Vulkan.Core10
import           Graphics.Vulkan.Zero
import           Text.Pretty.Simple

main :: IO ()
main = withInstance bracket zero Nothing $ \i -> do
  pPrint i
  (_, layers    ) <- enumerateInstanceLayerProperties
  (_, extensions) <- enumerateInstanceExtensionProperties Nothing
  pPrint layers
  pPrint extensions
  (_, devices) <- enumeratePhysicalDevices i
  traverse_ deviceInfo devices

deviceInfo :: PhysicalDevice -> IO ()
deviceInfo p = do
  (_, extensions) <- enumerateDeviceExtensionProperties p Nothing
  (_, layers    ) <- enumerateDeviceLayerProperties p
  traverse_ pPrint extensions
  traverse_ pPrint layers
  pPrint =<< getPhysicalDeviceFeatures p
  pPrint =<< getPhysicalDeviceProperties p
