module Main
  where

import           Control.Exception
import           Data.Foldable
import           Text.Pretty.Simple
import           Vulkan.Core10
import           Vulkan.Zero

main :: IO ()
main = withInstance zero Nothing bracket $ \i -> do
  myPrint i
  (_, layers    ) <- enumerateInstanceLayerProperties
  (_, extensions) <- enumerateInstanceExtensionProperties Nothing
  myPrint layers
  myPrint extensions
  (_, devices) <- enumeratePhysicalDevices i
  traverse_ deviceInfo devices

deviceInfo :: PhysicalDevice -> IO ()
deviceInfo p = do
  (_, extensions) <- enumerateDeviceExtensionProperties p Nothing
  (_, layers    ) <- enumerateDeviceLayerProperties p
  traverse_ myPrint extensions
  traverse_ myPrint layers
  myPrint =<< getPhysicalDeviceFeatures p
  myPrint =<< getPhysicalDeviceProperties p

myPrint :: Show a => a -> IO ()
myPrint =
  pPrintOpt
    CheckColorTty
    defaultOutputOptionsDarkBg
      { outputOptionsStringStyle = Literal
      }
