module Main where

import Control.Exception
import Data.Foldable
import Text.Pretty.Simple
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)

main :: IO ()
main = Vk.withInstance zero Nothing bracket $ \i -> do
  myPrint i
  (_, layers) <- Vk.enumerateInstanceLayerProperties
  (_, extensions) <- Vk.enumerateInstanceExtensionProperties Nothing
  myPrint layers
  myPrint extensions
  (_, devices) <- Vk.enumeratePhysicalDevices i
  traverse_ deviceInfo devices

deviceInfo :: Vk.PhysicalDevice -> IO ()
deviceInfo p = do
  (_, extensions) <- Vk.enumerateDeviceExtensionProperties p Nothing
  (_, layers) <- Vk.enumerateDeviceLayerProperties p
  traverse_ myPrint extensions
  traverse_ myPrint layers
  myPrint =<< Vk.getPhysicalDeviceFeatures p
  myPrint =<< Vk.getPhysicalDeviceProperties p
  myPrint =<< Vk.getPhysicalDeviceMemoryProperties p

myPrint :: (Show a) => a -> IO ()
myPrint =
  pPrintOpt
    CheckColorTty
    defaultOutputOptionsDarkBg{outputOptionsStringStyle = Literal}
