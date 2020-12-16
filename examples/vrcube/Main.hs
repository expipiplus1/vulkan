{-# language OverloadedLists #-}
module Main
  ( main
  ) where

import           Control.Exception
import           Data.Foldable
import           OpenXR.Core10
import           OpenXR.Extensions.XR_KHR_vulkan_enable
import           OpenXR.Version
import           OpenXR.Zero
import           Say

main :: IO ()
main = do
  sayErr "Available Extensions:"
  traverse_ sayErrShow =<< enumerateInstanceExtensionProperties Nothing
  let ici = zero
        { applicationInfo       = zero { applicationName = "Haskell VR Example"
                                       , apiVersion      = CURRENT_API_VERSION
                                       }
        , enabledExtensionNames = [KHR_VULKAN_ENABLE_EXTENSION_NAME]
        }
  withInstance ici bracket print
