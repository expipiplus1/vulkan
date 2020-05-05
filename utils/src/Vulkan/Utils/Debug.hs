module Vulkan.Utils.Debug
  ( debugCallbackPtr
  , nameObject
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString

import           Vulkan.Core10
import           Vulkan.Extensions.VK_EXT_debug_utils

-- | A debug callback which prints the message prefixed with "Validation: " to
-- stderr.
foreign import ccall unsafe "DebugCallback.c &debugCallback"
  debugCallbackPtr :: PFN_vkDebugUtilsMessengerCallbackEXT

-- | Assign a name to a handle using 'setDebugUtilsObjectNameEXT', note that
-- the @VK_EXT_debug_utils@ extension must be enabled.
nameObject :: (HasObjectType a, MonadIO m) => Device -> a -> ByteString -> m ()
nameObject device object name = setDebugUtilsObjectNameEXT
  device
  (uncurry DebugUtilsObjectNameInfoEXT (objectTypeAndHandle object) (Just name))
