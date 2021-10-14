{-# language CPP #-}
-- No documentation found for Chapter "FuncPointers"
module Vulkan.Core10.FuncPointers  ( PFN_vkVoidFunction
                                   , FN_vkVoidFunction
                                   ) where

import Foreign.Ptr (FunPtr)

type FN_vkVoidFunction = () -> IO ()
-- | PFN_vkVoidFunction - Placeholder function pointer type returned by
-- queries
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.DeviceInitialization.getDeviceProcAddr',
-- 'Vulkan.Core10.DeviceInitialization.getInstanceProcAddr'
type PFN_vkVoidFunction = FunPtr FN_vkVoidFunction

