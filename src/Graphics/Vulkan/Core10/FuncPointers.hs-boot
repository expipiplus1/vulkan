{-# language CPP #-}
module Graphics.Vulkan.Core10.FuncPointers  ( PFN_vkVoidFunction
                                            , FN_vkVoidFunction
                                            ) where

import Foreign.Ptr (FunPtr)

type FN_vkVoidFunction = () -> IO ()
-- | PFN_vkVoidFunction - Dummy function pointer type returned by queries
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getDeviceProcAddr',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getInstanceProcAddr'
type PFN_vkVoidFunction = FunPtr FN_vkVoidFunction

