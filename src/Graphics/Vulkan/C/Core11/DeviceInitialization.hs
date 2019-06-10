{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}

module Graphics.Vulkan.C.Core11.DeviceInitialization
  ( FN_vkEnumerateInstanceVersion
  , PFN_vkEnumerateInstanceVersion
  , vkEnumerateInstanceVersion
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , castPtrToFunPtr
  , nullPtr
  )
import qualified GHC.Ptr
  ( Ptr(Ptr)
  )
import System.IO.Unsafe
  ( unsafeDupablePerformIO
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( vkGetInstanceProcAddr'
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "vkEnumerateInstanceVersion"
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateInstanceVersion" vkEnumerateInstanceVersion :: ("pApiVersion" ::: Ptr Word32) -> IO VkResult
#else
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateInstanceVersion
  :: FunPtr (("pApiVersion" ::: Ptr Word32) -> IO VkResult) -> (("pApiVersion" ::: Ptr Word32) -> IO VkResult)

vkEnumerateInstanceVersion :: ("pApiVersion" ::: Ptr Word32) -> IO VkResult
vkEnumerateInstanceVersion = mkVkEnumerateInstanceVersion procAddr
  where
    procAddr = castPtrToFunPtr @_ @FN_vkEnumerateInstanceVersion $
      unsafeDupablePerformIO
        $ vkGetInstanceProcAddr' nullPtr (GHC.Ptr.Ptr "vkEnumerateInstanceVersion\NUL"#)
#endif

type FN_vkEnumerateInstanceVersion = ("pApiVersion" ::: Ptr Word32) -> IO VkResult
type PFN_vkEnumerateInstanceVersion = FunPtr FN_vkEnumerateInstanceVersion
