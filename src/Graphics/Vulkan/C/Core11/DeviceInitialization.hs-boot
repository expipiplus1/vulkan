{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.DeviceInitialization
  ( FN_vkEnumerateInstanceVersion
  , PFN_vkEnumerateInstanceVersion
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )


type FN_vkEnumerateInstanceVersion = ("pApiVersion" ::: Ptr Word32) -> IO VkResult
type PFN_vkEnumerateInstanceVersion = FunPtr FN_vkEnumerateInstanceVersion
