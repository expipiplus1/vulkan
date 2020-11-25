{-# language CPP #-}
-- No documentation found for Chapter "DeviceInitialization"
module Vulkan.Core11.DeviceInitialization  (enumerateInstanceVersion) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (castFunPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Foreign.Storable (Storable(peek))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Ptr (Ptr(Ptr))
import Data.Word (Word32)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Dynamic (getInstanceProcAddr')
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateInstanceVersion
  :: FunPtr (Ptr Word32 -> IO Result) -> Ptr Word32 -> IO Result

-- No documentation found for TopLevel "vkEnumerateInstanceVersion"
enumerateInstanceVersion :: forall io
                          . (MonadIO io)
                         => io (("apiVersion" ::: Word32))
enumerateInstanceVersion  = liftIO . evalContT $ do
  vkEnumerateInstanceVersionPtr <- lift $ castFunPtr @_ @(("pApiVersion" ::: Ptr Word32) -> IO Result) <$> getInstanceProcAddr' nullPtr (Ptr "vkEnumerateInstanceVersion"#)
  lift $ unless (vkEnumerateInstanceVersionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEnumerateInstanceVersion is null" Nothing Nothing
  let vkEnumerateInstanceVersion' = mkVkEnumerateInstanceVersion vkEnumerateInstanceVersionPtr
  pPApiVersion <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkEnumerateInstanceVersion' (pPApiVersion)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pApiVersion <- lift $ peek @Word32 pPApiVersion
  pure $ (pApiVersion)

