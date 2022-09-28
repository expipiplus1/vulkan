{-# language CPP #-}
-- No documentation found for Chapter "DeviceInitialization"
module Vulkan.Core11.DeviceInitialization  (enumerateInstanceVersion) where

import Vulkan.Internal.Utils (traceAroundEvent)
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

-- | vkEnumerateInstanceVersion - Query instance-level version before
-- instance creation
--
-- = Description
--
-- Note
--
-- The intended behaviour of 'enumerateInstanceVersion' is that an
-- implementation /should/ not need to perform memory allocations and
-- /should/ unconditionally return 'Vulkan.Core10.Enums.Result.SUCCESS'.
-- The loader, and any enabled layers, /may/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY' in the case of a
-- failed memory allocation.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>
enumerateInstanceVersion :: forall io
                          . (MonadIO io)
                         => io (("apiVersion" ::: Word32))
enumerateInstanceVersion  = liftIO . evalContT $ do
  vkEnumerateInstanceVersionPtr <- lift $ castFunPtr @_ @(("pApiVersion" ::: Ptr Word32) -> IO Result) <$> getInstanceProcAddr' nullPtr (Ptr "vkEnumerateInstanceVersion"#)
  lift $ unless (vkEnumerateInstanceVersionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEnumerateInstanceVersion is null" Nothing Nothing
  let vkEnumerateInstanceVersion' = mkVkEnumerateInstanceVersion vkEnumerateInstanceVersionPtr
  pPApiVersion <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkEnumerateInstanceVersion" (vkEnumerateInstanceVersion'
                                                               (pPApiVersion))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pApiVersion <- lift $ peek @Word32 pPApiVersion
  pure $ (pApiVersion)

