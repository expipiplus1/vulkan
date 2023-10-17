{-# language CPP #-}
-- | = Name
--
-- VK_EXT_acquire_drm_display - instance extension
--
-- == VK_EXT_acquire_drm_display
--
-- [__Name String__]
--     @VK_EXT_acquire_drm_display@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     286
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_direct_mode_display VK_EXT_direct_mode_display>
--
-- [__Contact__]
--
--     -   Drew DeVault <mailto:sir@cmpwn.com sir\@cmpwn.com>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-06-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Simon Zeni, Status Holdings, Ltd.
--
-- == Description
--
-- This extension allows an application to take exclusive control of a
-- display using the Direct Rendering Manager (DRM) interface. When
-- acquired, the display will be under full control of the application
-- until the display is either released or the connector is unplugged.
--
-- == New Commands
--
-- -   'acquireDrmDisplayEXT'
--
-- -   'getDrmDisplayEXT'
--
-- == New Enum Constants
--
-- -   'EXT_ACQUIRE_DRM_DISPLAY_EXTENSION_NAME'
--
-- -   'EXT_ACQUIRE_DRM_DISPLAY_SPEC_VERSION'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2021-05-11 (Simon Zeni)
--
--     -   Initial draft
--
-- == See Also
--
-- 'acquireDrmDisplayEXT', 'getDrmDisplayEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_acquire_drm_display Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_acquire_drm_display  ( acquireDrmDisplayEXT
                                                     , getDrmDisplayEXT
                                                     , EXT_ACQUIRE_DRM_DISPLAY_SPEC_VERSION
                                                     , pattern EXT_ACQUIRE_DRM_DISPLAY_SPEC_VERSION
                                                     , EXT_ACQUIRE_DRM_DISPLAY_EXTENSION_NAME
                                                     , pattern EXT_ACQUIRE_DRM_DISPLAY_EXTENSION_NAME
                                                     , DisplayKHR(..)
                                                     ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Foreign.Storable (Storable(peek))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.Handles (DisplayKHR)
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Dynamic (InstanceCmds(pVkAcquireDrmDisplayEXT))
import Vulkan.Dynamic (InstanceCmds(pVkGetDrmDisplayEXT))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DisplayKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireDrmDisplayEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Int32 -> DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> Int32 -> DisplayKHR -> IO Result

-- | vkAcquireDrmDisplayEXT - Acquire access to a VkDisplayKHR using DRM
--
-- = Description
--
-- All permissions necessary to control the display are granted to the
-- Vulkan instance associated with the provided @physicalDevice@ until the
-- display is either released or the connector is unplugged. The provided
-- @drmFd@ must correspond to the one owned by the @physicalDevice@. If
-- not, the error code 'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN' must be
-- returned. The DRM FD must have DRM master permissions. If any error is
-- encountered during the acquisition of the display, the call must return
-- the error code 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'.
--
-- The provided DRM fd should not be closed before the display is released,
-- attempting to do it may result in undefined behaviour.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_acquire_drm_display VK_EXT_acquire_drm_display>,
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
acquireDrmDisplayEXT :: forall io
                      . (MonadIO io)
                     => -- | @physicalDevice@ The physical device the display is on.
                        --
                        -- #VUID-vkAcquireDrmDisplayEXT-physicalDevice-parameter# @physicalDevice@
                        -- /must/ be a valid 'Vulkan.Core10.Handles.PhysicalDevice' handle
                        PhysicalDevice
                     -> -- | @drmFd@ DRM primary file descriptor.
                        ("drmFd" ::: Int32)
                     -> -- | @display@ The display the caller wishes Vulkan to control.
                        --
                        -- #VUID-vkAcquireDrmDisplayEXT-display-parameter# @display@ /must/ be a
                        -- valid 'Vulkan.Extensions.Handles.DisplayKHR' handle
                        --
                        -- #VUID-vkAcquireDrmDisplayEXT-display-parent# @display@ /must/ have been
                        -- created, allocated, or retrieved from @physicalDevice@
                        DisplayKHR
                     -> io ()
acquireDrmDisplayEXT physicalDevice drmFd display = liftIO $ do
  let vkAcquireDrmDisplayEXTPtr = pVkAcquireDrmDisplayEXT (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  unless (vkAcquireDrmDisplayEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireDrmDisplayEXT is null" Nothing Nothing
  let vkAcquireDrmDisplayEXT' = mkVkAcquireDrmDisplayEXT vkAcquireDrmDisplayEXTPtr
  r <- traceAroundEvent "vkAcquireDrmDisplayEXT" (vkAcquireDrmDisplayEXT'
                                                    (physicalDeviceHandle (physicalDevice))
                                                    (drmFd)
                                                    (display))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDrmDisplayEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Int32 -> Word32 -> Ptr DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> Int32 -> Word32 -> Ptr DisplayKHR -> IO Result

-- | vkGetDrmDisplayEXT - Query the VkDisplayKHR corresponding to a DRM
-- connector ID
--
-- = Description
--
-- If there is no 'Vulkan.Extensions.Handles.DisplayKHR' corresponding to
-- the @connectorId@ on the @physicalDevice@, the returning @display@ must
-- be set to 'Vulkan.Core10.APIConstants.NULL_HANDLE'. The provided @drmFd@
-- must correspond to the one owned by the @physicalDevice@. If not, the
-- error code 'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN' must be returned.
-- Master permissions are not required, because the file descriptor is just
-- used for information gathering purposes. The given @connectorId@ must be
-- a resource owned by the provided @drmFd@. If not, the error code
-- 'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN' must be returned. If any
-- error is encountered during the identification of the display, the call
-- must return the error code
-- 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_acquire_drm_display VK_EXT_acquire_drm_display>,
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getDrmDisplayEXT :: forall io
                  . (MonadIO io)
                 => -- | @physicalDevice@ The physical device to query the display from.
                    --
                    -- #VUID-vkGetDrmDisplayEXT-physicalDevice-parameter# @physicalDevice@
                    -- /must/ be a valid 'Vulkan.Core10.Handles.PhysicalDevice' handle
                    PhysicalDevice
                 -> -- | @drmFd@ DRM primary file descriptor.
                    ("drmFd" ::: Int32)
                 -> -- | @connectorId@ Identifier of the specified DRM connector.
                    ("connectorId" ::: Word32)
                 -> io (DisplayKHR)
getDrmDisplayEXT physicalDevice drmFd connectorId = liftIO . evalContT $ do
  let vkGetDrmDisplayEXTPtr = pVkGetDrmDisplayEXT (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetDrmDisplayEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDrmDisplayEXT is null" Nothing Nothing
  let vkGetDrmDisplayEXT' = mkVkGetDrmDisplayEXT vkGetDrmDisplayEXTPtr
  pDisplay <- ContT $ bracket (callocBytes @DisplayKHR 8) free
  r <- lift $ traceAroundEvent "vkGetDrmDisplayEXT" (vkGetDrmDisplayEXT'
                                                       (physicalDeviceHandle (physicalDevice))
                                                       (drmFd)
                                                       (connectorId)
                                                       (pDisplay))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  display <- lift $ peek @DisplayKHR pDisplay
  pure $ (display)


type EXT_ACQUIRE_DRM_DISPLAY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_ACQUIRE_DRM_DISPLAY_SPEC_VERSION"
pattern EXT_ACQUIRE_DRM_DISPLAY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_ACQUIRE_DRM_DISPLAY_SPEC_VERSION = 1


type EXT_ACQUIRE_DRM_DISPLAY_EXTENSION_NAME = "VK_EXT_acquire_drm_display"

-- No documentation found for TopLevel "VK_EXT_ACQUIRE_DRM_DISPLAY_EXTENSION_NAME"
pattern EXT_ACQUIRE_DRM_DISPLAY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_ACQUIRE_DRM_DISPLAY_EXTENSION_NAME = "VK_EXT_acquire_drm_display"

