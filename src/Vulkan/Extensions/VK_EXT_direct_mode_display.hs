{-# language CPP #-}
-- | = Name
--
-- VK_EXT_direct_mode_display - instance extension
--
-- == VK_EXT_direct_mode_display
--
-- [__Name String__]
--     @VK_EXT_direct_mode_display@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     89
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_display@
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_direct_mode_display] @cubanismo%0A<<Here describe the issue or question you have about the VK_EXT_direct_mode_display extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-12-13
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Pierre Boudier, NVIDIA
--
--     -   James Jones, NVIDIA
--
--     -   Damien Leone, NVIDIA
--
--     -   Pierre-Loup Griffais, Valve
--
--     -   Liam Middlebrook, NVIDIA
--
-- == Description
--
-- This is extension, along with related platform extensions, allows
-- applications to take exclusive control of displays associated with a
-- native windowing system. This is especially useful for virtual reality
-- applications that wish to hide HMDs (head mounted displays) from the
-- native platform’s display management system, desktop, and\/or other
-- applications.
--
-- == New Commands
--
-- -   'releaseDisplayEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME'
--
-- -   'EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION'
--
-- == Issues
--
-- 1) Should this extension and its related platform-specific extensions
-- leverage @VK_KHR_display@, or provide separate equivalent interfaces.
--
-- __RESOLVED__: Use @VK_KHR_display@ concepts and objects.
-- @VK_KHR_display@ can be used to enumerate all displays on the system,
-- including those attached to\/in use by a window system or native
-- platform, but @VK_KHR_display_swapchain@ will fail to create a swapchain
-- on in-use displays. This extension and its platform-specific children
-- will allow applications to grab in-use displays away from window systems
-- and\/or native platforms, allowing them to be used with
-- @VK_KHR_display_swapchain@.
--
-- 2) Are separate calls needed to acquire displays and enable direct mode?
--
-- __RESOLVED__: No, these operations happen in one combined command.
-- Acquiring a display puts it into direct mode.
--
-- == Version History
--
-- -   Revision 1, 2016-12-13 (James Jones)
--
--     -   Initial draft
--
-- == See Also
--
-- 'releaseDisplayEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_direct_mode_display Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_direct_mode_display  ( releaseDisplayEXT
                                                     , EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
                                                     , pattern EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
                                                     , EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
                                                     , pattern EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
                                                     , DisplayKHR(..)
                                                     ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Vulkan.Extensions.Handles (DisplayKHR)
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Dynamic (InstanceCmds(pVkReleaseDisplayEXT))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.Handles (DisplayKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseDisplayEXT
  :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> DisplayKHR -> IO Result

-- | vkReleaseDisplayEXT - Release access to an acquired VkDisplayKHR
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_direct_mode_display VK_EXT_direct_mode_display>,
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
releaseDisplayEXT :: forall io
                   . (MonadIO io)
                  => -- | @physicalDevice@ The physical device the display is on.
                     --
                     -- #VUID-vkReleaseDisplayEXT-physicalDevice-parameter# @physicalDevice@
                     -- /must/ be a valid 'Vulkan.Core10.Handles.PhysicalDevice' handle
                     PhysicalDevice
                  -> -- | @display@ The display to release control of.
                     --
                     -- #VUID-vkReleaseDisplayEXT-display-parameter# @display@ /must/ be a valid
                     -- 'Vulkan.Extensions.Handles.DisplayKHR' handle
                     --
                     -- #VUID-vkReleaseDisplayEXT-display-parent# @display@ /must/ have been
                     -- created, allocated, or retrieved from @physicalDevice@
                     DisplayKHR
                  -> io ()
releaseDisplayEXT physicalDevice display = liftIO $ do
  let vkReleaseDisplayEXTPtr = pVkReleaseDisplayEXT (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  unless (vkReleaseDisplayEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkReleaseDisplayEXT is null" Nothing Nothing
  let vkReleaseDisplayEXT' = mkVkReleaseDisplayEXT vkReleaseDisplayEXTPtr
  _ <- traceAroundEvent "vkReleaseDisplayEXT" (vkReleaseDisplayEXT' (physicalDeviceHandle (physicalDevice)) (display))
  pure $ ()


type EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION"
pattern EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1


type EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME = "VK_EXT_direct_mode_display"

-- No documentation found for TopLevel "VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME"
pattern EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME = "VK_EXT_direct_mode_display"

