{-# language CPP #-}
-- | = Name
--
-- VK_KHR_display - instance extension
--
-- == VK_KHR_display
--
-- [__Name String__]
--     @VK_KHR_display@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     3
--
-- [__Revision__]
--     23
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_display:%20&body=@cubanismo%20 >
--
--     -   Norbert Nopper
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_display:%20&body=@FslNopper%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-03-13
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
--     -   Norbert Nopper, Freescale
--
--     -   Jeff Vigil, Qualcomm
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- This extension provides the API to enumerate displays and available
-- modes on a given device.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.DisplayKHR'
--
-- -   'Vulkan.Extensions.Handles.DisplayModeKHR'
--
-- == New Commands
--
-- -   'createDisplayModeKHR'
--
-- -   'createDisplayPlaneSurfaceKHR'
--
-- -   'getDisplayModePropertiesKHR'
--
-- -   'getDisplayPlaneCapabilitiesKHR'
--
-- -   'getDisplayPlaneSupportedDisplaysKHR'
--
-- -   'getPhysicalDeviceDisplayPlanePropertiesKHR'
--
-- -   'getPhysicalDeviceDisplayPropertiesKHR'
--
-- == New Structures
--
-- -   'DisplayModeCreateInfoKHR'
--
-- -   'DisplayModeParametersKHR'
--
-- -   'DisplayModePropertiesKHR'
--
-- -   'DisplayPlaneCapabilitiesKHR'
--
-- -   'DisplayPlanePropertiesKHR'
--
-- -   'DisplayPropertiesKHR'
--
-- -   'DisplaySurfaceCreateInfoKHR'
--
-- == New Enums
--
-- -   'DisplayPlaneAlphaFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'DisplayModeCreateFlagsKHR'
--
-- -   'DisplayPlaneAlphaFlagsKHR'
--
-- -   'DisplaySurfaceCreateFlagsKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DISPLAY_EXTENSION_NAME'
--
-- -   'KHR_DISPLAY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_DISPLAY_KHR'
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_DISPLAY_MODE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Which properties of a mode should be fixed in the mode info vs.
-- settable in some other function when setting the mode? E.g., do we need
-- to double the size of the mode pool to include both stereo and
-- non-stereo modes? YUV and RGB scanout even if they both take RGB input
-- images? BGR vs. RGB input? etc.
--
-- __PROPOSED RESOLUTION__: Many modern displays support at most a handful
-- of resolutions and timings natively. Other “modes” are expected to be
-- supported using scaling hardware on the display engine or GPU. Other
-- properties, such as rotation and mirroring should not require
-- duplicating hardware modes just to express all combinations. Further,
-- these properties may be implemented on a per-display or per-overlay
-- granularity.
--
-- To avoid the exponential growth of modes as mutable properties are
-- added, as was the case with @EGLConfig@\/WGL pixel
-- formats\/@GLXFBConfig@, this specification should separate out hardware
-- properties and configurable state into separate objects. Modes and
-- overlay planes will express capabilities of the hardware, while a
-- separate structure will allow applications to configure scaling,
-- rotation, mirroring, color keys, LUT values, alpha masks, etc. for a
-- given swapchain independent of the mode in use. Constraints on these
-- settings will be established by properties of the immutable objects.
--
-- Note the resolution of this issue may affect issue 5 as well.
--
-- 2) What properties of a display itself are useful?
--
-- __PROPOSED RESOLUTION__: This issue is too broad. It was meant to prompt
-- general discussion, but resolving this issue amounts to completing this
-- specification. All interesting properties should be included. The issue
-- will remain as a placeholder since removing it would make it hard to
-- parse existing discussion notes that refer to issues by number.
--
-- 3) How are multiple overlay planes within a display or mode enumerated?
--
-- __PROPOSED RESOLUTION__: They are referred to by an index. Each display
-- will report the number of overlay planes it contains.
--
-- 4) Should swapchains be created relative to a mode or a display?
--
-- __PROPOSED RESOLUTION__: When using this extension, swapchains are
-- created relative to a mode and a plane. The mode implies the display
-- object the swapchain will present to. If the specified mode is not the
-- display’s current mode, the new mode will be applied when the first
-- image is presented to the swapchain, and the default operating system
-- mode, if any, will be restored when the swapchain is destroyed.
--
-- 5) Should users query generic ranges from displays and construct their
-- own modes explicitly using those constraints rather than querying a
-- fixed set of modes (Most monitors only have one real “mode” these days,
-- even though many support relatively arbitrary scaling, either on the
-- monitor side or in the GPU display engine, making “modes” something of a
-- relic\/compatibility construct).
--
-- __PROPOSED RESOLUTION__: Expose both. Display info structures will
-- expose a set of predefined modes, as well as any attributes necessary to
-- construct a customized mode.
--
-- 6) Is it fine if we return the display and display mode handles in the
-- structure used to query their properties?
--
-- __PROPOSED RESOLUTION__: Yes.
--
-- 7) Is there a possibility that not all displays of a device work with
-- all of the present queues of a device? If yes, how do we determine which
-- displays work with which present queues?
--
-- __PROPOSED RESOLUTION__: No known hardware has such limitations, but
-- determining such limitations is supported automatically using the
-- existing @VK_KHR_surface@ and @VK_KHR_swapchain@ query mechanisms.
--
-- 8) Should all presentation need to be done relative to an overlay plane,
-- or can a display mode + display be used alone to target an output?
--
-- __PROPOSED RESOLUTION__: Require specifying a plane explicitly.
--
-- 9) Should displays have an associated window system display, such as an
-- @HDC@ or 'Vulkan.Extensions.VK_KHR_xlib_surface.Display'*?
--
-- __PROPOSED RESOLUTION__: No. Displays are independent of any windowing
-- system in use on the system. Further, neither @HDC@ nor
-- 'Vulkan.Extensions.VK_KHR_xlib_surface.Display'* refer to a physical
-- display object.
--
-- 10) Are displays queried from a physical GPU or from a device instance?
--
-- __PROPOSED RESOLUTION__: Developers prefer to query modes directly from
-- the physical GPU so they can use display information as an input to
-- their device selection algorithms prior to device creation. This avoids
-- the need to create placeholder device instances to enumerate displays.
--
-- This preference must be weighed against the extra initialization that
-- must be done by driver vendors prior to device instance creation to
-- support this usage.
--
-- 11) Should displays and\/or modes be dispatchable objects? If functions
-- are to take displays, overlays, or modes as their first parameter, they
-- must be dispatchable objects as defined in Khronos bug 13529. If they
-- are not added to the list of dispatchable objects, functions operating
-- on them must take some higher-level object as their first parameter.
-- There is no performance case against making them dispatchable objects,
-- but they would be the first extension objects to be dispatchable.
--
-- __PROPOSED RESOLUTION__: Do not make displays or modes dispatchable.
-- They will dispatch based on their associated physical device.
--
-- 12) Should hardware cursor capabilities be exposed?
--
-- __PROPOSED RESOLUTION__: Defer. This could be a separate extension on
-- top of the base WSI specs.
--
-- if they are one physical display device to an end user, but may
-- internally be implemented as two side-by-side displays using the same
-- display engine (and sometimes cabling) resources as two physically
-- separate display devices.
--
-- __RESOLVED__: Tiled displays will appear as a single display object in
-- this API.
--
-- 14) Should the raw EDID data be included in the display information?
--
-- __RESOLVED__: No. A future extension could be added which reports the
-- EDID if necessary. This may be complicated by the outcome of issue 13.
--
-- 15) Should min and max scaling factor capabilities of overlays be
-- exposed?
--
-- __RESOLVED__: Yes. This is exposed indirectly by allowing applications
-- to query the min\/max position and extent of the source and destination
-- regions from which image contents are fetched by the display engine when
-- using a particular mode and overlay pair.
--
-- 16) Should devices be able to expose planes that can be moved between
-- displays? If so, how?
--
-- __RESOLVED__: Yes. Applications can determine which displays a given
-- plane supports using 'getDisplayPlaneSupportedDisplaysKHR'.
--
-- 17) Should there be a way to destroy display modes? If so, does it
-- support destroying “built in” modes?
--
-- __RESOLVED__: Not in this extension. A future extension could add this
-- functionality.
--
-- 18) What should the lifetime of display and built-in display mode
-- objects be?
--
-- __RESOLVED__: The lifetime of the instance. These objects cannot be
-- destroyed. A future extension may be added to expose a way to destroy
-- these objects and\/or support display hotplug.
--
-- 19) Should persistent mode for smart panels be enabled\/disabled at
-- swapchain creation time, or on a per-present basis.
--
-- __RESOLVED__: On a per-present basis.
--
-- == Examples
--
-- Note
--
-- The example code for the @VK_KHR_display@ and @VK_KHR_display_swapchain@
-- extensions was removed from the appendix after revision 1.0.43. The
-- display enumeration example code was ported to the cube demo that is
-- shipped with the official Khronos SDK, and is being kept up-to-date in
-- that location (see:
-- <https://github.com/KhronosGroup/Vulkan-Tools/blob/master/cube/cube.c>).
--
-- == Version History
--
-- -   Revision 1, 2015-02-24 (James Jones)
--
--     -   Initial draft
--
-- -   Revision 2, 2015-03-12 (Norbert Nopper)
--
--     -   Added overlay enumeration for a display.
--
-- -   Revision 3, 2015-03-17 (Norbert Nopper)
--
--     -   Fixed typos and namings as discussed in Bugzilla.
--
--     -   Reordered and grouped functions.
--
--     -   Added functions to query count of display, mode and overlay.
--
--     -   Added native display handle, which is maybe needed on some
--         platforms to create a native Window.
--
-- -   Revision 4, 2015-03-18 (Norbert Nopper)
--
--     -   Removed primary and virtualPostion members (see comment of James
--         Jones in Bugzilla).
--
--     -   Added native overlay handle to info structure.
--
--     -   Replaced , with ; in struct.
--
-- -   Revision 6, 2015-03-18 (Daniel Rakos)
--
--     -   Added WSI extension suffix to all items.
--
--     -   Made the whole API more \"Vulkanish\".
--
--     -   Replaced all functions with a single vkGetDisplayInfoKHR
--         function to better match the rest of the API.
--
--     -   Made the display, display mode, and overlay objects be first
--         class objects, not subclasses of VkBaseObject as they do not
--         support the common functions anyways.
--
--     -   Renamed *Info structures to *Properties.
--
--     -   Removed overlayIndex field from VkOverlayProperties as there is
--         an implicit index already as a result of moving to a
--         \"Vulkanish\" API.
--
--     -   Displays are not get through device, but through physical GPU to
--         match the rest of the Vulkan API. Also this is something ISVs
--         explicitly requested.
--
--     -   Added issue (6) and (7).
--
-- -   Revision 7, 2015-03-25 (James Jones)
--
--     -   Added an issues section
--
--     -   Added rotation and mirroring flags
--
-- -   Revision 8, 2015-03-25 (James Jones)
--
--     -   Combined the duplicate issues sections introduced in last
--         change.
--
--     -   Added proposed resolutions to several issues.
--
-- -   Revision 9, 2015-04-01 (Daniel Rakos)
--
--     -   Rebased extension against Vulkan 0.82.0
--
-- -   Revision 10, 2015-04-01 (James Jones)
--
--     -   Added issues (10) and (11).
--
--     -   Added more straw-man issue resolutions, and cleaned up the
--         proposed resolution for issue (4).
--
--     -   Updated the rotation and mirroring enums to have proper bitmask
--         semantics.
--
-- -   Revision 11, 2015-04-15 (James Jones)
--
--     -   Added proposed resolution for issues (1) and (2).
--
--     -   Added issues (12), (13), (14), and (15)
--
--     -   Removed pNativeHandle field from overlay structure.
--
--     -   Fixed small compilation errors in example code.
--
-- -   Revision 12, 2015-07-29 (James Jones)
--
--     -   Rewrote the guts of the extension against the latest WSI
--         swapchain specifications and the latest Vulkan API.
--
--     -   Address overlay planes by their index rather than an object
--         handle and refer to them as \"planes\" rather than \"overlays\"
--         to make it slightly clearer that even a display with no
--         \"overlays\" still has at least one base \"plane\" that images
--         can be displayed on.
--
--     -   Updated most of the issues.
--
--     -   Added an \"extension type\" section to the specification header.
--
--     -   Re-used the VK_EXT_KHR_surface surface transform enumerations
--         rather than redefining them here.
--
--     -   Updated the example code to use the new semantics.
--
-- -   Revision 13, 2015-08-21 (Ian Elliott)
--
--     -   Renamed this extension and all of its enumerations, types,
--         functions, etc. This makes it compliant with the proposed
--         standard for Vulkan extensions.
--
--     -   Switched from \"revision\" to \"version\", including use of the
--         VK_MAKE_VERSION macro in the header file.
--
-- -   Revision 14, 2015-09-01 (James Jones)
--
--     -   Restore single-field revision number.
--
-- -   Revision 15, 2015-09-08 (James Jones)
--
--     -   Added alpha flags enum.
--
--     -   Added premultiplied alpha support.
--
-- -   Revision 16, 2015-09-08 (James Jones)
--
--     -   Added description section to the spec.
--
--     -   Added issues 16 - 18.
--
-- -   Revision 17, 2015-10-02 (James Jones)
--
--     -   Planes are now a property of the entire device rather than
--         individual displays. This allows planes to be moved between
--         multiple displays on devices that support it.
--
--     -   Added a function to create a VkSurfaceKHR object describing a
--         display plane and mode to align with the new per-platform
--         surface creation conventions.
--
--     -   Removed detailed mode timing data. It was agreed that the mode
--         extents and refresh rate are sufficient for current use cases.
--         Other information could be added back2 in as an extension if it
--         is needed in the future.
--
--     -   Added support for smart\/persistent\/buffered display devices.
--
-- -   Revision 18, 2015-10-26 (Ian Elliott)
--
--     -   Renamed from VK_EXT_KHR_display to VK_KHR_display.
--
-- -   Revision 19, 2015-11-02 (James Jones)
--
--     -   Updated example code to match revision 17 changes.
--
-- -   Revision 20, 2015-11-03 (Daniel Rakos)
--
--     -   Added allocation callbacks to creation functions.
--
-- -   Revision 21, 2015-11-10 (Jesse Hall)
--
--     -   Added VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR, and use
--         VkDisplayPlaneAlphaFlagBitsKHR for
--         VkDisplayPlanePropertiesKHR::alphaMode instead of
--         VkDisplayPlaneAlphaFlagsKHR, since it only represents one mode.
--
--     -   Added reserved flags bitmask to VkDisplayPlanePropertiesKHR.
--
--     -   Use VkSurfaceTransformFlagBitsKHR instead of obsolete
--         VkSurfaceTransformKHR.
--
--     -   Renamed vkGetDisplayPlaneSupportedDisplaysKHR parameters for
--         clarity.
--
-- -   Revision 22, 2015-12-18 (James Jones)
--
--     -   Added missing \"planeIndex\" parameter to
--         vkGetDisplayPlaneSupportedDisplaysKHR()
--
-- -   Revision 23, 2017-03-13 (James Jones)
--
--     -   Closed all remaining issues. The specification and
--         implementations have been shipping with the proposed resolutions
--         for some time now.
--
--     -   Removed the sample code and noted it has been integrated into
--         the official Vulkan SDK cube demo.
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DisplayKHR', 'DisplayModeCreateFlagsKHR',
-- 'DisplayModeCreateInfoKHR', 'Vulkan.Extensions.Handles.DisplayModeKHR',
-- 'DisplayModeParametersKHR', 'DisplayModePropertiesKHR',
-- 'DisplayPlaneAlphaFlagBitsKHR', 'DisplayPlaneAlphaFlagsKHR',
-- 'DisplayPlaneCapabilitiesKHR', 'DisplayPlanePropertiesKHR',
-- 'DisplayPropertiesKHR', 'DisplaySurfaceCreateFlagsKHR',
-- 'DisplaySurfaceCreateInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagsKHR',
-- 'createDisplayModeKHR', 'createDisplayPlaneSurfaceKHR',
-- 'getDisplayModePropertiesKHR', 'getDisplayPlaneCapabilitiesKHR',
-- 'getDisplayPlaneSupportedDisplaysKHR',
-- 'getPhysicalDeviceDisplayPlanePropertiesKHR',
-- 'getPhysicalDeviceDisplayPropertiesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_display Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_display  ( getPhysicalDeviceDisplayPropertiesKHR
                                         , getPhysicalDeviceDisplayPlanePropertiesKHR
                                         , getDisplayPlaneSupportedDisplaysKHR
                                         , getDisplayModePropertiesKHR
                                         , createDisplayModeKHR
                                         , getDisplayPlaneCapabilitiesKHR
                                         , createDisplayPlaneSurfaceKHR
                                         , DisplayPropertiesKHR(..)
                                         , DisplayPlanePropertiesKHR(..)
                                         , DisplayModeParametersKHR(..)
                                         , DisplayModePropertiesKHR(..)
                                         , DisplayModeCreateInfoKHR(..)
                                         , DisplayPlaneCapabilitiesKHR(..)
                                         , DisplaySurfaceCreateInfoKHR(..)
                                         , DisplayModeCreateFlagsKHR(..)
                                         , DisplaySurfaceCreateFlagsKHR(..)
                                         , DisplayPlaneAlphaFlagsKHR
                                         , DisplayPlaneAlphaFlagBitsKHR( DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
                                                                       , DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
                                                                       , DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
                                                                       , DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
                                                                       , ..
                                                                       )
                                         , KHR_DISPLAY_SPEC_VERSION
                                         , pattern KHR_DISPLAY_SPEC_VERSION
                                         , KHR_DISPLAY_EXTENSION_NAME
                                         , pattern KHR_DISPLAY_EXTENSION_NAME
                                         , DisplayKHR(..)
                                         , DisplayModeKHR(..)
                                         , SurfaceKHR(..)
                                         , SurfaceTransformFlagBitsKHR(..)
                                         , SurfaceTransformFlagsKHR
                                         ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Extensions.Handles (DisplayKHR)
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Extensions.Handles (DisplayModeKHR)
import Vulkan.Extensions.Handles (DisplayModeKHR(..))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateDisplayModeKHR))
import Vulkan.Dynamic (InstanceCmds(pVkCreateDisplayPlaneSurfaceKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetDisplayModePropertiesKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetDisplayPlaneCapabilitiesKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetDisplayPlaneSupportedDisplaysKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceDisplayPlanePropertiesKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceDisplayPropertiesKHR))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.FundamentalTypes (Offset2D)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Extensions.Handles (DisplayModeKHR(..))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPropertiesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayPropertiesKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayPropertiesKHR -> IO Result

-- | vkGetPhysicalDeviceDisplayPropertiesKHR - Query information about the
-- available displays
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of display devices available
-- for @physicalDevice@ is returned in @pPropertyCount@. Otherwise,
-- @pPropertyCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pProperties@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pProperties@. If the value of @pPropertyCount@ is less than the
-- number of display devices for @physicalDevice@, at most @pPropertyCount@
-- structures will be written. If @pPropertyCount@ is smaller than the
-- number of display devices available for @physicalDevice@,
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS' to indicate that not all the
-- available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceDisplayPropertiesKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceDisplayPropertiesKHR-pPropertyCount-parameter#
--     @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceDisplayPropertiesKHR-pProperties-parameter#
--     If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'DisplayPropertiesKHR' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'DisplayPropertiesKHR', 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceDisplayPropertiesKHR :: forall io
                                       . (MonadIO io)
                                      => -- | @physicalDevice@ is a physical device.
                                         PhysicalDevice
                                      -> io (Result, ("properties" ::: Vector DisplayPropertiesKHR))
getPhysicalDeviceDisplayPropertiesKHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceDisplayPropertiesKHRPtr = pVkGetPhysicalDeviceDisplayPropertiesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceDisplayPropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceDisplayPropertiesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceDisplayPropertiesKHR' = mkVkGetPhysicalDeviceDisplayPropertiesKHR vkGetPhysicalDeviceDisplayPropertiesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceDisplayPropertiesKHR" (vkGetPhysicalDeviceDisplayPropertiesKHR' physicalDevice' (pPPropertyCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @DisplayPropertiesKHR ((fromIntegral (pPropertyCount)) * 48)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 48) :: Ptr DisplayPropertiesKHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceDisplayPropertiesKHR" (vkGetPhysicalDeviceDisplayPropertiesKHR' physicalDevice' (pPPropertyCount) ((pPProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @DisplayPropertiesKHR (((pPProperties) `advancePtrBytes` (48 * (i)) :: Ptr DisplayPropertiesKHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPlanePropertiesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayPlanePropertiesKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayPlanePropertiesKHR -> IO Result

-- | vkGetPhysicalDeviceDisplayPlanePropertiesKHR - Query the plane
-- properties
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of display planes available
-- for @physicalDevice@ is returned in @pPropertyCount@. Otherwise,
-- @pPropertyCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pProperties@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pProperties@. If the value of @pPropertyCount@ is less than the
-- number of display planes for @physicalDevice@, at most @pPropertyCount@
-- structures will be written.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceDisplayPlanePropertiesKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceDisplayPlanePropertiesKHR-pPropertyCount-parameter#
--     @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceDisplayPlanePropertiesKHR-pProperties-parameter#
--     If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'DisplayPlanePropertiesKHR'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'DisplayPlanePropertiesKHR', 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceDisplayPlanePropertiesKHR :: forall io
                                            . (MonadIO io)
                                           => -- | @physicalDevice@ is a physical device.
                                              PhysicalDevice
                                           -> io (Result, ("properties" ::: Vector DisplayPlanePropertiesKHR))
getPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceDisplayPlanePropertiesKHRPtr = pVkGetPhysicalDeviceDisplayPlanePropertiesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceDisplayPlanePropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceDisplayPlanePropertiesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceDisplayPlanePropertiesKHR' = mkVkGetPhysicalDeviceDisplayPlanePropertiesKHR vkGetPhysicalDeviceDisplayPlanePropertiesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceDisplayPlanePropertiesKHR" (vkGetPhysicalDeviceDisplayPlanePropertiesKHR' physicalDevice' (pPPropertyCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @DisplayPlanePropertiesKHR ((fromIntegral (pPropertyCount)) * 16)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 16) :: Ptr DisplayPlanePropertiesKHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceDisplayPlanePropertiesKHR" (vkGetPhysicalDeviceDisplayPlanePropertiesKHR' physicalDevice' (pPPropertyCount) ((pPProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @DisplayPlanePropertiesKHR (((pPProperties) `advancePtrBytes` (16 * (i)) :: Ptr DisplayPlanePropertiesKHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneSupportedDisplaysKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr Word32 -> Ptr DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> Word32 -> Ptr Word32 -> Ptr DisplayKHR -> IO Result

-- | vkGetDisplayPlaneSupportedDisplaysKHR - Query the list of displays a
-- plane supports
--
-- = Description
--
-- If @pDisplays@ is @NULL@, then the number of displays usable with the
-- specified @planeIndex@ for @physicalDevice@ is returned in
-- @pDisplayCount@. Otherwise, @pDisplayCount@ /must/ point to a variable
-- set by the user to the number of elements in the @pDisplays@ array, and
-- on return the variable is overwritten with the number of handles
-- actually written to @pDisplays@. If the value of @pDisplayCount@ is less
-- than the number of display planes for @physicalDevice@, at most
-- @pDisplayCount@ handles will be written. If @pDisplayCount@ is smaller
-- than the number of displays usable with the specified @planeIndex@ for
-- @physicalDevice@, 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be
-- returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS' to indicate
-- that not all the available values were returned.
--
-- == Valid Usage
--
-- -   #VUID-vkGetDisplayPlaneSupportedDisplaysKHR-planeIndex-01249#
--     @planeIndex@ /must/ be less than the number of display planes
--     supported by the device as determined by calling
--     'getPhysicalDeviceDisplayPlanePropertiesKHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDisplayPlaneSupportedDisplaysKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetDisplayPlaneSupportedDisplaysKHR-pDisplayCount-parameter#
--     @pDisplayCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetDisplayPlaneSupportedDisplaysKHR-pDisplays-parameter# If
--     the value referenced by @pDisplayCount@ is not @0@, and @pDisplays@
--     is not @NULL@, @pDisplays@ /must/ be a valid pointer to an array of
--     @pDisplayCount@ 'Vulkan.Extensions.Handles.DisplayKHR' handles
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getDisplayPlaneSupportedDisplaysKHR :: forall io
                                     . (MonadIO io)
                                    => -- | @physicalDevice@ is a physical device.
                                       PhysicalDevice
                                    -> -- | @planeIndex@ is the plane which the application wishes to use, and
                                       -- /must/ be in the range [0, physical device plane count - 1].
                                       ("planeIndex" ::: Word32)
                                    -> io (Result, ("displays" ::: Vector DisplayKHR))
getDisplayPlaneSupportedDisplaysKHR physicalDevice planeIndex = liftIO . evalContT $ do
  let vkGetDisplayPlaneSupportedDisplaysKHRPtr = pVkGetDisplayPlaneSupportedDisplaysKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetDisplayPlaneSupportedDisplaysKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDisplayPlaneSupportedDisplaysKHR is null" Nothing Nothing
  let vkGetDisplayPlaneSupportedDisplaysKHR' = mkVkGetDisplayPlaneSupportedDisplaysKHR vkGetDisplayPlaneSupportedDisplaysKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPDisplayCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetDisplayPlaneSupportedDisplaysKHR" (vkGetDisplayPlaneSupportedDisplaysKHR' physicalDevice' (planeIndex) (pPDisplayCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDisplayCount <- lift $ peek @Word32 pPDisplayCount
  pPDisplays <- ContT $ bracket (callocBytes @DisplayKHR ((fromIntegral (pDisplayCount)) * 8)) free
  r' <- lift $ traceAroundEvent "vkGetDisplayPlaneSupportedDisplaysKHR" (vkGetDisplayPlaneSupportedDisplaysKHR' physicalDevice' (planeIndex) (pPDisplayCount) (pPDisplays))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pDisplayCount' <- lift $ peek @Word32 pPDisplayCount
  pDisplays' <- lift $ generateM (fromIntegral (pDisplayCount')) (\i -> peek @DisplayKHR ((pPDisplays `advancePtrBytes` (8 * (i)) :: Ptr DisplayKHR)))
  pure $ ((r'), pDisplays')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayModePropertiesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> Ptr Word32 -> Ptr DisplayModePropertiesKHR -> IO Result) -> Ptr PhysicalDevice_T -> DisplayKHR -> Ptr Word32 -> Ptr DisplayModePropertiesKHR -> IO Result

-- | vkGetDisplayModePropertiesKHR - Query the set of mode properties
-- supported by the display
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of display modes available
-- on the specified @display@ for @physicalDevice@ is returned in
-- @pPropertyCount@. Otherwise, @pPropertyCount@ /must/ point to a variable
-- set by the user to the number of elements in the @pProperties@ array,
-- and on return the variable is overwritten with the number of structures
-- actually written to @pProperties@. If the value of @pPropertyCount@ is
-- less than the number of display modes for @physicalDevice@, at most
-- @pPropertyCount@ structures will be written. If @pPropertyCount@ is
-- smaller than the number of display modes available on the specified
-- @display@ for @physicalDevice@, 'Vulkan.Core10.Enums.Result.INCOMPLETE'
-- will be returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS' to
-- indicate that not all the available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDisplayModePropertiesKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetDisplayModePropertiesKHR-display-parameter# @display@
--     /must/ be a valid 'Vulkan.Extensions.Handles.DisplayKHR' handle
--
-- -   #VUID-vkGetDisplayModePropertiesKHR-pPropertyCount-parameter#
--     @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetDisplayModePropertiesKHR-pProperties-parameter# If the
--     value referenced by @pPropertyCount@ is not @0@, and @pProperties@
--     is not @NULL@, @pProperties@ /must/ be a valid pointer to an array
--     of @pPropertyCount@ 'DisplayModePropertiesKHR' structures
--
-- -   #VUID-vkGetDisplayModePropertiesKHR-display-parent# @display@ /must/
--     have been created, allocated, or retrieved from @physicalDevice@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DisplayKHR', 'DisplayModePropertiesKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getDisplayModePropertiesKHR :: forall io
                             . (MonadIO io)
                            => -- | @physicalDevice@ is the physical device associated with @display@.
                               PhysicalDevice
                            -> -- | @display@ is the display to query.
                               DisplayKHR
                            -> io (Result, ("properties" ::: Vector DisplayModePropertiesKHR))
getDisplayModePropertiesKHR physicalDevice display = liftIO . evalContT $ do
  let vkGetDisplayModePropertiesKHRPtr = pVkGetDisplayModePropertiesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetDisplayModePropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDisplayModePropertiesKHR is null" Nothing Nothing
  let vkGetDisplayModePropertiesKHR' = mkVkGetDisplayModePropertiesKHR vkGetDisplayModePropertiesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetDisplayModePropertiesKHR" (vkGetDisplayModePropertiesKHR' physicalDevice' (display) (pPPropertyCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @DisplayModePropertiesKHR ((fromIntegral (pPropertyCount)) * 24)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 24) :: Ptr DisplayModePropertiesKHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetDisplayModePropertiesKHR" (vkGetDisplayModePropertiesKHR' physicalDevice' (display) (pPPropertyCount) ((pPProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @DisplayModePropertiesKHR (((pPProperties) `advancePtrBytes` (24 * (i)) :: Ptr DisplayModePropertiesKHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDisplayModeKHR
  :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> Ptr DisplayModeCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr DisplayModeKHR -> IO Result) -> Ptr PhysicalDevice_T -> DisplayKHR -> Ptr DisplayModeCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr DisplayModeKHR -> IO Result

-- | vkCreateDisplayModeKHR - Create a display mode
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateDisplayModeKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkCreateDisplayModeKHR-display-parameter# @display@ /must/ be
--     a valid 'Vulkan.Extensions.Handles.DisplayKHR' handle
--
-- -   #VUID-vkCreateDisplayModeKHR-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'DisplayModeCreateInfoKHR'
--     structure
--
-- -   #VUID-vkCreateDisplayModeKHR-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateDisplayModeKHR-pMode-parameter# @pMode@ /must/ be a
--     valid pointer to a 'Vulkan.Extensions.Handles.DisplayModeKHR' handle
--
-- -   #VUID-vkCreateDisplayModeKHR-display-parent# @display@ /must/ have
--     been created, allocated, or retrieved from @physicalDevice@
--
-- == Host Synchronization
--
-- -   Host access to @display@ /must/ be externally synchronized
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Extensions.Handles.DisplayKHR', 'DisplayModeCreateInfoKHR',
-- 'Vulkan.Extensions.Handles.DisplayModeKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
createDisplayModeKHR :: forall io
                      . (MonadIO io)
                     => -- | @physicalDevice@ is the physical device associated with @display@.
                        PhysicalDevice
                     -> -- | @display@ is the display to create an additional mode for.
                        DisplayKHR
                     -> -- | @pCreateInfo@ is a 'DisplayModeCreateInfoKHR' structure describing the
                        -- new mode to create.
                        DisplayModeCreateInfoKHR
                     -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                        -- display mode object when there is no more specific allocator available
                        -- (see
                        -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                        ("allocator" ::: Maybe AllocationCallbacks)
                     -> io (DisplayModeKHR)
createDisplayModeKHR physicalDevice display createInfo allocator = liftIO . evalContT $ do
  let vkCreateDisplayModeKHRPtr = pVkCreateDisplayModeKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkCreateDisplayModeKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDisplayModeKHR is null" Nothing Nothing
  let vkCreateDisplayModeKHR' = mkVkCreateDisplayModeKHR vkCreateDisplayModeKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPMode <- ContT $ bracket (callocBytes @DisplayModeKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateDisplayModeKHR" (vkCreateDisplayModeKHR' (physicalDeviceHandle (physicalDevice)) (display) pCreateInfo pAllocator (pPMode))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMode <- lift $ peek @DisplayModeKHR pPMode
  pure $ (pMode)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneCapabilitiesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> DisplayModeKHR -> Word32 -> Ptr DisplayPlaneCapabilitiesKHR -> IO Result) -> Ptr PhysicalDevice_T -> DisplayModeKHR -> Word32 -> Ptr DisplayPlaneCapabilitiesKHR -> IO Result

-- | vkGetDisplayPlaneCapabilitiesKHR - Query capabilities of a mode and
-- plane combination
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDisplayPlaneCapabilitiesKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetDisplayPlaneCapabilitiesKHR-mode-parameter# @mode@ /must/
--     be a valid 'Vulkan.Extensions.Handles.DisplayModeKHR' handle
--
-- -   #VUID-vkGetDisplayPlaneCapabilitiesKHR-pCapabilities-parameter#
--     @pCapabilities@ /must/ be a valid pointer to a
--     'DisplayPlaneCapabilitiesKHR' structure
--
-- == Host Synchronization
--
-- -   Host access to @mode@ /must/ be externally synchronized
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DisplayModeKHR',
-- 'DisplayPlaneCapabilitiesKHR', 'Vulkan.Core10.Handles.PhysicalDevice'
getDisplayPlaneCapabilitiesKHR :: forall io
                                . (MonadIO io)
                               => -- | @physicalDevice@ is the physical device associated with @display@
                                  PhysicalDevice
                               -> -- | @mode@ is the display mode the application intends to program when using
                                  -- the specified plane. Note this parameter also implicitly specifies a
                                  -- display.
                                  DisplayModeKHR
                               -> -- | @planeIndex@ is the plane which the application intends to use with the
                                  -- display, and is less than the number of display planes supported by the
                                  -- device.
                                  ("planeIndex" ::: Word32)
                               -> io (DisplayPlaneCapabilitiesKHR)
getDisplayPlaneCapabilitiesKHR physicalDevice mode planeIndex = liftIO . evalContT $ do
  let vkGetDisplayPlaneCapabilitiesKHRPtr = pVkGetDisplayPlaneCapabilitiesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetDisplayPlaneCapabilitiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDisplayPlaneCapabilitiesKHR is null" Nothing Nothing
  let vkGetDisplayPlaneCapabilitiesKHR' = mkVkGetDisplayPlaneCapabilitiesKHR vkGetDisplayPlaneCapabilitiesKHRPtr
  pPCapabilities <- ContT (withZeroCStruct @DisplayPlaneCapabilitiesKHR)
  r <- lift $ traceAroundEvent "vkGetDisplayPlaneCapabilitiesKHR" (vkGetDisplayPlaneCapabilitiesKHR' (physicalDeviceHandle (physicalDevice)) (mode) (planeIndex) (pPCapabilities))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCapabilities <- lift $ peekCStruct @DisplayPlaneCapabilitiesKHR pPCapabilities
  pure $ (pCapabilities)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDisplayPlaneSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr DisplaySurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr DisplaySurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- | vkCreateDisplayPlaneSurfaceKHR - Create a
-- 'Vulkan.Extensions.Handles.SurfaceKHR' structure representing a display
-- plane and mode
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateDisplayPlaneSurfaceKHR-instance-parameter# @instance@
--     /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateDisplayPlaneSurfaceKHR-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'DisplaySurfaceCreateInfoKHR' structure
--
-- -   #VUID-vkCreateDisplayPlaneSurfaceKHR-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateDisplayPlaneSurfaceKHR-pSurface-parameter# @pSurface@
--     /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.SurfaceKHR' handle
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'DisplaySurfaceCreateInfoKHR', 'Vulkan.Core10.Handles.Instance',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
createDisplayPlaneSurfaceKHR :: forall io
                              . (MonadIO io)
                             => -- | @instance@ is the instance corresponding to the physical device the
                                -- targeted display is on.
                                Instance
                             -> -- | @pCreateInfo@ is a pointer to a 'DisplaySurfaceCreateInfoKHR' structure
                                -- specifying which mode, plane, and other parameters to use, as described
                                -- below.
                                DisplaySurfaceCreateInfoKHR
                             -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                                -- surface object when there is no more specific allocator available (see
                                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                                ("allocator" ::: Maybe AllocationCallbacks)
                             -> io (SurfaceKHR)
createDisplayPlaneSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateDisplayPlaneSurfaceKHRPtr = pVkCreateDisplayPlaneSurfaceKHR (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateDisplayPlaneSurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDisplayPlaneSurfaceKHR is null" Nothing Nothing
  let vkCreateDisplayPlaneSurfaceKHR' = mkVkCreateDisplayPlaneSurfaceKHR vkCreateDisplayPlaneSurfaceKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateDisplayPlaneSurfaceKHR" (vkCreateDisplayPlaneSurfaceKHR' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)


-- | VkDisplayPropertiesKHR - Structure describing an available display
-- device
--
-- = Description
--
-- Note
--
-- For devices which have no natural value to return here, implementations
-- /should/ return the maximum resolution supported.
--
-- Note
--
-- Persistent presents /may/ have higher latency, and /may/ use less power
-- when the screen content is updated infrequently, or when only a portion
-- of the screen needs to be updated in most frames.
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayProperties2KHR',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagsKHR',
-- 'getPhysicalDeviceDisplayPropertiesKHR'
data DisplayPropertiesKHR = DisplayPropertiesKHR
  { -- | @display@ is a handle that is used to refer to the display described
    -- here. This handle will be valid for the lifetime of the Vulkan instance.
    display :: DisplayKHR
  , -- | @displayName@ is a pointer to a null-terminated UTF-8 string containing
    -- the name of the display. Generally, this will be the name provided by
    -- the display’s EDID. It /can/ be @NULL@ if no suitable name is available.
    -- If not @NULL@, the memory it points to /must/ remain accessible as long
    -- as @display@ is valid.
    displayName :: ByteString
  , -- | @physicalDimensions@ describes the physical width and height of the
    -- visible portion of the display, in millimeters.
    physicalDimensions :: Extent2D
  , -- | @physicalResolution@ describes the physical, native, or preferred
    -- resolution of the display.
    physicalResolution :: Extent2D
  , -- | @supportedTransforms@ is a bitmask of
    -- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR'
    -- describing which transforms are supported by this display.
    supportedTransforms :: SurfaceTransformFlagsKHR
  , -- | @planeReorderPossible@ tells whether the planes on this display /can/
    -- have their z order changed. If this is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE', the application /can/ re-arrange
    -- the planes on this display in any order relative to each other.
    planeReorderPossible :: Bool
  , -- | @persistentContent@ tells whether the display supports
    -- self-refresh\/internal buffering. If this is true, the application /can/
    -- submit persistent present operations on swapchains created against this
    -- display.
    persistentContent :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPropertiesKHR)
#endif
deriving instance Show DisplayPropertiesKHR

instance ToCStruct DisplayPropertiesKHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPropertiesKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr DisplayKHR)) (display)
    displayName'' <- ContT $ useAsCString (displayName)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr CChar))) displayName''
    lift $ poke ((p `plusPtr` 16 :: Ptr Extent2D)) (physicalDimensions)
    lift $ poke ((p `plusPtr` 24 :: Ptr Extent2D)) (physicalResolution)
    lift $ poke ((p `plusPtr` 32 :: Ptr SurfaceTransformFlagsKHR)) (supportedTransforms)
    lift $ poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (planeReorderPossible))
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (persistentContent))
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr DisplayKHR)) (zero)
    displayName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr CChar))) displayName''
    lift $ poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance FromCStruct DisplayPropertiesKHR where
  peekCStruct p = do
    display <- peek @DisplayKHR ((p `plusPtr` 0 :: Ptr DisplayKHR))
    displayName <- packCString =<< peek ((p `plusPtr` 8 :: Ptr (Ptr CChar)))
    physicalDimensions <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    physicalResolution <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    supportedTransforms <- peek @SurfaceTransformFlagsKHR ((p `plusPtr` 32 :: Ptr SurfaceTransformFlagsKHR))
    planeReorderPossible <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    persistentContent <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    pure $ DisplayPropertiesKHR
             display displayName physicalDimensions physicalResolution supportedTransforms (bool32ToBool planeReorderPossible) (bool32ToBool persistentContent)

instance Zero DisplayPropertiesKHR where
  zero = DisplayPropertiesKHR
           zero
           mempty
           zero
           zero
           zero
           zero
           zero


-- | VkDisplayPlanePropertiesKHR - Structure describing display plane
-- properties
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayPlaneProperties2KHR',
-- 'getPhysicalDeviceDisplayPlanePropertiesKHR'
data DisplayPlanePropertiesKHR = DisplayPlanePropertiesKHR
  { -- | @currentDisplay@ is the handle of the display the plane is currently
    -- associated with. If the plane is not currently attached to any displays,
    -- this will be 'Vulkan.Core10.APIConstants.NULL_HANDLE'.
    currentDisplay :: DisplayKHR
  , -- | @currentStackIndex@ is the current z-order of the plane. This will be
    -- between 0 and the value returned by
    -- 'getPhysicalDeviceDisplayPlanePropertiesKHR' in @pPropertyCount@.
    currentStackIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPlanePropertiesKHR)
#endif
deriving instance Show DisplayPlanePropertiesKHR

instance ToCStruct DisplayPlanePropertiesKHR where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPlanePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DisplayKHR)) (currentDisplay)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (currentStackIndex)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DisplayKHR)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct DisplayPlanePropertiesKHR where
  peekCStruct p = do
    currentDisplay <- peek @DisplayKHR ((p `plusPtr` 0 :: Ptr DisplayKHR))
    currentStackIndex <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ DisplayPlanePropertiesKHR
             currentDisplay currentStackIndex

instance Storable DisplayPlanePropertiesKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayPlanePropertiesKHR where
  zero = DisplayPlanePropertiesKHR
           zero
           zero


-- | VkDisplayModeParametersKHR - Structure describing display parameters
-- associated with a display mode
--
-- = Description
--
-- Note
--
-- For example, a 60Hz display mode would report a @refreshRate@ of 60,000.
--
-- == Valid Usage
--
-- -   #VUID-VkDisplayModeParametersKHR-width-01990# The @width@ member of
--     @visibleRegion@ /must/ be greater than @0@
--
-- -   #VUID-VkDisplayModeParametersKHR-height-01991# The @height@ member
--     of @visibleRegion@ /must/ be greater than @0@
--
-- -   #VUID-VkDisplayModeParametersKHR-refreshRate-01992# @refreshRate@
--     /must/ be greater than @0@
--
-- = See Also
--
-- 'DisplayModeCreateInfoKHR', 'DisplayModePropertiesKHR',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D'
data DisplayModeParametersKHR = DisplayModeParametersKHR
  { -- | @visibleRegion@ is the 2D extents of the visible region.
    visibleRegion :: Extent2D
  , -- | @refreshRate@ is a @uint32_t@ that is the number of times the display is
    -- refreshed each second multiplied by 1000.
    refreshRate :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayModeParametersKHR)
#endif
deriving instance Show DisplayModeParametersKHR

instance ToCStruct DisplayModeParametersKHR where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayModeParametersKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Extent2D)) (visibleRegion)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (refreshRate)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct DisplayModeParametersKHR where
  peekCStruct p = do
    visibleRegion <- peekCStruct @Extent2D ((p `plusPtr` 0 :: Ptr Extent2D))
    refreshRate <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ DisplayModeParametersKHR
             visibleRegion refreshRate

instance Storable DisplayModeParametersKHR where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayModeParametersKHR where
  zero = DisplayModeParametersKHR
           zero
           zero


-- | VkDisplayModePropertiesKHR - Structure describing display mode
-- properties
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DisplayModeKHR', 'DisplayModeParametersKHR',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayModeProperties2KHR',
-- 'getDisplayModePropertiesKHR'
data DisplayModePropertiesKHR = DisplayModePropertiesKHR
  { -- | @displayMode@ is a handle to the display mode described in this
    -- structure. This handle will be valid for the lifetime of the Vulkan
    -- instance.
    displayMode :: DisplayModeKHR
  , -- | @parameters@ is a 'DisplayModeParametersKHR' structure describing the
    -- display parameters associated with @displayMode@.
    parameters :: DisplayModeParametersKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayModePropertiesKHR)
#endif
deriving instance Show DisplayModePropertiesKHR

instance ToCStruct DisplayModePropertiesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayModePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DisplayModeKHR)) (displayMode)
    poke ((p `plusPtr` 8 :: Ptr DisplayModeParametersKHR)) (parameters)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DisplayModeKHR)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DisplayModeParametersKHR)) (zero)
    f

instance FromCStruct DisplayModePropertiesKHR where
  peekCStruct p = do
    displayMode <- peek @DisplayModeKHR ((p `plusPtr` 0 :: Ptr DisplayModeKHR))
    parameters <- peekCStruct @DisplayModeParametersKHR ((p `plusPtr` 8 :: Ptr DisplayModeParametersKHR))
    pure $ DisplayModePropertiesKHR
             displayMode parameters

instance Storable DisplayModePropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayModePropertiesKHR where
  zero = DisplayModePropertiesKHR
           zero
           zero


-- | VkDisplayModeCreateInfoKHR - Structure specifying parameters of a newly
-- created display mode object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DisplayModeCreateFlagsKHR', 'DisplayModeParametersKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createDisplayModeKHR'
data DisplayModeCreateInfoKHR = DisplayModeCreateInfoKHR
  { -- | @flags@ is reserved for future use, and /must/ be zero.
    --
    -- #VUID-VkDisplayModeCreateInfoKHR-flags-zerobitmask# @flags@ /must/ be
    -- @0@
    flags :: DisplayModeCreateFlagsKHR
  , -- | @parameters@ is a 'DisplayModeParametersKHR' structure describing the
    -- display parameters to use in creating the new mode. If the parameters
    -- are not compatible with the specified display, the implementation /must/
    -- return 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'.
    --
    -- #VUID-VkDisplayModeCreateInfoKHR-parameters-parameter# @parameters@
    -- /must/ be a valid 'DisplayModeParametersKHR' structure
    parameters :: DisplayModeParametersKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayModeCreateInfoKHR)
#endif
deriving instance Show DisplayModeCreateInfoKHR

instance ToCStruct DisplayModeCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayModeCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayModeCreateFlagsKHR)) (flags)
    poke ((p `plusPtr` 20 :: Ptr DisplayModeParametersKHR)) (parameters)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr DisplayModeParametersKHR)) (zero)
    f

instance FromCStruct DisplayModeCreateInfoKHR where
  peekCStruct p = do
    flags <- peek @DisplayModeCreateFlagsKHR ((p `plusPtr` 16 :: Ptr DisplayModeCreateFlagsKHR))
    parameters <- peekCStruct @DisplayModeParametersKHR ((p `plusPtr` 20 :: Ptr DisplayModeParametersKHR))
    pure $ DisplayModeCreateInfoKHR
             flags parameters

instance Storable DisplayModeCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayModeCreateInfoKHR where
  zero = DisplayModeCreateInfoKHR
           zero
           zero


-- | VkDisplayPlaneCapabilitiesKHR - Structure describing capabilities of a
-- mode and plane combination
--
-- = Description
--
-- The minimum and maximum position and extent fields describe the
-- implementation limits, if any, as they apply to the specified display
-- mode and plane. Vendors /may/ support displaying a subset of a
-- swapchain’s presentable images on the specified display plane. This is
-- expressed by returning @minSrcPosition@, @maxSrcPosition@,
-- @minSrcExtent@, and @maxSrcExtent@ values that indicate a range of
-- possible positions and sizes /may/ be used to specify the region within
-- the presentable images that source pixels will be read from when
-- creating a swapchain on the specified display mode and plane.
--
-- Vendors /may/ also support mapping the presentable images’ content to a
-- subset or superset of the visible region in the specified display mode.
-- This is expressed by returning @minDstPosition@, @maxDstPosition@,
-- @minDstExtent@ and @maxDstExtent@ values that indicate a range of
-- possible positions and sizes /may/ be used to describe the region within
-- the display mode that the source pixels will be mapped to.
--
-- Other vendors /may/ support only a 1-1 mapping between pixels in the
-- presentable images and the display mode. This /may/ be indicated by
-- returning (0,0) for @minSrcPosition@, @maxSrcPosition@,
-- @minDstPosition@, and @maxDstPosition@, and (display mode width, display
-- mode height) for @minSrcExtent@, @maxSrcExtent@, @minDstExtent@, and
-- @maxDstExtent@.
--
-- These values indicate the limits of the implementation’s individual
-- fields. Not all combinations of values within the offset and extent
-- ranges returned in 'DisplayPlaneCapabilitiesKHR' are guaranteed to be
-- supported. Presentation requests specifying unsupported combinations
-- /may/ fail.
--
-- = See Also
--
-- 'DisplayPlaneAlphaFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayPlaneCapabilities2KHR',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.FundamentalTypes.Offset2D',
-- 'getDisplayPlaneCapabilitiesKHR'
data DisplayPlaneCapabilitiesKHR = DisplayPlaneCapabilitiesKHR
  { -- | @supportedAlpha@ is a bitmask of 'DisplayPlaneAlphaFlagBitsKHR'
    -- describing the supported alpha blending modes.
    supportedAlpha :: DisplayPlaneAlphaFlagsKHR
  , -- | @minSrcPosition@ is the minimum source rectangle offset supported by
    -- this plane using the specified mode.
    minSrcPosition :: Offset2D
  , -- | @maxSrcPosition@ is the maximum source rectangle offset supported by
    -- this plane using the specified mode. The @x@ and @y@ components of
    -- @maxSrcPosition@ /must/ each be greater than or equal to the @x@ and @y@
    -- components of @minSrcPosition@, respectively.
    maxSrcPosition :: Offset2D
  , -- | @minSrcExtent@ is the minimum source rectangle size supported by this
    -- plane using the specified mode.
    minSrcExtent :: Extent2D
  , -- | @maxSrcExtent@ is the maximum source rectangle size supported by this
    -- plane using the specified mode.
    maxSrcExtent :: Extent2D
  , -- | @minDstPosition@, @maxDstPosition@, @minDstExtent@, @maxDstExtent@ all
    -- have similar semantics to their corresponding @*Src*@ equivalents, but
    -- apply to the output region within the mode rather than the input region
    -- within the source image. Unlike the @*Src*@ offsets, @minDstPosition@
    -- and @maxDstPosition@ /may/ contain negative values.
    minDstPosition :: Offset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "maxDstPosition"
    maxDstPosition :: Offset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "minDstExtent"
    minDstExtent :: Extent2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "maxDstExtent"
    maxDstExtent :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPlaneCapabilitiesKHR)
#endif
deriving instance Show DisplayPlaneCapabilitiesKHR

instance ToCStruct DisplayPlaneCapabilitiesKHR where
  withCStruct x f = allocaBytesAligned 68 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPlaneCapabilitiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DisplayPlaneAlphaFlagsKHR)) (supportedAlpha)
    poke ((p `plusPtr` 4 :: Ptr Offset2D)) (minSrcPosition)
    poke ((p `plusPtr` 12 :: Ptr Offset2D)) (maxSrcPosition)
    poke ((p `plusPtr` 20 :: Ptr Extent2D)) (minSrcExtent)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (maxSrcExtent)
    poke ((p `plusPtr` 36 :: Ptr Offset2D)) (minDstPosition)
    poke ((p `plusPtr` 44 :: Ptr Offset2D)) (maxDstPosition)
    poke ((p `plusPtr` 52 :: Ptr Extent2D)) (minDstExtent)
    poke ((p `plusPtr` 60 :: Ptr Extent2D)) (maxDstExtent)
    f
  cStructSize = 68
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 4 :: Ptr Offset2D)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Offset2D)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Offset2D)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Offset2D)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct DisplayPlaneCapabilitiesKHR where
  peekCStruct p = do
    supportedAlpha <- peek @DisplayPlaneAlphaFlagsKHR ((p `plusPtr` 0 :: Ptr DisplayPlaneAlphaFlagsKHR))
    minSrcPosition <- peekCStruct @Offset2D ((p `plusPtr` 4 :: Ptr Offset2D))
    maxSrcPosition <- peekCStruct @Offset2D ((p `plusPtr` 12 :: Ptr Offset2D))
    minSrcExtent <- peekCStruct @Extent2D ((p `plusPtr` 20 :: Ptr Extent2D))
    maxSrcExtent <- peekCStruct @Extent2D ((p `plusPtr` 28 :: Ptr Extent2D))
    minDstPosition <- peekCStruct @Offset2D ((p `plusPtr` 36 :: Ptr Offset2D))
    maxDstPosition <- peekCStruct @Offset2D ((p `plusPtr` 44 :: Ptr Offset2D))
    minDstExtent <- peekCStruct @Extent2D ((p `plusPtr` 52 :: Ptr Extent2D))
    maxDstExtent <- peekCStruct @Extent2D ((p `plusPtr` 60 :: Ptr Extent2D))
    pure $ DisplayPlaneCapabilitiesKHR
             supportedAlpha minSrcPosition maxSrcPosition minSrcExtent maxSrcExtent minDstPosition maxDstPosition minDstExtent maxDstExtent

instance Storable DisplayPlaneCapabilitiesKHR where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayPlaneCapabilitiesKHR where
  zero = DisplayPlaneCapabilitiesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDisplaySurfaceCreateInfoKHR - Structure specifying parameters of a
-- newly created display plane surface object
--
-- = Description
--
-- Note
--
-- Creating a display surface /must/ not modify the state of the displays,
-- planes, or other resources it names. For example, it /must/ not apply
-- the specified mode to be set on the associated display. Application of
-- display configuration occurs as a side effect of presenting to a display
-- surface.
--
-- == Valid Usage
--
-- -   #VUID-VkDisplaySurfaceCreateInfoKHR-planeIndex-01252# @planeIndex@
--     /must/ be less than the number of display planes supported by the
--     device as determined by calling
--     'getPhysicalDeviceDisplayPlanePropertiesKHR'
--
-- -   #VUID-VkDisplaySurfaceCreateInfoKHR-planeReorderPossible-01253# If
--     the @planeReorderPossible@ member of the 'DisplayPropertiesKHR'
--     structure returned by 'getPhysicalDeviceDisplayPropertiesKHR' for
--     the display corresponding to @displayMode@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE' then @planeStackIndex@ /must/
--     be less than the number of display planes supported by the device as
--     determined by calling 'getPhysicalDeviceDisplayPlanePropertiesKHR';
--     otherwise @planeStackIndex@ /must/ equal the @currentStackIndex@
--     member of 'DisplayPlanePropertiesKHR' returned by
--     'getPhysicalDeviceDisplayPlanePropertiesKHR' for the display plane
--     corresponding to @displayMode@
--
-- -   #VUID-VkDisplaySurfaceCreateInfoKHR-alphaMode-01254# If @alphaMode@
--     is 'DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR' then @globalAlpha@ /must/ be
--     between @0@ and @1@, inclusive
--
-- -   #VUID-VkDisplaySurfaceCreateInfoKHR-alphaMode-01255# @alphaMode@
--     /must/ be @0@ or one of the bits present in the @supportedAlpha@
--     member of 'DisplayPlaneCapabilitiesKHR' returned by
--     'getDisplayPlaneCapabilitiesKHR' for the display plane corresponding
--     to @displayMode@
--
-- -   #VUID-VkDisplaySurfaceCreateInfoKHR-width-01256# The @width@ and
--     @height@ members of @imageExtent@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxImageDimension2D@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDisplaySurfaceCreateInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR'
--
-- -   #VUID-VkDisplaySurfaceCreateInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkDisplaySurfaceCreateInfoKHR-flags-zerobitmask# @flags@
--     /must/ be @0@
--
-- -   #VUID-VkDisplaySurfaceCreateInfoKHR-displayMode-parameter#
--     @displayMode@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DisplayModeKHR' handle
--
-- -   #VUID-VkDisplaySurfaceCreateInfoKHR-transform-parameter# @transform@
--     /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR' value
--
-- -   #VUID-VkDisplaySurfaceCreateInfoKHR-alphaMode-parameter# @alphaMode@
--     /must/ be a valid 'DisplayPlaneAlphaFlagBitsKHR' value
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DisplayModeKHR',
-- 'DisplayPlaneAlphaFlagBitsKHR', 'DisplaySurfaceCreateFlagsKHR',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR',
-- 'createDisplayPlaneSurfaceKHR'
data DisplaySurfaceCreateInfoKHR = DisplaySurfaceCreateInfoKHR
  { -- | @flags@ is reserved for future use, and /must/ be zero.
    flags :: DisplaySurfaceCreateFlagsKHR
  , -- | @displayMode@ is a 'Vulkan.Extensions.Handles.DisplayModeKHR' handle
    -- specifying the mode to use when displaying this surface.
    displayMode :: DisplayModeKHR
  , -- | @planeIndex@ is the plane on which this surface appears.
    planeIndex :: Word32
  , -- | @planeStackIndex@ is the z-order of the plane.
    planeStackIndex :: Word32
  , -- | @transform@ is a
    -- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR' value
    -- specifying the transformation to apply to images as part of the scanout
    -- operation.
    transform :: SurfaceTransformFlagBitsKHR
  , -- | @globalAlpha@ is the global alpha value. This value is ignored if
    -- @alphaMode@ is not 'DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR'.
    globalAlpha :: Float
  , -- | @alphaMode@ is a 'DisplayPlaneAlphaFlagBitsKHR' value specifying the
    -- type of alpha blending to use.
    alphaMode :: DisplayPlaneAlphaFlagBitsKHR
  , -- | @imageExtent@ The size of the presentable images to use with the
    -- surface.
    imageExtent :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplaySurfaceCreateInfoKHR)
#endif
deriving instance Show DisplaySurfaceCreateInfoKHR

instance ToCStruct DisplaySurfaceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplaySurfaceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplaySurfaceCreateFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr DisplayModeKHR)) (displayMode)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (planeIndex)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (planeStackIndex)
    poke ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR)) (transform)
    poke ((p `plusPtr` 44 :: Ptr CFloat)) (CFloat (globalAlpha))
    poke ((p `plusPtr` 48 :: Ptr DisplayPlaneAlphaFlagBitsKHR)) (alphaMode)
    poke ((p `plusPtr` 52 :: Ptr Extent2D)) (imageExtent)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr DisplayModeKHR)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    poke ((p `plusPtr` 44 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 48 :: Ptr DisplayPlaneAlphaFlagBitsKHR)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct DisplaySurfaceCreateInfoKHR where
  peekCStruct p = do
    flags <- peek @DisplaySurfaceCreateFlagsKHR ((p `plusPtr` 16 :: Ptr DisplaySurfaceCreateFlagsKHR))
    displayMode <- peek @DisplayModeKHR ((p `plusPtr` 24 :: Ptr DisplayModeKHR))
    planeIndex <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    planeStackIndex <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    transform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR))
    globalAlpha <- peek @CFloat ((p `plusPtr` 44 :: Ptr CFloat))
    alphaMode <- peek @DisplayPlaneAlphaFlagBitsKHR ((p `plusPtr` 48 :: Ptr DisplayPlaneAlphaFlagBitsKHR))
    imageExtent <- peekCStruct @Extent2D ((p `plusPtr` 52 :: Ptr Extent2D))
    pure $ DisplaySurfaceCreateInfoKHR
             flags displayMode planeIndex planeStackIndex transform (coerce @CFloat @Float globalAlpha) alphaMode imageExtent

instance Storable DisplaySurfaceCreateInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplaySurfaceCreateInfoKHR where
  zero = DisplaySurfaceCreateInfoKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDisplayModeCreateFlagsKHR - Reserved for future use
--
-- = Description
--
-- 'DisplayModeCreateFlagsKHR' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'DisplayModeCreateInfoKHR'
newtype DisplayModeCreateFlagsKHR = DisplayModeCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDisplayModeCreateFlagsKHR :: String
conNameDisplayModeCreateFlagsKHR = "DisplayModeCreateFlagsKHR"

enumPrefixDisplayModeCreateFlagsKHR :: String
enumPrefixDisplayModeCreateFlagsKHR = ""

showTableDisplayModeCreateFlagsKHR :: [(DisplayModeCreateFlagsKHR, String)]
showTableDisplayModeCreateFlagsKHR = []

instance Show DisplayModeCreateFlagsKHR where
  showsPrec = enumShowsPrec enumPrefixDisplayModeCreateFlagsKHR
                            showTableDisplayModeCreateFlagsKHR
                            conNameDisplayModeCreateFlagsKHR
                            (\(DisplayModeCreateFlagsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DisplayModeCreateFlagsKHR where
  readPrec = enumReadPrec enumPrefixDisplayModeCreateFlagsKHR
                          showTableDisplayModeCreateFlagsKHR
                          conNameDisplayModeCreateFlagsKHR
                          DisplayModeCreateFlagsKHR


-- | VkDisplaySurfaceCreateFlagsKHR - Reserved for future use
--
-- = Description
--
-- 'DisplaySurfaceCreateFlagsKHR' is a bitmask type for setting a mask, but
-- is currently reserved for future use.
--
-- = See Also
--
-- 'DisplaySurfaceCreateInfoKHR'
newtype DisplaySurfaceCreateFlagsKHR = DisplaySurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDisplaySurfaceCreateFlagsKHR :: String
conNameDisplaySurfaceCreateFlagsKHR = "DisplaySurfaceCreateFlagsKHR"

enumPrefixDisplaySurfaceCreateFlagsKHR :: String
enumPrefixDisplaySurfaceCreateFlagsKHR = ""

showTableDisplaySurfaceCreateFlagsKHR :: [(DisplaySurfaceCreateFlagsKHR, String)]
showTableDisplaySurfaceCreateFlagsKHR = []

instance Show DisplaySurfaceCreateFlagsKHR where
  showsPrec = enumShowsPrec enumPrefixDisplaySurfaceCreateFlagsKHR
                            showTableDisplaySurfaceCreateFlagsKHR
                            conNameDisplaySurfaceCreateFlagsKHR
                            (\(DisplaySurfaceCreateFlagsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DisplaySurfaceCreateFlagsKHR where
  readPrec = enumReadPrec enumPrefixDisplaySurfaceCreateFlagsKHR
                          showTableDisplaySurfaceCreateFlagsKHR
                          conNameDisplaySurfaceCreateFlagsKHR
                          DisplaySurfaceCreateFlagsKHR


type DisplayPlaneAlphaFlagsKHR = DisplayPlaneAlphaFlagBitsKHR

-- | VkDisplayPlaneAlphaFlagBitsKHR - Alpha blending type
--
-- = See Also
--
-- 'DisplayPlaneAlphaFlagsKHR', 'DisplaySurfaceCreateInfoKHR'
newtype DisplayPlaneAlphaFlagBitsKHR = DisplayPlaneAlphaFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR' specifies that the source image
-- will be treated as opaque.
pattern DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR                  = DisplayPlaneAlphaFlagBitsKHR 0x00000001
-- | 'DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR' specifies that a global alpha value
-- /must/ be specified that will be applied to all pixels in the source
-- image.
pattern DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR                  = DisplayPlaneAlphaFlagBitsKHR 0x00000002
-- | 'DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR' specifies that the alpha value
-- will be determined by the alpha channel of the source image’s pixels. If
-- the source format contains no alpha values, no blending will be applied.
-- The source alpha values are not premultiplied into the source image’s
-- other color channels.
pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR               = DisplayPlaneAlphaFlagBitsKHR 0x00000004
-- | 'DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR' is equivalent to
-- 'DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR', except the source alpha values
-- are assumed to be premultiplied into the source image’s other color
-- channels.
pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = DisplayPlaneAlphaFlagBitsKHR 0x00000008

conNameDisplayPlaneAlphaFlagBitsKHR :: String
conNameDisplayPlaneAlphaFlagBitsKHR = "DisplayPlaneAlphaFlagBitsKHR"

enumPrefixDisplayPlaneAlphaFlagBitsKHR :: String
enumPrefixDisplayPlaneAlphaFlagBitsKHR = "DISPLAY_PLANE_ALPHA_"

showTableDisplayPlaneAlphaFlagBitsKHR :: [(DisplayPlaneAlphaFlagBitsKHR, String)]
showTableDisplayPlaneAlphaFlagBitsKHR =
  [ (DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR                 , "OPAQUE_BIT_KHR")
  , (DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR                 , "GLOBAL_BIT_KHR")
  , (DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR              , "PER_PIXEL_BIT_KHR")
  , (DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR, "PER_PIXEL_PREMULTIPLIED_BIT_KHR")
  ]

instance Show DisplayPlaneAlphaFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixDisplayPlaneAlphaFlagBitsKHR
                            showTableDisplayPlaneAlphaFlagBitsKHR
                            conNameDisplayPlaneAlphaFlagBitsKHR
                            (\(DisplayPlaneAlphaFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DisplayPlaneAlphaFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixDisplayPlaneAlphaFlagBitsKHR
                          showTableDisplayPlaneAlphaFlagBitsKHR
                          conNameDisplayPlaneAlphaFlagBitsKHR
                          DisplayPlaneAlphaFlagBitsKHR


type KHR_DISPLAY_SPEC_VERSION = 23

-- No documentation found for TopLevel "VK_KHR_DISPLAY_SPEC_VERSION"
pattern KHR_DISPLAY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DISPLAY_SPEC_VERSION = 23


type KHR_DISPLAY_EXTENSION_NAME = "VK_KHR_display"

-- No documentation found for TopLevel "VK_KHR_DISPLAY_EXTENSION_NAME"
pattern KHR_DISPLAY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DISPLAY_EXTENSION_NAME = "VK_KHR_display"

