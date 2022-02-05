{-# language CPP #-}
-- | = Name
--
-- VK_KHR_swapchain - device extension
--
-- == VK_KHR_swapchain
--
-- [__Name String__]
--     @VK_KHR_swapchain@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     2
--
-- [__Revision__]
--     70
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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_swapchain] @cubanismo%0A<<Here describe the issue or question you have about the VK_KHR_swapchain extension>> >
--
--     -   Ian Elliott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_swapchain] @ianelliottus%0A<<Here describe the issue or question you have about the VK_KHR_swapchain extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-10-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with Vulkan 1.1
--
-- [__Contributors__]
--
--     -   Patrick Doane, Blizzard
--
--     -   Ian Elliott, LunarG
--
--     -   Jesse Hall, Google
--
--     -   Mathias Heyer, NVIDIA
--
--     -   James Jones, NVIDIA
--
--     -   David Mao, AMD
--
--     -   Norbert Nopper, Freescale
--
--     -   Alon Or-bach, Samsung
--
--     -   Daniel Rakos, AMD
--
--     -   Graham Sellers, AMD
--
--     -   Jeff Vigil, Qualcomm
--
--     -   Chia-I Wu, LunarG
--
--     -   Jason Ekstrand, Intel
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Ray Smith, ARM
--
-- == Description
--
-- The @VK_KHR_swapchain@ extension is the device-level companion to the
-- @VK_KHR_surface@ extension. It introduces
-- 'Vulkan.Extensions.Handles.SwapchainKHR' objects, which provide the
-- ability to present rendering results to a surface.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.SwapchainKHR'
--
-- == New Commands
--
-- -   'acquireNextImageKHR'
--
-- -   'createSwapchainKHR'
--
-- -   'destroySwapchainKHR'
--
-- -   'getSwapchainImagesKHR'
--
-- -   'queuePresentKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- is supported:
--
-- -   'acquireNextImage2KHR'
--
-- -   'getDeviceGroupPresentCapabilitiesKHR'
--
-- -   'getDeviceGroupSurfacePresentModesKHR'
--
-- -   'getPhysicalDevicePresentRectanglesKHR'
--
-- == New Structures
--
-- -   'PresentInfoKHR'
--
-- -   'SwapchainCreateInfoKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- is supported:
--
-- -   'AcquireNextImageInfoKHR'
--
-- -   'DeviceGroupPresentCapabilitiesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo':
--
--     -   'BindImageMemorySwapchainInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'ImageSwapchainCreateInfoKHR'
--
-- -   Extending 'PresentInfoKHR':
--
--     -   'DeviceGroupPresentInfoKHR'
--
-- -   Extending 'SwapchainCreateInfoKHR':
--
--     -   'DeviceGroupSwapchainCreateInfoKHR'
--
-- == New Enums
--
-- -   'SwapchainCreateFlagBitsKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- is supported:
--
-- -   'DeviceGroupPresentModeFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'SwapchainCreateFlagsKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- is supported:
--
-- -   'DeviceGroupPresentModeFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SWAPCHAIN_EXTENSION_NAME'
--
-- -   'KHR_SWAPCHAIN_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_SWAPCHAIN_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.SUBOPTIMAL_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR'
--
-- -   Extending 'SwapchainCreateFlagBitsKHR':
--
--     -   'SWAPCHAIN_CREATE_PROTECTED_BIT_KHR'
--
--     -   'SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR'
--
-- == Issues
--
-- 1) Does this extension allow the application to specify the memory
-- backing of the presentable images?
--
-- __RESOLVED__: No. Unlike standard images, the implementation will
-- allocate the memory backing of the presentable image.
--
-- 2) What operations are allowed on presentable images?
--
-- __RESOLVED__: This is determined by the image usage flags specified when
-- creating the presentable image’s swapchain.
--
-- 3) Does this extension support MSAA presentable images?
--
-- __RESOLVED__: No. Presentable images are always single-sampled.
-- Multi-sampled rendering must use regular images. To present the
-- rendering results the application must manually resolve the multi-
-- sampled image to a single-sampled presentable image prior to
-- presentation.
--
-- 4) Does this extension support stereo\/multi-view presentable images?
--
-- __RESOLVED__: Yes. The number of views associated with a presentable
-- image is determined by the @imageArrayLayers@ specified when creating a
-- swapchain. All presentable images in a given swapchain use the same
-- array size.
--
-- 5) Are the layers of stereo presentable images half-sized?
--
-- __RESOLVED__: No. The image extents always match those requested by the
-- application.
--
-- 6) Do the “present” and “acquire next image” commands operate on a
-- queue? If not, do they need to include explicit semaphore objects to
-- interlock them with queue operations?
--
-- __RESOLVED__: The present command operates on a queue. The image
-- ownership operation it represents happens in order with other operations
-- on the queue, so no explicit semaphore object is required to synchronize
-- its actions.
--
-- Applications may want to acquire the next image in separate threads from
-- those in which they manage their queue, or in multiple threads. To make
-- such usage easier, the acquire next image command takes a semaphore to
-- signal as a method of explicit synchronization. The application must
-- later queue a wait for this semaphore before queuing execution of any
-- commands using the image.
--
-- 7) Does 'acquireNextImageKHR' block if no images are available?
--
-- __RESOLVED__: The command takes a timeout parameter. Special values for
-- the timeout are 0, which makes the call a non-blocking operation, and
-- @UINT64_MAX@, which blocks indefinitely. Values in between will block
-- for up to the specified time. The call will return when an image becomes
-- available or an error occurs. It may, but is not required to, return
-- before the specified timeout expires if the swapchain becomes out of
-- date.
--
-- 8) Can multiple presents be queued using one 'queuePresentKHR' call?
--
-- __RESOLVED__: Yes. 'PresentInfoKHR' contains a list of swapchains and
-- corresponding image indices that will be presented. When supported, all
-- presentations queued with a single 'queuePresentKHR' call will be
-- applied atomically as one operation. The same swapchain must not appear
-- in the list more than once. Later extensions may provide applications
-- stronger guarantees of atomicity for such present operations, and\/or
-- allow them to query whether atomic presentation of a particular group of
-- swapchains is possible.
--
-- 9) How do the presentation and acquire next image functions notify the
-- application the targeted surface has changed?
--
-- __RESOLVED__: Two new result codes are introduced for this purpose:
--
-- -   'Vulkan.Core10.Enums.Result.SUBOPTIMAL_KHR' - Presentation will
--     still succeed, subject to the window resize behavior, but the
--     swapchain is no longer configured optimally for the surface it
--     targets. Applications should query updated surface information and
--     recreate their swapchain at the next convenient opportunity.
--
-- -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR' - Failure. The
--     swapchain is no longer compatible with the surface it targets. The
--     application must query updated surface information and recreate the
--     swapchain before presentation will succeed.
--
-- These can be returned by both 'acquireNextImageKHR' and
-- 'queuePresentKHR'.
--
-- 10) Does the 'acquireNextImageKHR' command return a semaphore to the
-- application via an output parameter, or accept a semaphore to signal
-- from the application as an object handle parameter?
--
-- __RESOLVED__: Accept a semaphore to signal as an object handle. This
-- avoids the need to specify whether the application must destroy the
-- semaphore or whether it is owned by the swapchain, and if the latter,
-- what its lifetime is and whether it can be reused for other operations
-- once it is received from 'acquireNextImageKHR'.
--
-- 11) What types of swapchain queuing behavior should be exposed? Options
-- include swap interval specification, mailbox\/most recent vs. FIFO queue
-- management, targeting specific vertical blank intervals or absolute
-- times for a given present operation, and probably others. For some of
-- these, whether they are specified at swapchain creation time or as
-- per-present parameters needs to be decided as well.
--
-- __RESOLVED__: The base swapchain extension will expose 3 possible
-- behaviors (of which, FIFO will always be supported):
--
-- -   Immediate present: Does not wait for vertical blanking period to
--     update the current image, likely resulting in visible tearing. No
--     internal queue is used. Present requests are applied immediately.
--
-- -   Mailbox queue: Waits for the next vertical blanking period to update
--     the current image. No tearing should be observed. An internal
--     single-entry queue is used to hold pending presentation requests. If
--     the queue is full when a new presentation request is received, the
--     new request replaces the existing entry, and any images associated
--     with the prior entry become available for reuse by the application.
--
-- -   FIFO queue: Waits for the next vertical blanking period to update
--     the current image. No tearing should be observed. An internal queue
--     containing @numSwapchainImages@ - 1 entries is used to hold pending
--     presentation requests. New requests are appended to the end of the
--     queue, and one request is removed from the beginning of the queue
--     and processed during each vertical blanking period in which the
--     queue is non-empty
--
-- Not all surfaces will support all of these modes, so the modes supported
-- will be returned using a surface information query. All surfaces must
-- support the FIFO queue mode. Applications must choose one of these modes
-- up front when creating a swapchain. Switching modes can be accomplished
-- by recreating the swapchain.
--
-- 12) Can 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR'
-- provide non-blocking guarantees for 'acquireNextImageKHR'? If so, what
-- is the proper criteria?
--
-- __RESOLVED__: Yes. The difficulty is not immediately obvious here.
-- Naively, if at least 3 images are requested, mailbox mode should always
-- have an image available for the application if the application does not
-- own any images when the call to 'acquireNextImageKHR' was made. However,
-- some presentation engines may have more than one “current” image, and
-- would still need to block in some cases. The right requirement appears
-- to be that if the application allocates the surface’s minimum number of
-- images + 1 then it is guaranteed non-blocking behavior when it does not
-- currently own any images.
--
-- 13) Is there a way to create and initialize a new swapchain for a
-- surface that has generated a 'Vulkan.Core10.Enums.Result.SUBOPTIMAL_KHR'
-- return code while still using the old swapchain?
--
-- __RESOLVED__: Not as part of this specification. This could be useful to
-- allow the application to create an “optimal” replacement swapchain and
-- rebuild all its command buffers using it in a background thread at a low
-- priority while continuing to use the “suboptimal” swapchain in the main
-- thread. It could probably use the same “atomic replace” semantics
-- proposed for recreating direct-to-device swapchains without incurring a
-- mode switch. However, after discussion, it was determined some platforms
-- probably could not support concurrent swapchains for the same surface
-- though, so this will be left out of the base KHR extensions. A future
-- extension could add this for platforms where it is supported.
--
-- 14) Should there be a special value for
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@maxImageCount@
-- to indicate there are no practical limits on the number of images in a
-- swapchain?
--
-- __RESOLVED__: Yes. There will often be cases where there is no practical
-- limit to the number of images in a swapchain other than the amount of
-- available resources (i.e., memory) in the system. Trying to derive a
-- hard limit from things like memory size is prone to failure. It is
-- better in such cases to leave it to applications to figure such soft
-- limits out via trial\/failure iterations.
--
-- 15) Should there be a special value for
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@currentExtent@
-- to indicate the size of the platform surface is undefined?
--
-- __RESOLVED__: Yes. On some platforms (Wayland, for example), the surface
-- size is defined by the images presented to it rather than the other way
-- around.
--
-- 16) Should there be a special value for
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@maxImageExtent@
-- to indicate there is no practical limit on the surface size?
--
-- __RESOLVED__: No. It seems unlikely such a system would exist. 0 could
-- be used to indicate the platform places no limits on the extents beyond
-- those imposed by Vulkan for normal images, but this query could just as
-- easily return those same limits, so a special “unlimited” value does not
-- seem useful for this field.
--
-- 17) How should surface rotation and mirroring be exposed to
-- applications? How do they specify rotation and mirroring transforms
-- applied prior to presentation?
--
-- __RESOLVED__: Applications can query both the supported and current
-- transforms of a surface. Both are specified relative to the device’s
-- “natural” display rotation and direction. The supported transforms
-- indicate which orientations the presentation engine accepts images in.
-- For example, a presentation engine that does not support transforming
-- surfaces as part of presentation, and which is presenting to a surface
-- that is displayed with a 90-degree rotation, would return only one
-- supported transform bit:
-- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR'.
-- Applications must transform their rendering by the transform they
-- specify when creating the swapchain in @preTransform@ field.
--
-- 18) Can surfaces ever not support @VK_MIRROR_NONE@? Can they support
-- vertical and horizontal mirroring simultaneously? Relatedly, should
-- @VK_MIRROR_NONE@[_BIT] be zero, or bit one, and should applications be
-- allowed to specify multiple pre and current mirror transform bits, or
-- exactly one?
--
-- __RESOLVED__: Since some platforms may not support presenting with a
-- transform other than the native window’s current transform, and
-- prerotation\/mirroring are specified relative to the device’s natural
-- rotation and direction, rather than relative to the surface’s current
-- rotation and direction, it is necessary to express lack of support for
-- no mirroring. To allow this, the @MIRROR_NONE@ enum must occupy a bit in
-- the flags. Since @MIRROR_NONE@ must be a bit in the bitmask rather than
-- a bitmask with no values set, allowing more than one bit to be set in
-- the bitmask would make it possible to describe undefined transforms such
-- as @VK_MIRROR_NONE_BIT@ | @VK_MIRROR_HORIZONTAL_BIT@, or a transform
-- that includes both “no mirroring” and “horizontal mirroring”
-- simultaneously. Therefore, it is desirable to allow specifying all
-- supported mirroring transforms using only one bit. The question then
-- becomes, should there be a @VK_MIRROR_HORIZONTAL_AND_VERTICAL_BIT@ to
-- represent a simultaneous horizontal and vertical mirror transform?
-- However, such a transform is equivalent to a 180 degree rotation, so
-- presentation engines and applications that wish to support or use such a
-- transform can express it through rotation instead. Therefore, 3
-- exclusive bits are sufficient to express all needed mirroring
-- transforms.
--
-- 19) Should support for sRGB be required?
--
-- __RESOLVED__: In the advent of UHD and HDR display devices, proper color
-- space information is vital to the display pipeline represented by the
-- swapchain. The app can discover the supported format\/color-space pairs
-- and select a pair most suited to its rendering needs. Currently only the
-- sRGB color space is supported, future extensions may provide support for
-- more color spaces. See issues 23 and 24.
--
-- 20) Is there a mechanism to modify or replace an existing swapchain with
-- one targeting the same surface?
--
-- __RESOLVED__: Yes. This is described above in the text.
--
-- 21) Should there be a way to set prerotation and mirroring using native
-- APIs when presenting using a Vulkan swapchain?
--
-- __RESOLVED__: Yes. The transforms that can be expressed in this
-- extension are a subset of those possible on native platforms. If a
-- platform exposes a method to specify the transform of presented images
-- for a given surface using native methods and exposes more transforms or
-- other properties for surfaces than Vulkan supports, it might be
-- impossible, difficult, or inconvenient to set some of those properties
-- using Vulkan KHR extensions and some using the native interfaces. To
-- avoid overwriting properties set using native commands when presenting
-- using a Vulkan swapchain, the application can set the pretransform to
-- “inherit”, in which case the current native properties will be used, or
-- if none are available, a platform-specific default will be used.
-- Platforms that do not specify a reasonable default or do not provide
-- native mechanisms to specify such transforms should not include the
-- inherit bits in the @supportedTransforms@ bitmask they return in
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'.
--
-- 22) Should the content of presentable images be clipped by objects
-- obscuring their target surface?
--
-- __RESOLVED__: Applications can choose which behavior they prefer.
-- Allowing the content to be clipped could enable more efficient
-- presentation methods on some platforms, but some applications might rely
-- on the content of presentable images to perform techniques such as
-- partial updates or motion blurs.
--
-- 23) What is the purpose of specifying a
-- 'Vulkan.Extensions.VK_KHR_surface.ColorSpaceKHR' along with
-- 'Vulkan.Core10.Enums.Format.Format' when creating a swapchain?
--
-- __RESOLVED__: While Vulkan itself is color space agnostic (e.g. even the
-- meaning of R, G, B and A can be freely defined by the rendering
-- application), the swapchain eventually will have to present the images
-- on a display device with specific color reproduction characteristics. If
-- any color space transformations are necessary before an image can be
-- displayed, the color space of the presented image must be known to the
-- swapchain. A swapchain will only support a restricted set of color
-- format and -space pairs. This set can be discovered via
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceFormatsKHR'.
-- As it can be expected that most display devices support the sRGB color
-- space, at least one format\/color-space pair has to be exposed, where
-- the color space is
-- 'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_SRGB_NONLINEAR_KHR'.
--
-- 24) How are sRGB formats and the sRGB color space related?
--
-- __RESOLVED__: While Vulkan exposes a number of SRGB texture formats,
-- using such formats does not guarantee working in a specific color space.
-- It merely means that the hardware can directly support applying the
-- non-linear transfer functions defined by the sRGB standard color space
-- when reading from or writing to images of those formats. Still, it is
-- unlikely that a swapchain will expose a @*_SRGB@ format along with any
-- color space other than
-- 'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_SRGB_NONLINEAR_KHR'.
--
-- On the other hand, non-@*_SRGB@ formats will be very likely exposed in
-- pair with a SRGB color space. This means, the hardware will not apply
-- any transfer function when reading from or writing to such images, yet
-- they will still be presented on a device with sRGB display
-- characteristics. In this case the application is responsible for
-- applying the transfer function, for instance by using shader math.
--
-- 25) How are the lifetimes of surfaces and swapchains targeting them
-- related?
--
-- __RESOLVED__: A surface must outlive any swapchains targeting it. A
-- 'Vulkan.Extensions.Handles.SurfaceKHR' owns the binding of the native
-- window to the Vulkan driver.
--
-- 26) How can the client control the way the alpha component of swapchain
-- images is treated by the presentation engine during compositing?
--
-- __RESOLVED__: We should add new enum values to allow the client to
-- negotiate with the presentation engine on how to treat image alpha
-- values during the compositing process. Since not all platforms can
-- practically control this through the Vulkan driver, a value of
-- 'Vulkan.Extensions.VK_KHR_surface.COMPOSITE_ALPHA_INHERIT_BIT_KHR' is
-- provided like for surface transforms.
--
-- 27) Is 'createSwapchainKHR' the right function to return
-- 'Vulkan.Core10.Enums.Result.ERROR_NATIVE_WINDOW_IN_USE_KHR', or should
-- the various platform-specific 'Vulkan.Extensions.Handles.SurfaceKHR'
-- factory functions catch this error earlier?
--
-- __RESOLVED__: For most platforms, the
-- 'Vulkan.Extensions.Handles.SurfaceKHR' structure is a simple container
-- holding the data that identifies a native window or other object
-- representing a surface on a particular platform. For the surface factory
-- functions to return this error, they would likely need to register a
-- reference on the native objects with the native display server somehow,
-- and ensure no other such references exist. Surfaces were not intended to
-- be that heavyweight.
--
-- Swapchains are intended to be the objects that directly manipulate
-- native windows and communicate with the native presentation mechanisms.
-- Swapchains will already need to communicate with the native display
-- server to negotiate allocation and\/or presentation of presentable
-- images for a native surface. Therefore, it makes more sense for
-- swapchain creation to be the point at which native object exclusivity is
-- enforced. Platforms may choose to enforce further restrictions on the
-- number of 'Vulkan.Extensions.Handles.SurfaceKHR' objects that may be
-- created for the same native window if such a requirement makes sense on
-- a particular platform, but a global requirement is only sensible at the
-- swapchain level.
--
-- == Examples
--
-- Note
--
-- The example code for the @VK_KHR_surface@ and @VK_KHR_swapchain@
-- extensions was removed from the appendix after revision 1.0.29. This WSI
-- example code was ported to the cube demo that is shipped with the
-- official Khronos SDK, and is being kept up-to-date in that location
-- (see:
-- <https://github.com/KhronosGroup/Vulkan-Tools/blob/master/cube/cube.c>).
--
-- == Version History
--
-- -   Revision 1, 2015-05-20 (James Jones)
--
--     -   Initial draft, based on LunarG KHR spec, other KHR specs,
--         patches attached to bugs.
--
-- -   Revision 2, 2015-05-22 (Ian Elliott)
--
--     -   Made many agreed-upon changes from 2015-05-21 KHR TSG meeting.
--         This includes using only a queue for presentation, and having an
--         explicit function to acquire the next image.
--
--     -   Fixed typos and other minor mistakes.
--
-- -   Revision 3, 2015-05-26 (Ian Elliott)
--
--     -   Improved the Description section.
--
--     -   Added or resolved issues that were found in improving the
--         Description. For example, pSurfaceDescription is used
--         consistently, instead of sometimes using pSurface.
--
-- -   Revision 4, 2015-05-27 (James Jones)
--
--     -   Fixed some grammatical errors and typos
--
--     -   Filled in the description of imageUseFlags when creating a
--         swapchain.
--
--     -   Added a description of swapInterval.
--
--     -   Replaced the paragraph describing the order of operations on a
--         queue for image ownership and presentation.
--
-- -   Revision 5, 2015-05-27 (James Jones)
--
--     -   Imported relevant issues from the (abandoned)
--         vk_wsi_persistent_swapchain_images extension.
--
--     -   Added issues 6 and 7, regarding behavior of the acquire next
--         image and present commands with respect to queues.
--
--     -   Updated spec language and examples to align with proposed
--         resolutions to issues 6 and 7.
--
-- -   Revision 6, 2015-05-27 (James Jones)
--
--     -   Added issue 8, regarding atomic presentation of multiple
--         swapchains
--
--     -   Updated spec language and examples to align with proposed
--         resolution to issue 8.
--
-- -   Revision 7, 2015-05-27 (James Jones)
--
--     -   Fixed compilation errors in example code, and made related spec
--         fixes.
--
-- -   Revision 8, 2015-05-27 (James Jones)
--
--     -   Added issue 9, and the related VK_SUBOPTIMAL_KHR result code.
--
--     -   Renamed VK_OUT_OF_DATE_KHR to VK_ERROR_OUT_OF_DATE_KHR.
--
-- -   Revision 9, 2015-05-27 (James Jones)
--
--     -   Added inline proposed resolutions (marked with [JRJ]) to some
--         XXX questions\/issues. These should be moved to the issues
--         section in a subsequent update if the proposals are adopted.
--
-- -   Revision 10, 2015-05-28 (James Jones)
--
--     -   Converted vkAcquireNextImageKHR back to a non-queue operation
--         that uses a VkSemaphore object for explicit synchronization.
--
--     -   Added issue 10 to determine whether vkAcquireNextImageKHR
--         generates or returns semaphores, or whether it operates on a
--         semaphore provided by the application.
--
-- -   Revision 11, 2015-05-28 (James Jones)
--
--     -   Marked issues 6, 7, and 8 resolved.
--
--     -   Renamed VkSurfaceCapabilityPropertiesKHR to
--         VkSurfacePropertiesKHR to better convey the mutable nature of
--         the information it contains.
--
-- -   Revision 12, 2015-05-28 (James Jones)
--
--     -   Added issue 11 with a proposed resolution, and the related issue
--         12.
--
--     -   Updated various sections of the spec to match the proposed
--         resolution to issue 11.
--
-- -   Revision 13, 2015-06-01 (James Jones)
--
--     -   Moved some structures to VK_EXT_KHR_swap_chain to resolve the
--         specification’s issues 1 and 2.
--
-- -   Revision 14, 2015-06-01 (James Jones)
--
--     -   Added code for example 4 demonstrating how an application might
--         make use of the two different present and acquire next image KHR
--         result codes.
--
--     -   Added issue 13.
--
-- -   Revision 15, 2015-06-01 (James Jones)
--
--     -   Added issues 14 - 16 and related spec language.
--
--     -   Fixed some spelling errors.
--
--     -   Added language describing the meaningful return values for
--         vkAcquireNextImageKHR and vkQueuePresentKHR.
--
-- -   Revision 16, 2015-06-02 (James Jones)
--
--     -   Added issues 17 and 18, as well as related spec language.
--
--     -   Removed some erroneous text added by mistake in the last update.
--
-- -   Revision 17, 2015-06-15 (Ian Elliott)
--
--     -   Changed special value from \"-1\" to \"0\" so that the data
--         types can be unsigned.
--
-- -   Revision 18, 2015-06-15 (Ian Elliott)
--
--     -   Clarified the values of VkSurfacePropertiesKHR::minImageCount
--         and the timeout parameter of the vkAcquireNextImageKHR function.
--
-- -   Revision 19, 2015-06-17 (James Jones)
--
--     -   Misc. cleanup. Removed resolved inline issues and fixed typos.
--
--     -   Fixed clarification of VkSurfacePropertiesKHR::minImageCount
--         made in version 18.
--
--     -   Added a brief \"Image Ownership\" definition to the list of
--         terms used in the spec.
--
-- -   Revision 20, 2015-06-17 (James Jones)
--
--     -   Updated enum-extending values using new convention.
--
-- -   Revision 21, 2015-06-17 (James Jones)
--
--     -   Added language describing how to use
--         VK_IMAGE_LAYOUT_PRESENT_SOURCE_KHR.
--
--     -   Cleaned up an XXX comment regarding the description of which
--         queues vkQueuePresentKHR can be used on.
--
-- -   Revision 22, 2015-06-17 (James Jones)
--
--     -   Rebased on Vulkan API version 126.
--
-- -   Revision 23, 2015-06-18 (James Jones)
--
--     -   Updated language for issue 12 to read as a proposed resolution.
--
--     -   Marked issues 11, 12, 13, 16, and 17 resolved.
--
--     -   Temporarily added links to the relevant bugs under the remaining
--         unresolved issues.
--
--     -   Added issues 19 and 20 as well as proposed resolutions.
--
-- -   Revision 24, 2015-06-19 (Ian Elliott)
--
--     -   Changed special value for VkSurfacePropertiesKHR::currentExtent
--         back to “-1” from “0”. This value will never need to be
--         unsigned, and “0” is actually a legal value.
--
-- -   Revision 25, 2015-06-23 (Ian Elliott)
--
--     -   Examples now show use of function pointers for extension
--         functions.
--
--     -   Eliminated extraneous whitespace.
--
-- -   Revision 26, 2015-06-25 (Ian Elliott)
--
--     -   Resolved Issues 9 & 10 per KHR TSG meeting.
--
-- -   Revision 27, 2015-06-25 (James Jones)
--
--     -   Added oldSwapchain member to VkSwapchainCreateInfoKHR.
--
-- -   Revision 28, 2015-06-25 (James Jones)
--
--     -   Added the “inherit” bits to the rotation and mirroring flags and
--         the associated issue 21.
--
-- -   Revision 29, 2015-06-25 (James Jones)
--
--     -   Added the “clipped” flag to VkSwapchainCreateInfoKHR, and the
--         associated issue 22.
--
--     -   Specified that presenting an image does not modify it.
--
-- -   Revision 30, 2015-06-25 (James Jones)
--
--     -   Added language to the spec that clarifies the behavior of
--         vkCreateSwapchainKHR() when the oldSwapchain field of
--         VkSwapchainCreateInfoKHR is not NULL.
--
-- -   Revision 31, 2015-06-26 (Ian Elliott)
--
--     -   Example of new VkSwapchainCreateInfoKHR members, “oldSwapchain”
--         and “clipped”.
--
--     -   Example of using VkSurfacePropertiesKHR::{min|max}ImageCount to
--         set VkSwapchainCreateInfoKHR::minImageCount.
--
--     -   Rename vkGetSurfaceInfoKHR()\'s 4th parameter to “pDataSize”,
--         for consistency with other functions.
--
--     -   Add macro with C-string name of extension (just to header file).
--
-- -   Revision 32, 2015-06-26 (James Jones)
--
--     -   Minor adjustments to the language describing the behavior of
--         “oldSwapchain”
--
--     -   Fixed the version date on my previous two updates.
--
-- -   Revision 33, 2015-06-26 (Jesse Hall)
--
--     -   Add usage flags to VkSwapchainCreateInfoKHR
--
-- -   Revision 34, 2015-06-26 (Ian Elliott)
--
--     -   Rename vkQueuePresentKHR()\'s 2nd parameter to “pPresentInfo”,
--         for consistency with other functions.
--
-- -   Revision 35, 2015-06-26 (Jason Ekstrand)
--
--     -   Merged the VkRotationFlagBitsKHR and VkMirrorFlagBitsKHR enums
--         into a single VkSurfaceTransformFlagBitsKHR enum.
--
-- -   Revision 36, 2015-06-26 (Jason Ekstrand)
--
--     -   Added a VkSurfaceTransformKHR enum that is not a bitmask. Each
--         value in VkSurfaceTransformKHR corresponds directly to one of
--         the bits in VkSurfaceTransformFlagBitsKHR so transforming from
--         one to the other is easy. Having a separate enum means that
--         currentTransform and preTransform are now unambiguous by
--         definition.
--
-- -   Revision 37, 2015-06-29 (Ian Elliott)
--
--     -   Corrected one of the signatures of vkAcquireNextImageKHR, which
--         had the last two parameters switched from what it is elsewhere
--         in the specification and header files.
--
-- -   Revision 38, 2015-06-30 (Ian Elliott)
--
--     -   Corrected a typo in description of the vkGetSwapchainInfoKHR()
--         function.
--
--     -   Corrected a typo in header file comment for
--         VkPresentInfoKHR::sType.
--
-- -   Revision 39, 2015-07-07 (Daniel Rakos)
--
--     -   Added error section describing when each error is expected to be
--         reported.
--
--     -   Replaced bool32_t with VkBool32.
--
-- -   Revision 40, 2015-07-10 (Ian Elliott)
--
--     -   Updated to work with version 138 of the @vulkan.h@ header. This
--         includes declaring the VkSwapchainKHR type using the new
--         VK_DEFINE_NONDISP_HANDLE macro, and no longer extending
--         VkObjectType (which was eliminated).
--
-- -   Revision 41 2015-07-09 (Mathias Heyer)
--
--     -   Added color space language.
--
-- -   Revision 42, 2015-07-10 (Daniel Rakos)
--
--     -   Updated query mechanism to reflect the convention changes done
--         in the core spec.
--
--     -   Removed “queue” from the name of
--         VK_STRUCTURE_TYPE_QUEUE_PRESENT_INFO_KHR to be consistent with
--         the established naming convention.
--
--     -   Removed reference to the no longer existing VkObjectType enum.
--
-- -   Revision 43, 2015-07-17 (Daniel Rakos)
--
--     -   Added support for concurrent sharing of swapchain images across
--         queue families.
--
--     -   Updated sample code based on recent changes
--
-- -   Revision 44, 2015-07-27 (Ian Elliott)
--
--     -   Noted that support for VK_PRESENT_MODE_FIFO_KHR is required.
--         That is ICDs may optionally support IMMEDIATE and MAILBOX, but
--         must support FIFO.
--
-- -   Revision 45, 2015-08-07 (Ian Elliott)
--
--     -   Corrected a typo in spec file (type and variable name had wrong
--         case for the imageColorSpace member of the
--         VkSwapchainCreateInfoKHR struct).
--
--     -   Corrected a typo in header file (last parameter in
--         PFN_vkGetSurfacePropertiesKHR was missing “KHR” at the end of
--         type: VkSurfacePropertiesKHR).
--
-- -   Revision 46, 2015-08-20 (Ian Elliott)
--
--     -   Renamed this extension and all of its enumerations, types,
--         functions, etc. This makes it compliant with the proposed
--         standard for Vulkan extensions.
--
--     -   Switched from “revision” to “version”, including use of the
--         VK_MAKE_VERSION macro in the header file.
--
--     -   Made improvements to several descriptions.
--
--     -   Changed the status of several issues from PROPOSED to RESOLVED,
--         leaving no unresolved issues.
--
--     -   Resolved several TODOs, did miscellaneous cleanup, etc.
--
-- -   Revision 47, 2015-08-20 (Ian Elliott—​porting a 2015-07-29 change
--     from James Jones)
--
--     -   Moved the surface transform enums to VK_WSI_swapchain so they
--         could be reused by VK_WSI_display.
--
-- -   Revision 48, 2015-09-01 (James Jones)
--
--     -   Various minor cleanups.
--
-- -   Revision 49, 2015-09-01 (James Jones)
--
--     -   Restore single-field revision number.
--
-- -   Revision 50, 2015-09-01 (James Jones)
--
--     -   Update Example #4 to include code that illustrates how to use
--         the oldSwapchain field.
--
-- -   Revision 51, 2015-09-01 (James Jones)
--
--     -   Fix example code compilation errors.
--
-- -   Revision 52, 2015-09-08 (Matthaeus G. Chajdas)
--
--     -   Corrected a typo.
--
-- -   Revision 53, 2015-09-10 (Alon Or-bach)
--
--     -   Removed underscore from SWAP_CHAIN left in
--         VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR.
--
-- -   Revision 54, 2015-09-11 (Jesse Hall)
--
--     -   Described the execution and memory coherence requirements for
--         image transitions to and from
--         VK_IMAGE_LAYOUT_PRESENT_SOURCE_KHR.
--
-- -   Revision 55, 2015-09-11 (Ray Smith)
--
--     -   Added errors for destroying and binding memory to presentable
--         images
--
-- -   Revision 56, 2015-09-18 (James Jones)
--
--     -   Added fence argument to vkAcquireNextImageKHR
--
--     -   Added example of how to meter a host thread based on
--         presentation rate.
--
-- -   Revision 57, 2015-09-26 (Jesse Hall)
--
--     -   Replace VkSurfaceDescriptionKHR with VkSurfaceKHR.
--
--     -   Added issue 25 with agreed resolution.
--
-- -   Revision 58, 2015-09-28 (Jesse Hall)
--
--     -   Renamed from VK_EXT_KHR_device_swapchain to
--         VK_EXT_KHR_swapchain.
--
-- -   Revision 59, 2015-09-29 (Ian Elliott)
--
--     -   Changed vkDestroySwapchainKHR() to return void.
--
-- -   Revision 60, 2015-10-01 (Jeff Vigil)
--
--     -   Added error result VK_ERROR_SURFACE_LOST_KHR.
--
-- -   Revision 61, 2015-10-05 (Jason Ekstrand)
--
--     -   Added the VkCompositeAlpha enum and corresponding structure
--         fields.
--
-- -   Revision 62, 2015-10-12 (Daniel Rakos)
--
--     -   Added VK_PRESENT_MODE_FIFO_RELAXED_KHR.
--
-- -   Revision 63, 2015-10-15 (Daniel Rakos)
--
--     -   Moved surface capability queries to VK_EXT_KHR_surface.
--
-- -   Revision 64, 2015-10-26 (Ian Elliott)
--
--     -   Renamed from VK_EXT_KHR_swapchain to VK_KHR_swapchain.
--
-- -   Revision 65, 2015-10-28 (Ian Elliott)
--
--     -   Added optional pResult member to VkPresentInfoKHR, so that
--         per-swapchain results can be obtained from vkQueuePresentKHR().
--
-- -   Revision 66, 2015-11-03 (Daniel Rakos)
--
--     -   Added allocation callbacks to create and destroy functions.
--
--     -   Updated resource transition language.
--
--     -   Updated sample code.
--
-- -   Revision 67, 2015-11-10 (Jesse Hall)
--
--     -   Add reserved flags bitmask to VkSwapchainCreateInfoKHR.
--
--     -   Modify naming and member ordering to match API style
--         conventions, and so the VkSwapchainCreateInfoKHR image property
--         members mirror corresponding VkImageCreateInfo members but with
--         an \'image\' prefix.
--
--     -   Make VkPresentInfoKHR::pResults non-const; it is an output array
--         parameter.
--
--     -   Make pPresentInfo parameter to vkQueuePresentKHR const.
--
-- -   Revision 68, 2016-04-05 (Ian Elliott)
--
--     -   Moved the “validity” include for vkAcquireNextImage to be in its
--         proper place, after the prototype and list of parameters.
--
--     -   Clarified language about presentable images, including how they
--         are acquired, when applications can and cannot use them, etc. As
--         part of this, removed language about “ownership” of presentable
--         images, and replaced it with more-consistent language about
--         presentable images being “acquired” by the application.
--
-- -   2016-08-23 (Ian Elliott)
--
--     -   Update the example code, to use the final API command names, to
--         not have so many characters per line, and to split out a new
--         example to show how to obtain function pointers. This code is
--         more similar to the LunarG “cube” demo program.
--
-- -   2016-08-25 (Ian Elliott)
--
--     -   A note was added at the beginning of the example code, stating
--         that it will be removed from future versions of the appendix.
--
-- -   Revision 69, 2017-09-07 (Tobias Hector)
--
--     -   Added interactions with Vulkan 1.1
--
-- -   Revision 70, 2017-10-06 (Ian Elliott)
--
--     -   Corrected interactions with Vulkan 1.1
--
-- == See Also
--
-- 'PresentInfoKHR', 'SwapchainCreateFlagBitsKHR',
-- 'SwapchainCreateFlagsKHR', 'SwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.Handles.SwapchainKHR', 'acquireNextImageKHR',
-- 'createSwapchainKHR', 'destroySwapchainKHR', 'getSwapchainImagesKHR',
-- 'queuePresentKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_swapchain Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_swapchain  ( createSwapchainKHR
                                           , withSwapchainKHR
                                           , destroySwapchainKHR
                                           , getSwapchainImagesKHR
                                           , acquireNextImageKHR
                                           , acquireNextImageKHRSafe
                                           , queuePresentKHR
                                           , getDeviceGroupPresentCapabilitiesKHR
                                           , getDeviceGroupSurfacePresentModesKHR
                                           , acquireNextImage2KHR
                                           , acquireNextImage2KHRSafe
                                           , getPhysicalDevicePresentRectanglesKHR
                                           , SwapchainCreateInfoKHR(..)
                                           , PresentInfoKHR(..)
                                           , DeviceGroupPresentCapabilitiesKHR(..)
                                           , ImageSwapchainCreateInfoKHR(..)
                                           , BindImageMemorySwapchainInfoKHR(..)
                                           , AcquireNextImageInfoKHR(..)
                                           , DeviceGroupPresentInfoKHR(..)
                                           , DeviceGroupSwapchainCreateInfoKHR(..)
                                           , DeviceGroupPresentModeFlagsKHR
                                           , DeviceGroupPresentModeFlagBitsKHR( DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR
                                                                              , DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR
                                                                              , DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR
                                                                              , DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR
                                                                              , ..
                                                                              )
                                           , SwapchainCreateFlagsKHR
                                           , SwapchainCreateFlagBitsKHR( SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR
                                                                       , SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
                                                                       , SWAPCHAIN_CREATE_PROTECTED_BIT_KHR
                                                                       , ..
                                                                       )
                                           , KHR_SWAPCHAIN_SPEC_VERSION
                                           , pattern KHR_SWAPCHAIN_SPEC_VERSION
                                           , KHR_SWAPCHAIN_EXTENSION_NAME
                                           , pattern KHR_SWAPCHAIN_EXTENSION_NAME
                                           , SurfaceKHR(..)
                                           , SwapchainKHR(..)
                                           , PresentModeKHR(..)
                                           , ColorSpaceKHR(..)
                                           , CompositeAlphaFlagBitsKHR(..)
                                           , CompositeAlphaFlagsKHR
                                           , SurfaceTransformFlagBitsKHR(..)
                                           , SurfaceTransformFlagsKHR
                                           ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Extensions.VK_KHR_surface (ColorSpaceKHR)
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagBitsKHR)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkAcquireNextImage2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkAcquireNextImageKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCreateSwapchainKHR))
import Vulkan.Dynamic (DeviceCmds(pVkDestroySwapchainKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceGroupPresentCapabilitiesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceGroupSurfacePresentModesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetSwapchainImagesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkQueuePresentKHR))
import Vulkan.Core10.Handles (Device_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display_swapchain (DisplayPresentInfoKHR)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_image_format_list (ImageFormatListCreateInfo)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDevicePresentRectanglesKHR))
import Vulkan.Core10.APIConstants (MAX_DEVICE_GROUP_SIZE)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_GGP_frame_token (PresentFrameTokenGGP)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_present_id (PresentIdKHR)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_incremental_present (PresentRegionsKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_GOOGLE_display_timing (PresentTimesInfoGOOGLE)
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue(Queue))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.Core10.Handles (Semaphore(..))
import Vulkan.Core10.Enums.SharingMode (SharingMode)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceFullScreenExclusiveInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceFullScreenExclusiveWin32InfoEXT)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_control (SwapchainCounterCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_display_native_hdr (SwapchainDisplayNativeHdrCreateInfoAMD)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.APIConstants (pattern MAX_DEVICE_GROUP_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_surface (ColorSpaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagsKHR)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSwapchainKHR
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct SwapchainCreateInfoKHR) -> Ptr AllocationCallbacks -> Ptr SwapchainKHR -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct SwapchainCreateInfoKHR) -> Ptr AllocationCallbacks -> Ptr SwapchainKHR -> IO Result

-- | vkCreateSwapchainKHR - Create a swapchain
--
-- = Description
--
-- If the @oldSwapchain@ parameter of @pCreateInfo@ is a valid swapchain,
-- which has exclusive full-screen access, that access is released from
-- @oldSwapchain@. If the command succeeds in this case, the newly created
-- swapchain will automatically acquire exclusive full-screen access from
-- @oldSwapchain@.
--
-- Note
--
-- This implicit transfer is intended to avoid exiting and entering
-- full-screen exclusive mode, which may otherwise cause unwanted visual
-- updates to the display.
--
-- In some cases, swapchain creation /may/ fail if exclusive full-screen
-- mode is requested for application control, but for some
-- implementation-specific reason exclusive full-screen access is
-- unavailable for the particular combination of parameters provided. If
-- this occurs, 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
-- will be returned.
--
-- Note
--
-- In particular, it will fail if the @imageExtent@ member of @pCreateInfo@
-- does not match the extents of the monitor. Other reasons for failure may
-- include the app not being set as high-dpi aware, or if the physical
-- device and monitor are not compatible in this mode.
--
-- When the 'Vulkan.Extensions.Handles.SurfaceKHR' in
-- 'SwapchainCreateInfoKHR' is a display surface, then the
-- 'Vulkan.Extensions.Handles.DisplayModeKHR' in display surface’s
-- 'Vulkan.Extensions.VK_KHR_display.DisplaySurfaceCreateInfoKHR' is
-- associated with a particular 'Vulkan.Extensions.Handles.DisplayKHR'.
-- Swapchain creation /may/ fail if that
-- 'Vulkan.Extensions.Handles.DisplayKHR' is not acquired by the
-- application. In this scenario
-- 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED' is returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateSwapchainKHR-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateSwapchainKHR-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'SwapchainCreateInfoKHR'
--     structure
--
-- -   #VUID-vkCreateSwapchainKHR-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateSwapchainKHR-pSwapchain-parameter# @pSwapchain@ /must/
--     be a valid pointer to a 'Vulkan.Extensions.Handles.SwapchainKHR'
--     handle
--
-- == Host Synchronization
--
-- -   Host access to @pCreateInfo->surface@ /must/ be externally
--     synchronized
--
-- -   Host access to @pCreateInfo->oldSwapchain@ /must/ be externally
--     synchronized
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_NATIVE_WINDOW_IN_USE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'SwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
createSwapchainKHR :: forall a io
                    . (Extendss SwapchainCreateInfoKHR a, PokeChain a, MonadIO io)
                   => -- | @device@ is the device to create the swapchain for.
                      Device
                   -> -- | @pCreateInfo@ is a pointer to a 'SwapchainCreateInfoKHR' structure
                      -- specifying the parameters of the created swapchain.
                      (SwapchainCreateInfoKHR a)
                   -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                      -- swapchain object when there is no more specific allocator available (see
                      -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                      ("allocator" ::: Maybe AllocationCallbacks)
                   -> io (SwapchainKHR)
createSwapchainKHR device createInfo allocator = liftIO . evalContT $ do
  let vkCreateSwapchainKHRPtr = pVkCreateSwapchainKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateSwapchainKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateSwapchainKHR is null" Nothing Nothing
  let vkCreateSwapchainKHR' = mkVkCreateSwapchainKHR vkCreateSwapchainKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSwapchain <- ContT $ bracket (callocBytes @SwapchainKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateSwapchainKHR" (vkCreateSwapchainKHR' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPSwapchain))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSwapchain <- lift $ peek @SwapchainKHR pPSwapchain
  pure $ (pSwapchain)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSwapchainKHR' and 'destroySwapchainKHR'
--
-- To ensure that 'destroySwapchainKHR' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSwapchainKHR :: forall a io r . (Extendss SwapchainCreateInfoKHR a, PokeChain a, MonadIO io) => Device -> SwapchainCreateInfoKHR a -> Maybe AllocationCallbacks -> (io SwapchainKHR -> (SwapchainKHR -> io ()) -> r) -> r
withSwapchainKHR device pCreateInfo pAllocator b =
  b (createSwapchainKHR device pCreateInfo pAllocator)
    (\(o0) -> destroySwapchainKHR device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySwapchainKHR
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> SwapchainKHR -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroySwapchainKHR - Destroy a swapchain object
--
-- = Description
--
-- The application /must/ not destroy a swapchain until after completion of
-- all outstanding operations on images that were acquired from the
-- swapchain. @swapchain@ and all associated 'Vulkan.Core10.Handles.Image'
-- handles are destroyed, and /must/ not be acquired or used any more by
-- the application. The memory of each 'Vulkan.Core10.Handles.Image' will
-- only be freed after that image is no longer used by the presentation
-- engine. For example, if one image of the swapchain is being displayed in
-- a window, the memory for that image /may/ not be freed until the window
-- is destroyed, or another swapchain is created for the window. Destroying
-- the swapchain does not invalidate the parent
-- 'Vulkan.Extensions.Handles.SurfaceKHR', and a new swapchain /can/ be
-- created with it.
--
-- When a swapchain associated with a display surface is destroyed, if the
-- image most recently presented to the display surface is from the
-- swapchain being destroyed, then either any display resources modified by
-- presenting images from any swapchain associated with the display surface
-- /must/ be reverted by the implementation to their state prior to the
-- first present performed on one of these swapchains, or such resources
-- /must/ be left in their current state.
--
-- If @swapchain@ has exclusive full-screen access, it is released before
-- the swapchain is destroyed.
--
-- == Valid Usage
--
-- -   #VUID-vkDestroySwapchainKHR-swapchain-01282# All uses of presentable
--     images acquired from @swapchain@ /must/ have completed execution
--
-- -   #VUID-vkDestroySwapchainKHR-swapchain-01283# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @swapchain@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroySwapchainKHR-swapchain-01284# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @swapchain@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroySwapchainKHR-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroySwapchainKHR-swapchain-parameter# If @swapchain@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @swapchain@ /must/ be
--     a valid 'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-vkDestroySwapchainKHR-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroySwapchainKHR-commonparent# Both of @device@, and
--     @swapchain@ that are valid handles of non-ignored parameters /must/
--     have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.SwapchainKHR'
destroySwapchainKHR :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the 'Vulkan.Core10.Handles.Device' associated with
                       -- @swapchain@.
                       Device
                    -> -- | @swapchain@ is the swapchain to destroy.
                       SwapchainKHR
                    -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                       -- swapchain object when there is no more specific allocator available (see
                       -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                       ("allocator" ::: Maybe AllocationCallbacks)
                    -> io ()
destroySwapchainKHR device swapchain allocator = liftIO . evalContT $ do
  let vkDestroySwapchainKHRPtr = pVkDestroySwapchainKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroySwapchainKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroySwapchainKHR is null" Nothing Nothing
  let vkDestroySwapchainKHR' = mkVkDestroySwapchainKHR vkDestroySwapchainKHRPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroySwapchainKHR" (vkDestroySwapchainKHR' (deviceHandle (device)) (swapchain) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSwapchainImagesKHR
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr Word32 -> Ptr Image -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Ptr Word32 -> Ptr Image -> IO Result

-- | vkGetSwapchainImagesKHR - Obtain the array of presentable images
-- associated with a swapchain
--
-- = Description
--
-- If @pSwapchainImages@ is @NULL@, then the number of presentable images
-- for @swapchain@ is returned in @pSwapchainImageCount@. Otherwise,
-- @pSwapchainImageCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pSwapchainImages@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pSwapchainImages@. If the value of @pSwapchainImageCount@ is less
-- than the number of presentable images for @swapchain@, at most
-- @pSwapchainImageCount@ structures will be written, and
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available presentable images were returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetSwapchainImagesKHR-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetSwapchainImagesKHR-swapchain-parameter# @swapchain@
--     /must/ be a valid 'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-vkGetSwapchainImagesKHR-pSwapchainImageCount-parameter#
--     @pSwapchainImageCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   #VUID-vkGetSwapchainImagesKHR-pSwapchainImages-parameter# If the
--     value referenced by @pSwapchainImageCount@ is not @0@, and
--     @pSwapchainImages@ is not @NULL@, @pSwapchainImages@ /must/ be a
--     valid pointer to an array of @pSwapchainImageCount@
--     'Vulkan.Core10.Handles.Image' handles
--
-- -   #VUID-vkGetSwapchainImagesKHR-commonparent# Both of @device@, and
--     @swapchain@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Instance'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
getSwapchainImagesKHR :: forall io
                       . (MonadIO io)
                      => -- | @device@ is the device associated with @swapchain@.
                         Device
                      -> -- | @swapchain@ is the swapchain to query.
                         SwapchainKHR
                      -> io (Result, ("swapchainImages" ::: Vector Image))
getSwapchainImagesKHR device swapchain = liftIO . evalContT $ do
  let vkGetSwapchainImagesKHRPtr = pVkGetSwapchainImagesKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetSwapchainImagesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSwapchainImagesKHR is null" Nothing Nothing
  let vkGetSwapchainImagesKHR' = mkVkGetSwapchainImagesKHR vkGetSwapchainImagesKHRPtr
  let device' = deviceHandle (device)
  pPSwapchainImageCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetSwapchainImagesKHR" (vkGetSwapchainImagesKHR' device' (swapchain) (pPSwapchainImageCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSwapchainImageCount <- lift $ peek @Word32 pPSwapchainImageCount
  pPSwapchainImages <- ContT $ bracket (callocBytes @Image ((fromIntegral (pSwapchainImageCount)) * 8)) free
  r' <- lift $ traceAroundEvent "vkGetSwapchainImagesKHR" (vkGetSwapchainImagesKHR' device' (swapchain) (pPSwapchainImageCount) (pPSwapchainImages))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pSwapchainImageCount' <- lift $ peek @Word32 pPSwapchainImageCount
  pSwapchainImages' <- lift $ generateM (fromIntegral (pSwapchainImageCount')) (\i -> peek @Image ((pPSwapchainImages `advancePtrBytes` (8 * (i)) :: Ptr Image)))
  pure $ ((r'), pSwapchainImages')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireNextImageKHRUnsafe
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result

foreign import ccall
  "dynamic" mkVkAcquireNextImageKHRSafe
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result

-- | acquireNextImageKHR with selectable safeness
acquireNextImageKHRSafeOrUnsafe :: forall io
                                 . (MonadIO io)
                                => (FunPtr (Ptr Device_T -> SwapchainKHR -> Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Word64 -> Semaphore -> Fence -> Ptr Word32 -> IO Result)
                                -> -- | @device@ is the device associated with @swapchain@.
                                   Device
                                -> -- | @swapchain@ is the non-retired swapchain from which an image is being
                                   -- acquired.
                                   SwapchainKHR
                                -> -- | @timeout@ specifies how long the function waits, in nanoseconds, if no
                                   -- image is available.
                                   ("timeout" ::: Word64)
                                -> -- | @semaphore@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a semaphore
                                   -- to signal.
                                   Semaphore
                                -> -- | @fence@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a fence to
                                   -- signal.
                                   Fence
                                -> io (Result, ("imageIndex" ::: Word32))
acquireNextImageKHRSafeOrUnsafe mkVkAcquireNextImageKHR device swapchain timeout semaphore fence = liftIO . evalContT $ do
  let vkAcquireNextImageKHRPtr = pVkAcquireNextImageKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkAcquireNextImageKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireNextImageKHR is null" Nothing Nothing
  let vkAcquireNextImageKHR' = mkVkAcquireNextImageKHR vkAcquireNextImageKHRPtr
  pPImageIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkAcquireNextImageKHR" (vkAcquireNextImageKHR' (deviceHandle (device)) (swapchain) (timeout) (semaphore) (fence) (pPImageIndex))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImageIndex <- lift $ peek @Word32 pPImageIndex
  pure $ (r, pImageIndex)

-- | vkAcquireNextImageKHR - Retrieve the index of the next available
-- presentable image
--
-- == Valid Usage
--
-- -   #VUID-vkAcquireNextImageKHR-swapchain-01285# @swapchain@ /must/ not
--     be in the retired state
--
-- -   #VUID-vkAcquireNextImageKHR-semaphore-01286# If @semaphore@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ be unsignaled
--
-- -   #VUID-vkAcquireNextImageKHR-semaphore-01779# If @semaphore@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ not have any
--     uncompleted signal or wait operations pending
--
-- -   #VUID-vkAcquireNextImageKHR-fence-01287# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ be unsignaled and
--     /must/ not be associated with any other queue command that has not
--     yet completed execution on that queue
--
-- -   #VUID-vkAcquireNextImageKHR-semaphore-01780# @semaphore@ and @fence@
--     /must/ not both be equal to 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkAcquireNextImageKHR-swapchain-01802# If the number of
--     currently acquired images is greater than the difference between the
--     number of images in @swapchain@ and the value of
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@minImageCount@
--     as returned by a call to
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'
--     with the @surface@ used to create @swapchain@, @timeout@ /must/ not
--     be @UINT64_MAX@
--
-- -   #VUID-vkAcquireNextImageKHR-semaphore-03265# @semaphore@ /must/ have
--     a 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkAcquireNextImageKHR-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkAcquireNextImageKHR-swapchain-parameter# @swapchain@ /must/
--     be a valid 'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-vkAcquireNextImageKHR-semaphore-parameter# If @semaphore@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @semaphore@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Semaphore' handle
--
-- -   #VUID-vkAcquireNextImageKHR-fence-parameter# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ be a valid
--     'Vulkan.Core10.Handles.Fence' handle
--
-- -   #VUID-vkAcquireNextImageKHR-pImageIndex-parameter# @pImageIndex@
--     /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkAcquireNextImageKHR-semaphore-parent# If @semaphore@ is a
--     valid handle, it /must/ have been created, allocated, or retrieved
--     from @device@
--
-- -   #VUID-vkAcquireNextImageKHR-fence-parent# If @fence@ is a valid
--     handle, it /must/ have been created, allocated, or retrieved from
--     @device@
--
-- -   #VUID-vkAcquireNextImageKHR-commonparent# Both of @device@, and
--     @swapchain@ that are valid handles of non-ignored parameters /must/
--     have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- -   Host access to @semaphore@ /must/ be externally synchronized
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.TIMEOUT'
--
--     -   'Vulkan.Core10.Enums.Result.NOT_READY'
--
--     -   'Vulkan.Core10.Enums.Result.SUBOPTIMAL_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Fence',
-- 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
acquireNextImageKHR :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the device associated with @swapchain@.
                       Device
                    -> -- | @swapchain@ is the non-retired swapchain from which an image is being
                       -- acquired.
                       SwapchainKHR
                    -> -- | @timeout@ specifies how long the function waits, in nanoseconds, if no
                       -- image is available.
                       ("timeout" ::: Word64)
                    -> -- | @semaphore@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a semaphore
                       -- to signal.
                       Semaphore
                    -> -- | @fence@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a fence to
                       -- signal.
                       Fence
                    -> io (Result, ("imageIndex" ::: Word32))
acquireNextImageKHR = acquireNextImageKHRSafeOrUnsafe mkVkAcquireNextImageKHRUnsafe

-- | A variant of 'acquireNextImageKHR' which makes a *safe* FFI call
acquireNextImageKHRSafe :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the device associated with @swapchain@.
                           Device
                        -> -- | @swapchain@ is the non-retired swapchain from which an image is being
                           -- acquired.
                           SwapchainKHR
                        -> -- | @timeout@ specifies how long the function waits, in nanoseconds, if no
                           -- image is available.
                           ("timeout" ::: Word64)
                        -> -- | @semaphore@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a semaphore
                           -- to signal.
                           Semaphore
                        -> -- | @fence@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a fence to
                           -- signal.
                           Fence
                        -> io (Result, ("imageIndex" ::: Word32))
acquireNextImageKHRSafe = acquireNextImageKHRSafeOrUnsafe mkVkAcquireNextImageKHRSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueuePresentKHR
  :: FunPtr (Ptr Queue_T -> Ptr (SomeStruct PresentInfoKHR) -> IO Result) -> Ptr Queue_T -> Ptr (SomeStruct PresentInfoKHR) -> IO Result

-- | vkQueuePresentKHR - Queue an image for presentation
--
-- = Description
--
-- Note
--
-- There is no requirement for an application to present images in the same
-- order that they were acquired - applications can arbitrarily present any
-- image that is currently acquired.
--
-- == Valid Usage
--
-- -   #VUID-vkQueuePresentKHR-pSwapchains-01292# Each element of
--     @pSwapchains@ member of @pPresentInfo@ /must/ be a swapchain that is
--     created for a surface for which presentation is supported from
--     @queue@ as determined using a call to
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR'
--
-- -   #VUID-vkQueuePresentKHR-pSwapchains-01293# If more than one member
--     of @pSwapchains@ was created from a display surface, all display
--     surfaces referenced that refer to the same display /must/ use the
--     same display mode
--
-- -   #VUID-vkQueuePresentKHR-pWaitSemaphores-01294# When a semaphore wait
--     operation referring to a binary semaphore defined by the elements of
--     the @pWaitSemaphores@ member of @pPresentInfo@ executes on @queue@,
--     there /must/ be no other queues waiting on the same semaphore
--
-- -   #VUID-vkQueuePresentKHR-pWaitSemaphores-01295# All elements of the
--     @pWaitSemaphores@ member of @pPresentInfo@ /must/ be semaphores that
--     are signaled, or have
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operations>
--     previously submitted for execution
--
-- -   #VUID-vkQueuePresentKHR-pWaitSemaphores-03267# All elements of the
--     @pWaitSemaphores@ member of @pPresentInfo@ /must/ be created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY'
--
-- -   #VUID-vkQueuePresentKHR-pWaitSemaphores-03268# All elements of the
--     @pWaitSemaphores@ member of @pPresentInfo@ /must/ reference a
--     semaphore signal operation that has been submitted for execution and
--     any semaphore signal operations on which it depends (if any) /must/
--     have also been submitted for execution
--
-- Any writes to memory backing the images referenced by the
-- @pImageIndices@ and @pSwapchains@ members of @pPresentInfo@, that are
-- available before 'queuePresentKHR' is executed, are automatically made
-- visible to the read access performed by the presentation engine. This
-- automatic visibility operation for an image happens-after the semaphore
-- signal operation, and happens-before the presentation engine accesses
-- the image.
--
-- Queueing an image for presentation defines a set of /queue operations/,
-- including waiting on the semaphores and submitting a presentation
-- request to the presentation engine. However, the scope of this set of
-- queue operations does not include the actual processing of the image by
-- the presentation engine.
--
-- Note
--
-- The origin of the native orientation of the surface coordinate system is
-- not specified in the Vulkan specification; it depends on the platform.
-- For most platforms the origin is by default upper-left, meaning the
-- pixel of the presented 'Vulkan.Core10.Handles.Image' at coordinates
-- (0,0) would appear at the upper left pixel of the platform surface
-- (assuming
-- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
-- and the display standing the right way up).
--
-- If 'queuePresentKHR' fails to enqueue the corresponding set of queue
-- operations, it /may/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY' or
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'. If it does, the
-- implementation /must/ ensure that the state and contents of any
-- resources or synchronization primitives referenced is unaffected by the
-- call or its failure.
--
-- If 'queuePresentKHR' fails in such a way that the implementation is
-- unable to make that guarantee, the implementation /must/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'.
--
-- However, if the presentation request is rejected by the presentation
-- engine with an error 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR',
-- 'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT',
-- or 'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR', the set of queue
-- operations are still considered to be enqueued and thus any semaphore
-- wait operation specified in 'PresentInfoKHR' will execute when the
-- corresponding queue operation is complete.
--
-- Calls to 'queuePresentKHR' /may/ block, but /must/ return in finite
-- time.
--
-- If any @swapchain@ member of @pPresentInfo@ was created with
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT',
-- 'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
-- will be returned if that swapchain does not have exclusive full-screen
-- access, possibly for implementation-specific reasons outside of the
-- application’s control.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkQueuePresentKHR-queue-parameter# @queue@ /must/ be a valid
--     'Vulkan.Core10.Handles.Queue' handle
--
-- -   #VUID-vkQueuePresentKHR-pPresentInfo-parameter# @pPresentInfo@
--     /must/ be a valid pointer to a valid 'PresentInfoKHR' structure
--
-- == Host Synchronization
--
-- -   Host access to @queue@ /must/ be externally synchronized
--
-- -   Host access to @pPresentInfo->pWaitSemaphores@[] /must/ be
--     externally synchronized
--
-- -   Host access to @pPresentInfo->pSwapchains@[] /must/ be externally
--     synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | Any                                                                                                                   |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.SUBOPTIMAL_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- 'PresentInfoKHR', 'Vulkan.Core10.Handles.Queue'
queuePresentKHR :: forall a io
                 . (Extendss PresentInfoKHR a, PokeChain a, MonadIO io)
                => -- | @queue@ is a queue that is capable of presentation to the target
                   -- surface’s platform on the same device as the image’s swapchain.
                   Queue
                -> -- | @pPresentInfo@ is a pointer to a 'PresentInfoKHR' structure specifying
                   -- parameters of the presentation.
                   (PresentInfoKHR a)
                -> io (Result)
queuePresentKHR queue presentInfo = liftIO . evalContT $ do
  let vkQueuePresentKHRPtr = pVkQueuePresentKHR (case queue of Queue{deviceCmds} -> deviceCmds)
  lift $ unless (vkQueuePresentKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueuePresentKHR is null" Nothing Nothing
  let vkQueuePresentKHR' = mkVkQueuePresentKHR vkQueuePresentKHRPtr
  pPresentInfo <- ContT $ withCStruct (presentInfo)
  r <- lift $ traceAroundEvent "vkQueuePresentKHR" (vkQueuePresentKHR' (queueHandle (queue)) (forgetExtensions pPresentInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupPresentCapabilitiesKHR
  :: FunPtr (Ptr Device_T -> Ptr DeviceGroupPresentCapabilitiesKHR -> IO Result) -> Ptr Device_T -> Ptr DeviceGroupPresentCapabilitiesKHR -> IO Result

-- | vkGetDeviceGroupPresentCapabilitiesKHR - Query present capabilities from
-- other physical devices
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceGroupPresentCapabilitiesKHR'
getDeviceGroupPresentCapabilitiesKHR :: forall io
                                      . (MonadIO io)
                                     => -- | @device@ is the logical device.
                                        --
                                        -- #VUID-vkGetDeviceGroupPresentCapabilitiesKHR-device-parameter# @device@
                                        -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                        Device
                                     -> io (DeviceGroupPresentCapabilitiesKHR)
getDeviceGroupPresentCapabilitiesKHR device = liftIO . evalContT $ do
  let vkGetDeviceGroupPresentCapabilitiesKHRPtr = pVkGetDeviceGroupPresentCapabilitiesKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceGroupPresentCapabilitiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceGroupPresentCapabilitiesKHR is null" Nothing Nothing
  let vkGetDeviceGroupPresentCapabilitiesKHR' = mkVkGetDeviceGroupPresentCapabilitiesKHR vkGetDeviceGroupPresentCapabilitiesKHRPtr
  pPDeviceGroupPresentCapabilities <- ContT (withZeroCStruct @DeviceGroupPresentCapabilitiesKHR)
  r <- lift $ traceAroundEvent "vkGetDeviceGroupPresentCapabilitiesKHR" (vkGetDeviceGroupPresentCapabilitiesKHR' (deviceHandle (device)) (pPDeviceGroupPresentCapabilities))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDeviceGroupPresentCapabilities <- lift $ peekCStruct @DeviceGroupPresentCapabilitiesKHR pPDeviceGroupPresentCapabilities
  pure $ (pDeviceGroupPresentCapabilities)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupSurfacePresentModesKHR
  :: FunPtr (Ptr Device_T -> SurfaceKHR -> Ptr DeviceGroupPresentModeFlagsKHR -> IO Result) -> Ptr Device_T -> SurfaceKHR -> Ptr DeviceGroupPresentModeFlagsKHR -> IO Result

-- | vkGetDeviceGroupSurfacePresentModesKHR - Query present capabilities for
-- a surface
--
-- = Description
--
-- The modes returned by this command are not invariant, and /may/ change
-- in response to the surface being moved, resized, or occluded. These
-- modes /must/ be a subset of the modes returned by
-- 'getDeviceGroupPresentCapabilitiesKHR'.
--
-- == Valid Usage
--
-- -   #VUID-vkGetDeviceGroupSurfacePresentModesKHR-surface-06212#
--     @surface@ /must/ be supported by all physical devices associated
--     with @device@, as reported by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR'
--     or an equivalent platform-specific mechanism
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDeviceGroupSurfacePresentModesKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDeviceGroupSurfacePresentModesKHR-surface-parameter#
--     @surface@ /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- -   #VUID-vkGetDeviceGroupSurfacePresentModesKHR-pModes-parameter#
--     @pModes@ /must/ be a valid pointer to a
--     'DeviceGroupPresentModeFlagsKHR' value
--
-- -   #VUID-vkGetDeviceGroupSurfacePresentModesKHR-commonparent# Both of
--     @device@, and @surface@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @surface@ /must/ be externally synchronized
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceGroupPresentModeFlagsKHR',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
getDeviceGroupSurfacePresentModesKHR :: forall io
                                      . (MonadIO io)
                                     => -- | @device@ is the logical device.
                                        Device
                                     -> -- | @surface@ is the surface.
                                        SurfaceKHR
                                     -> io (("modes" ::: DeviceGroupPresentModeFlagsKHR))
getDeviceGroupSurfacePresentModesKHR device surface = liftIO . evalContT $ do
  let vkGetDeviceGroupSurfacePresentModesKHRPtr = pVkGetDeviceGroupSurfacePresentModesKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceGroupSurfacePresentModesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceGroupSurfacePresentModesKHR is null" Nothing Nothing
  let vkGetDeviceGroupSurfacePresentModesKHR' = mkVkGetDeviceGroupSurfacePresentModesKHR vkGetDeviceGroupSurfacePresentModesKHRPtr
  pPModes <- ContT $ bracket (callocBytes @DeviceGroupPresentModeFlagsKHR 4) free
  r <- lift $ traceAroundEvent "vkGetDeviceGroupSurfacePresentModesKHR" (vkGetDeviceGroupSurfacePresentModesKHR' (deviceHandle (device)) (surface) (pPModes))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pModes <- lift $ peek @DeviceGroupPresentModeFlagsKHR pPModes
  pure $ (pModes)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireNextImage2KHRUnsafe
  :: FunPtr (Ptr Device_T -> Ptr AcquireNextImageInfoKHR -> Ptr Word32 -> IO Result) -> Ptr Device_T -> Ptr AcquireNextImageInfoKHR -> Ptr Word32 -> IO Result

foreign import ccall
  "dynamic" mkVkAcquireNextImage2KHRSafe
  :: FunPtr (Ptr Device_T -> Ptr AcquireNextImageInfoKHR -> Ptr Word32 -> IO Result) -> Ptr Device_T -> Ptr AcquireNextImageInfoKHR -> Ptr Word32 -> IO Result

-- | acquireNextImage2KHR with selectable safeness
acquireNextImage2KHRSafeOrUnsafe :: forall io
                                  . (MonadIO io)
                                 => (FunPtr (Ptr Device_T -> Ptr AcquireNextImageInfoKHR -> Ptr Word32 -> IO Result) -> Ptr Device_T -> Ptr AcquireNextImageInfoKHR -> Ptr Word32 -> IO Result)
                                 -> -- | @device@ is the device associated with @swapchain@.
                                    Device
                                 -> -- | @pAcquireInfo@ is a pointer to a 'AcquireNextImageInfoKHR' structure
                                    -- containing parameters of the acquire.
                                    ("acquireInfo" ::: AcquireNextImageInfoKHR)
                                 -> io (Result, ("imageIndex" ::: Word32))
acquireNextImage2KHRSafeOrUnsafe mkVkAcquireNextImage2KHR device acquireInfo = liftIO . evalContT $ do
  let vkAcquireNextImage2KHRPtr = pVkAcquireNextImage2KHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkAcquireNextImage2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireNextImage2KHR is null" Nothing Nothing
  let vkAcquireNextImage2KHR' = mkVkAcquireNextImage2KHR vkAcquireNextImage2KHRPtr
  pAcquireInfo <- ContT $ withCStruct (acquireInfo)
  pPImageIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkAcquireNextImage2KHR" (vkAcquireNextImage2KHR' (deviceHandle (device)) pAcquireInfo (pPImageIndex))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImageIndex <- lift $ peek @Word32 pPImageIndex
  pure $ (r, pImageIndex)

-- | vkAcquireNextImage2KHR - Retrieve the index of the next available
-- presentable image
--
-- == Valid Usage
--
-- -   #VUID-vkAcquireNextImage2KHR-swapchain-01803# If the number of
--     currently acquired images is greater than the difference between the
--     number of images in the @swapchain@ member of @pAcquireInfo@ and the
--     value of
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@minImageCount@
--     as returned by a call to
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'
--     with the @surface@ used to create @swapchain@, the @timeout@ member
--     of @pAcquireInfo@ /must/ not be @UINT64_MAX@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkAcquireNextImage2KHR-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkAcquireNextImage2KHR-pAcquireInfo-parameter# @pAcquireInfo@
--     /must/ be a valid pointer to a valid 'AcquireNextImageInfoKHR'
--     structure
--
-- -   #VUID-vkAcquireNextImage2KHR-pImageIndex-parameter# @pImageIndex@
--     /must/ be a valid pointer to a @uint32_t@ value
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.TIMEOUT'
--
--     -   'Vulkan.Core10.Enums.Result.NOT_READY'
--
--     -   'Vulkan.Core10.Enums.Result.SUBOPTIMAL_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'AcquireNextImageInfoKHR', 'Vulkan.Core10.Handles.Device'
acquireNextImage2KHR :: forall io
                      . (MonadIO io)
                     => -- | @device@ is the device associated with @swapchain@.
                        Device
                     -> -- | @pAcquireInfo@ is a pointer to a 'AcquireNextImageInfoKHR' structure
                        -- containing parameters of the acquire.
                        ("acquireInfo" ::: AcquireNextImageInfoKHR)
                     -> io (Result, ("imageIndex" ::: Word32))
acquireNextImage2KHR = acquireNextImage2KHRSafeOrUnsafe mkVkAcquireNextImage2KHRUnsafe

-- | A variant of 'acquireNextImage2KHR' which makes a *safe* FFI call
acquireNextImage2KHRSafe :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the device associated with @swapchain@.
                            Device
                         -> -- | @pAcquireInfo@ is a pointer to a 'AcquireNextImageInfoKHR' structure
                            -- containing parameters of the acquire.
                            ("acquireInfo" ::: AcquireNextImageInfoKHR)
                         -> io (Result, ("imageIndex" ::: Word32))
acquireNextImage2KHRSafe = acquireNextImage2KHRSafeOrUnsafe mkVkAcquireNextImage2KHRSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDevicePresentRectanglesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr Rect2D -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr Rect2D -> IO Result

-- | vkGetPhysicalDevicePresentRectanglesKHR - Query present rectangles for a
-- surface on a physical device
--
-- = Description
--
-- If @pRects@ is @NULL@, then the number of rectangles used when
-- presenting the given @surface@ is returned in @pRectCount@. Otherwise,
-- @pRectCount@ /must/ point to a variable set by the user to the number of
-- elements in the @pRects@ array, and on return the variable is
-- overwritten with the number of structures actually written to @pRects@.
-- If the value of @pRectCount@ is less than the number of rectangles, at
-- most @pRectCount@ structures will be written, and
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available rectangles were returned.
--
-- The values returned by this command are not invariant, and /may/ change
-- in response to the surface being moved, resized, or occluded.
--
-- The rectangles returned by this command /must/ not overlap.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPhysicalDevicePresentRectanglesKHR-surface-06523#
--     @surface@ /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- -   #VUID-vkGetPhysicalDevicePresentRectanglesKHR-surface-06211#
--     @surface@ /must/ be supported by @physicalDevice@, as reported by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR'
--     or an equivalent platform-specific mechanism
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDevicePresentRectanglesKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDevicePresentRectanglesKHR-surface-parameter#
--     @surface@ /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- -   #VUID-vkGetPhysicalDevicePresentRectanglesKHR-pRectCount-parameter#
--     @pRectCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDevicePresentRectanglesKHR-pRects-parameter# If
--     the value referenced by @pRectCount@ is not @0@, and @pRects@ is not
--     @NULL@, @pRects@ /must/ be a valid pointer to an array of
--     @pRectCount@ 'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- -   #VUID-vkGetPhysicalDevicePresentRectanglesKHR-commonparent# Both of
--     @physicalDevice@, and @surface@ /must/ have been created, allocated,
--     or retrieved from the same 'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @surface@ /must/ be externally synchronized
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'Vulkan.Core10.FundamentalTypes.Rect2D',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
getPhysicalDevicePresentRectanglesKHR :: forall io
                                       . (MonadIO io)
                                      => -- | @physicalDevice@ is the physical device.
                                         PhysicalDevice
                                      -> -- | @surface@ is the surface.
                                         SurfaceKHR
                                      -> io (Result, ("rects" ::: Vector Rect2D))
getPhysicalDevicePresentRectanglesKHR physicalDevice surface = liftIO . evalContT $ do
  let vkGetPhysicalDevicePresentRectanglesKHRPtr = pVkGetPhysicalDevicePresentRectanglesKHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDevicePresentRectanglesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDevicePresentRectanglesKHR is null" Nothing Nothing
  let vkGetPhysicalDevicePresentRectanglesKHR' = mkVkGetPhysicalDevicePresentRectanglesKHR vkGetPhysicalDevicePresentRectanglesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPRectCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDevicePresentRectanglesKHR" (vkGetPhysicalDevicePresentRectanglesKHR' physicalDevice' (surface) (pPRectCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pRectCount <- lift $ peek @Word32 pPRectCount
  pPRects <- ContT $ bracket (callocBytes @Rect2D ((fromIntegral (pRectCount)) * 16)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPRects `advancePtrBytes` (i * 16) :: Ptr Rect2D) . ($ ())) [0..(fromIntegral (pRectCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDevicePresentRectanglesKHR" (vkGetPhysicalDevicePresentRectanglesKHR' physicalDevice' (surface) (pPRectCount) ((pPRects)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pRectCount' <- lift $ peek @Word32 pPRectCount
  pRects' <- lift $ generateM (fromIntegral (pRectCount')) (\i -> peekCStruct @Rect2D (((pPRects) `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
  pure $ ((r'), pRects')


-- | VkSwapchainCreateInfoKHR - Structure specifying parameters of a newly
-- created swapchain object
--
-- = Description
--
-- Upon calling 'createSwapchainKHR' with an @oldSwapchain@ that is not
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', @oldSwapchain@ is
-- retired — even if creation of the new swapchain fails. The new swapchain
-- is created in the non-retired state whether or not @oldSwapchain@ is
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- Upon calling 'createSwapchainKHR' with an @oldSwapchain@ that is not
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', any images from @oldSwapchain@
-- that are not acquired by the application /may/ be freed by the
-- implementation, which /may/ occur even if creation of the new swapchain
-- fails. The application /can/ destroy @oldSwapchain@ to free all memory
-- associated with @oldSwapchain@.
--
-- Note
--
-- Multiple retired swapchains /can/ be associated with the same
-- 'Vulkan.Extensions.Handles.SurfaceKHR' through multiple uses of
-- @oldSwapchain@ that outnumber calls to 'destroySwapchainKHR'.
--
-- After @oldSwapchain@ is retired, the application /can/ pass to
-- 'queuePresentKHR' any images it had already acquired from
-- @oldSwapchain@. E.g., an application may present an image from the old
-- swapchain before an image from the new swapchain is ready to be
-- presented. As usual, 'queuePresentKHR' /may/ fail if @oldSwapchain@ has
-- entered a state that causes
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR' to be returned.
--
-- The application /can/ continue to use a shared presentable image
-- obtained from @oldSwapchain@ until a presentable image is acquired from
-- the new swapchain, as long as it has not entered a state that causes it
-- to return 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'.
--
-- == Valid Usage
--
-- -   #VUID-VkSwapchainCreateInfoKHR-surface-01270# @surface@ /must/ be a
--     surface that is supported by the device as determined using
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR'
--
-- -   #VUID-VkSwapchainCreateInfoKHR-minImageCount-01272# @minImageCount@
--     /must/ be less than or equal to the value returned in the
--     @maxImageCount@ member of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface if the returned @maxImageCount@ is not zero
--
-- -   #VUID-VkSwapchainCreateInfoKHR-presentMode-02839# If @presentMode@
--     is not
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
--     nor
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR',
--     then @minImageCount@ /must/ be greater than or equal to the value
--     returned in the @minImageCount@ member of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   #VUID-VkSwapchainCreateInfoKHR-minImageCount-01383# @minImageCount@
--     /must/ be @1@ if @presentMode@ is either
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR'
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageFormat-01273# @imageFormat@ and
--     @imageColorSpace@ /must/ match the @format@ and @colorSpace@
--     members, respectively, of one of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceFormatKHR' structures
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceFormatsKHR'
--     for the surface
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageExtent-01274# @imageExtent@
--     /must/ be between @minImageExtent@ and @maxImageExtent@, inclusive,
--     where @minImageExtent@ and @maxImageExtent@ are members of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageExtent-01689# @imageExtent@
--     members @width@ and @height@ /must/ both be non-zero
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageArrayLayers-01275#
--     @imageArrayLayers@ /must/ be greater than @0@ and less than or equal
--     to the @maxImageArrayLayers@ member of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   #VUID-VkSwapchainCreateInfoKHR-presentMode-01427# If @presentMode@
--     is 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_IMMEDIATE_KHR',
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR',
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_RELAXED_KHR',
--     @imageUsage@ /must/ be a subset of the supported usage flags present
--     in the @supportedUsageFlags@ member of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for @surface@
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageUsage-01384# If @presentMode@ is
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR',
--     @imageUsage@ /must/ be a subset of the supported usage flags present
--     in the @sharedPresentSupportedUsageFlags@ member of the
--     'Vulkan.Extensions.VK_KHR_shared_presentable_image.SharedPresentSurfaceCapabilitiesKHR'
--     structure returned by
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'
--     for @surface@
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageSharingMode-01277# If
--     @imageSharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageSharingMode-01278# If
--     @imageSharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @queueFamilyIndexCount@ /must/ be greater than @1@
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageSharingMode-01428# If
--     @imageSharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT', each
--     element of @pQueueFamilyIndices@ /must/ be unique and /must/ be less
--     than @pQueueFamilyPropertyCount@ returned by either
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2'
--     for the @physicalDevice@ that was used to create @device@
--
-- -   #VUID-VkSwapchainCreateInfoKHR-preTransform-01279# @preTransform@
--     /must/ be one of the bits present in the @supportedTransforms@
--     member of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   #VUID-VkSwapchainCreateInfoKHR-compositeAlpha-01280#
--     @compositeAlpha@ /must/ be one of the bits present in the
--     @supportedCompositeAlpha@ member of the
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
--     returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   #VUID-VkSwapchainCreateInfoKHR-presentMode-01281# @presentMode@
--     /must/ be one of the
--     'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR' values returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR'
--     for the surface
--
-- -   #VUID-VkSwapchainCreateInfoKHR-physicalDeviceCount-01429# If the
--     logical device was created with
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.DeviceGroupDeviceCreateInfo'::@physicalDeviceCount@
--     equal to 1, @flags@ /must/ not contain
--     'SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR'
--
-- -   #VUID-VkSwapchainCreateInfoKHR-oldSwapchain-01933# If @oldSwapchain@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @oldSwapchain@
--     /must/ be a non-retired swapchain associated with native window
--     referred to by @surface@
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageFormat-01778# The
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#swapchain-wsi-image-create-info implied image creation parameters>
--     of the swapchain /must/ be supported as reported by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties'
--
-- -   #VUID-VkSwapchainCreateInfoKHR-flags-03168# If @flags@ contains
--     'SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR' then the @pNext@ chain
--     /must/ include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure with a @viewFormatCount@ greater than zero and
--     @pViewFormats@ /must/ have an element equal to @imageFormat@
--
-- -   #VUID-VkSwapchainCreateInfoKHR-pNext-04099# If a
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure was included in the @pNext@ chain and
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@viewFormatCount@
--     is not zero then all of the formats in
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@pViewFormats@
--     /must/ be compatible with the @format@ as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#formats-compatibility compatibility table>
--
-- -   #VUID-VkSwapchainCreateInfoKHR-flags-04100# If @flags@ does not
--     contain 'SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR' and the @pNext@
--     chain include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure then
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@viewFormatCount@
--     /must/ be @0@ or @1@
--
-- -   #VUID-VkSwapchainCreateInfoKHR-flags-03187# If @flags@ contains
--     'SWAPCHAIN_CREATE_PROTECTED_BIT_KHR', then
--     'Vulkan.Extensions.VK_KHR_surface_protected_capabilities.SurfaceProtectedCapabilitiesKHR'::@supportsProtected@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE' in the
--     'Vulkan.Extensions.VK_KHR_surface_protected_capabilities.SurfaceProtectedCapabilitiesKHR'
--     structure returned by
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'
--     for @surface@
--
-- -   #VUID-VkSwapchainCreateInfoKHR-pNext-02679# If the @pNext@ chain
--     includes a
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveInfoEXT'
--     structure with its @fullScreenExclusive@ member set to
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT',
--     and @surface@ was created using
--     'Vulkan.Extensions.VK_KHR_win32_surface.createWin32SurfaceKHR', a
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveWin32InfoEXT'
--     structure /must/ be included in the @pNext@ chain
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSwapchainCreateInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR'
--
-- -   #VUID-VkSwapchainCreateInfoKHR-pNext-pNext# Each @pNext@ member of
--     any structure (including this one) in the @pNext@ chain /must/ be
--     either @NULL@ or a pointer to a valid instance of
--     'DeviceGroupSwapchainCreateInfoKHR',
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo',
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveInfoEXT',
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveWin32InfoEXT',
--     'Vulkan.Extensions.VK_EXT_display_control.SwapchainCounterCreateInfoEXT',
--     or
--     'Vulkan.Extensions.VK_AMD_display_native_hdr.SwapchainDisplayNativeHdrCreateInfoAMD'
--
-- -   #VUID-VkSwapchainCreateInfoKHR-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkSwapchainCreateInfoKHR-flags-parameter# @flags@ /must/ be a
--     valid combination of 'SwapchainCreateFlagBitsKHR' values
--
-- -   #VUID-VkSwapchainCreateInfoKHR-surface-parameter# @surface@ /must/
--     be a valid 'Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageFormat-parameter# @imageFormat@
--     /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageColorSpace-parameter#
--     @imageColorSpace@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_surface.ColorSpaceKHR' value
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageUsage-parameter# @imageUsage@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' values
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageUsage-requiredbitmask#
--     @imageUsage@ /must/ not be @0@
--
-- -   #VUID-VkSwapchainCreateInfoKHR-imageSharingMode-parameter#
--     @imageSharingMode@ /must/ be a valid
--     'Vulkan.Core10.Enums.SharingMode.SharingMode' value
--
-- -   #VUID-VkSwapchainCreateInfoKHR-preTransform-parameter#
--     @preTransform@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR' value
--
-- -   #VUID-VkSwapchainCreateInfoKHR-compositeAlpha-parameter#
--     @compositeAlpha@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_surface.CompositeAlphaFlagBitsKHR' value
--
-- -   #VUID-VkSwapchainCreateInfoKHR-presentMode-parameter# @presentMode@
--     /must/ be a valid 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR'
--     value
--
-- -   #VUID-VkSwapchainCreateInfoKHR-oldSwapchain-parameter# If
--     @oldSwapchain@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @oldSwapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-VkSwapchainCreateInfoKHR-oldSwapchain-parent# If
--     @oldSwapchain@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @surface@
--
-- -   #VUID-VkSwapchainCreateInfoKHR-commonparent# Both of @oldSwapchain@,
--     and @surface@ that are valid handles of non-ignored parameters
--     /must/ have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Extensions.VK_KHR_surface.ColorSpaceKHR',
-- 'Vulkan.Extensions.VK_KHR_surface.CompositeAlphaFlagBitsKHR',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR',
-- 'Vulkan.Core10.Enums.SharingMode.SharingMode',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR',
-- 'SwapchainCreateFlagsKHR', 'Vulkan.Extensions.Handles.SwapchainKHR',
-- 'Vulkan.Extensions.VK_KHR_display_swapchain.createSharedSwapchainsKHR',
-- 'createSwapchainKHR'
data SwapchainCreateInfoKHR (es :: [Type]) = SwapchainCreateInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of 'SwapchainCreateFlagBitsKHR' indicating
    -- parameters of the swapchain creation.
    flags :: SwapchainCreateFlagsKHR
  , -- | @surface@ is the surface onto which the swapchain will present images.
    -- If the creation succeeds, the swapchain becomes associated with
    -- @surface@.
    surface :: SurfaceKHR
  , -- | @minImageCount@ is the minimum number of presentable images that the
    -- application needs. The implementation will either create the swapchain
    -- with at least that many images, or it will fail to create the swapchain.
    minImageCount :: Word32
  , -- | @imageFormat@ is a 'Vulkan.Core10.Enums.Format.Format' value specifying
    -- the format the swapchain image(s) will be created with.
    imageFormat :: Format
  , -- | @imageColorSpace@ is a 'Vulkan.Extensions.VK_KHR_surface.ColorSpaceKHR'
    -- value specifying the way the swapchain interprets image data.
    imageColorSpace :: ColorSpaceKHR
  , -- | @imageExtent@ is the size (in pixels) of the swapchain image(s). The
    -- behavior is platform-dependent if the image extent does not match the
    -- surface’s @currentExtent@ as returned by
    -- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'.
    --
    -- Note
    --
    -- On some platforms, it is normal that @maxImageExtent@ /may/ become @(0,
    -- 0)@, for example when the window is minimized. In such a case, it is not
    -- possible to create a swapchain due to the Valid Usage requirements.
    imageExtent :: Extent2D
  , -- | @imageArrayLayers@ is the number of views in a multiview\/stereo
    -- surface. For non-stereoscopic-3D applications, this value is 1.
    imageArrayLayers :: Word32
  , -- | @imageUsage@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' describing
    -- the intended usage of the (acquired) swapchain images.
    imageUsage :: ImageUsageFlags
  , -- | @imageSharingMode@ is the sharing mode used for the image(s) of the
    -- swapchain.
    imageSharingMode :: SharingMode
  , -- | @pQueueFamilyIndices@ is a pointer to an array of queue family indices
    -- having access to the images(s) of the swapchain when @imageSharingMode@
    -- is 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT'.
    queueFamilyIndices :: Vector Word32
  , -- | @preTransform@ is a
    -- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR' value
    -- describing the transform, relative to the presentation engine’s natural
    -- orientation, applied to the image content prior to presentation. If it
    -- does not match the @currentTransform@ value returned by
    -- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR',
    -- the presentation engine will transform the image content as part of the
    -- presentation operation.
    preTransform :: SurfaceTransformFlagBitsKHR
  , -- | @compositeAlpha@ is a
    -- 'Vulkan.Extensions.VK_KHR_surface.CompositeAlphaFlagBitsKHR' value
    -- indicating the alpha compositing mode to use when this surface is
    -- composited together with other surfaces on certain window systems.
    compositeAlpha :: CompositeAlphaFlagBitsKHR
  , -- | @presentMode@ is the presentation mode the swapchain will use. A
    -- swapchain’s present mode determines how incoming present requests will
    -- be processed and queued internally.
    presentMode :: PresentModeKHR
  , -- | @clipped@ specifies whether the Vulkan implementation is allowed to
    -- discard rendering operations that affect regions of the surface that are
    -- not visible.
    --
    -- -   If set to 'Vulkan.Core10.FundamentalTypes.TRUE', the presentable
    --     images associated with the swapchain /may/ not own all of their
    --     pixels. Pixels in the presentable images that correspond to regions
    --     of the target surface obscured by another window on the desktop, or
    --     subject to some other clipping mechanism will have undefined content
    --     when read back. Fragment shaders /may/ not execute for these pixels,
    --     and thus any side effects they would have had will not occur.
    --     Setting 'Vulkan.Core10.FundamentalTypes.TRUE' does not guarantee any
    --     clipping will occur, but allows more efficient presentation methods
    --     to be used on some platforms.
    --
    -- -   If set to 'Vulkan.Core10.FundamentalTypes.FALSE', presentable images
    --     associated with the swapchain will own all of the pixels they
    --     contain.
    --
    --     Note
    --
    --     Applications /should/ set this value to
    --     'Vulkan.Core10.FundamentalTypes.TRUE' if they do not expect to read
    --     back the content of presentable images before presenting them or
    --     after reacquiring them, and if their fragment shaders do not have
    --     any side effects that require them to run for all pixels in the
    --     presentable image.
    clipped :: Bool
  , -- | @oldSwapchain@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', or the
    -- existing non-retired swapchain currently associated with @surface@.
    -- Providing a valid @oldSwapchain@ /may/ aid in the resource reuse, and
    -- also allows the application to still present any images that are already
    -- acquired from it.
    oldSwapchain :: SwapchainKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainCreateInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SwapchainCreateInfoKHR es)

instance Extensible SwapchainCreateInfoKHR where
  extensibleTypeName = "SwapchainCreateInfoKHR"
  setNext SwapchainCreateInfoKHR{..} next' = SwapchainCreateInfoKHR{next = next', ..}
  getNext SwapchainCreateInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SwapchainCreateInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SurfaceFullScreenExclusiveWin32InfoEXT = Just f
    | Just Refl <- eqT @e @SurfaceFullScreenExclusiveInfoEXT = Just f
    | Just Refl <- eqT @e @ImageFormatListCreateInfo = Just f
    | Just Refl <- eqT @e @SwapchainDisplayNativeHdrCreateInfoAMD = Just f
    | Just Refl <- eqT @e @DeviceGroupSwapchainCreateInfoKHR = Just f
    | Just Refl <- eqT @e @SwapchainCounterCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss SwapchainCreateInfoKHR es, PokeChain es) => ToCStruct (SwapchainCreateInfoKHR es) where
  withCStruct x f = allocaBytes 104 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SwapchainCreateFlagsKHR)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr SurfaceKHR)) (surface)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (minImageCount)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (imageFormat)
    lift $ poke ((p `plusPtr` 40 :: Ptr ColorSpaceKHR)) (imageColorSpace)
    lift $ poke ((p `plusPtr` 44 :: Ptr Extent2D)) (imageExtent)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (imageArrayLayers)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageUsageFlags)) (imageUsage)
    lift $ poke ((p `plusPtr` 60 :: Ptr SharingMode)) (imageSharingMode)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueFamilyIndices)) :: Word32))
    pPQueueFamilyIndices' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (queueFamilyIndices)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (queueFamilyIndices)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ poke ((p `plusPtr` 80 :: Ptr SurfaceTransformFlagBitsKHR)) (preTransform)
    lift $ poke ((p `plusPtr` 84 :: Ptr CompositeAlphaFlagBitsKHR)) (compositeAlpha)
    lift $ poke ((p `plusPtr` 88 :: Ptr PresentModeKHR)) (presentMode)
    lift $ poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (clipped))
    lift $ poke ((p `plusPtr` 96 :: Ptr SwapchainKHR)) (oldSwapchain)
    lift $ f
  cStructSize = 104
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr SurfaceKHR)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr ColorSpaceKHR)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr Extent2D)) (zero)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageUsageFlags)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr SharingMode)) (zero)
    lift $ poke ((p `plusPtr` 80 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    lift $ poke ((p `plusPtr` 84 :: Ptr CompositeAlphaFlagBitsKHR)) (zero)
    lift $ poke ((p `plusPtr` 88 :: Ptr PresentModeKHR)) (zero)
    lift $ poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance (Extendss SwapchainCreateInfoKHR es, PeekChain es) => FromCStruct (SwapchainCreateInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @SwapchainCreateFlagsKHR ((p `plusPtr` 16 :: Ptr SwapchainCreateFlagsKHR))
    surface <- peek @SurfaceKHR ((p `plusPtr` 24 :: Ptr SurfaceKHR))
    minImageCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    imageFormat <- peek @Format ((p `plusPtr` 36 :: Ptr Format))
    imageColorSpace <- peek @ColorSpaceKHR ((p `plusPtr` 40 :: Ptr ColorSpaceKHR))
    imageExtent <- peekCStruct @Extent2D ((p `plusPtr` 44 :: Ptr Extent2D))
    imageArrayLayers <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    imageUsage <- peek @ImageUsageFlags ((p `plusPtr` 56 :: Ptr ImageUsageFlags))
    imageSharingMode <- peek @SharingMode ((p `plusPtr` 60 :: Ptr SharingMode))
    queueFamilyIndexCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pQueueFamilyIndices <- peek @(Ptr Word32) ((p `plusPtr` 72 :: Ptr (Ptr Word32)))
    pQueueFamilyIndices' <- generateM (fromIntegral queueFamilyIndexCount) (\i -> peek @Word32 ((pQueueFamilyIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    preTransform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 80 :: Ptr SurfaceTransformFlagBitsKHR))
    compositeAlpha <- peek @CompositeAlphaFlagBitsKHR ((p `plusPtr` 84 :: Ptr CompositeAlphaFlagBitsKHR))
    presentMode <- peek @PresentModeKHR ((p `plusPtr` 88 :: Ptr PresentModeKHR))
    clipped <- peek @Bool32 ((p `plusPtr` 92 :: Ptr Bool32))
    oldSwapchain <- peek @SwapchainKHR ((p `plusPtr` 96 :: Ptr SwapchainKHR))
    pure $ SwapchainCreateInfoKHR
             next flags surface minImageCount imageFormat imageColorSpace imageExtent imageArrayLayers imageUsage imageSharingMode pQueueFamilyIndices' preTransform compositeAlpha presentMode (bool32ToBool clipped) oldSwapchain

instance es ~ '[] => Zero (SwapchainCreateInfoKHR es) where
  zero = SwapchainCreateInfoKHR
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           mempty
           zero
           zero
           zero
           zero
           zero


-- | VkPresentInfoKHR - Structure describing parameters of a queue
-- presentation
--
-- = Description
--
-- Before an application /can/ present an image, the image’s layout /must/
-- be transitioned to the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR' layout,
-- or for a shared presentable image the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
-- layout.
--
-- Note
--
-- When transitioning the image to
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR' or
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR', there is
-- no need to delay subsequent processing, or perform any visibility
-- operations (as 'queuePresentKHR' performs automatic visibility
-- operations). To achieve this, the @dstAccessMask@ member of the
-- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier' /should/ be set to @0@,
-- and the @dstStageMask@ parameter /should/ be set to
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT'.
--
-- == Valid Usage
--
-- -   #VUID-VkPresentInfoKHR-pImageIndices-01430# Each element of
--     @pImageIndices@ /must/ be the index of a presentable image acquired
--     from the swapchain specified by the corresponding element of the
--     @pSwapchains@ array, and the presented image subresource /must/ be
--     in the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PRESENT_SRC_KHR' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--     layout at the time the operation is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkPresentInfoKHR-pNext-06235# If a
--     'Vulkan.Extensions.VK_KHR_present_id.PresentIdKHR' structure is
--     included in the @pNext@ chain, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-presentId presentId>
--     feature is not enabled, each @presentIds@ entry in that structure
--     /must/ be NULL
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPresentInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_INFO_KHR'
--
-- -   #VUID-VkPresentInfoKHR-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'DeviceGroupPresentInfoKHR',
--     'Vulkan.Extensions.VK_KHR_display_swapchain.DisplayPresentInfoKHR',
--     'Vulkan.Extensions.VK_GGP_frame_token.PresentFrameTokenGGP',
--     'Vulkan.Extensions.VK_KHR_present_id.PresentIdKHR',
--     'Vulkan.Extensions.VK_KHR_incremental_present.PresentRegionsKHR', or
--     'Vulkan.Extensions.VK_GOOGLE_display_timing.PresentTimesInfoGOOGLE'
--
-- -   #VUID-VkPresentInfoKHR-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPresentInfoKHR-pWaitSemaphores-parameter# If
--     @waitSemaphoreCount@ is not @0@, @pWaitSemaphores@ /must/ be a valid
--     pointer to an array of @waitSemaphoreCount@ valid
--     'Vulkan.Core10.Handles.Semaphore' handles
--
-- -   #VUID-VkPresentInfoKHR-pSwapchains-parameter# @pSwapchains@ /must/
--     be a valid pointer to an array of @swapchainCount@ valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handles
--
-- -   #VUID-VkPresentInfoKHR-pImageIndices-parameter# @pImageIndices@
--     /must/ be a valid pointer to an array of @swapchainCount@ @uint32_t@
--     values
--
-- -   #VUID-VkPresentInfoKHR-pResults-parameter# If @pResults@ is not
--     @NULL@, @pResults@ /must/ be a valid pointer to an array of
--     @swapchainCount@ 'Vulkan.Core10.Enums.Result.Result' values
--
-- -   #VUID-VkPresentInfoKHR-swapchainCount-arraylength# @swapchainCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkPresentInfoKHR-commonparent# Both of the elements of
--     @pSwapchains@, and the elements of @pWaitSemaphores@ that are valid
--     handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- 'Vulkan.Core10.Enums.Result.Result', 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SwapchainKHR', 'queuePresentKHR'
data PresentInfoKHR (es :: [Type]) = PresentInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @pWaitSemaphores@ is @NULL@ or a pointer to an array of
    -- 'Vulkan.Core10.Handles.Semaphore' objects with @waitSemaphoreCount@
    -- entries, and specifies the semaphores to wait for before issuing the
    -- present request.
    waitSemaphores :: Vector Semaphore
  , -- | @pSwapchains@ is a pointer to an array of
    -- 'Vulkan.Extensions.Handles.SwapchainKHR' objects with @swapchainCount@
    -- entries. A given swapchain /must/ not appear in this list more than
    -- once.
    swapchains :: Vector SwapchainKHR
  , -- | @pImageIndices@ is a pointer to an array of indices into the array of
    -- each swapchain’s presentable images, with @swapchainCount@ entries. Each
    -- entry in this array identifies the image to present on the corresponding
    -- entry in the @pSwapchains@ array.
    imageIndices :: Vector Word32
  , -- | @pResults@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.Result.Result' typed elements with @swapchainCount@
    -- entries. Applications that do not need per-swapchain results /can/ use
    -- @NULL@ for @pResults@. If non-@NULL@, each entry in @pResults@ will be
    -- set to the 'Vulkan.Core10.Enums.Result.Result' for presenting the
    -- swapchain corresponding to the same index in @pSwapchains@.
    results :: Ptr Result
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PresentInfoKHR es)

instance Extensible PresentInfoKHR where
  extensibleTypeName = "PresentInfoKHR"
  setNext PresentInfoKHR{..} next' = PresentInfoKHR{next = next', ..}
  getNext PresentInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PresentInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PresentFrameTokenGGP = Just f
    | Just Refl <- eqT @e @PresentTimesInfoGOOGLE = Just f
    | Just Refl <- eqT @e @PresentIdKHR = Just f
    | Just Refl <- eqT @e @DeviceGroupPresentInfoKHR = Just f
    | Just Refl <- eqT @e @PresentRegionsKHR = Just f
    | Just Refl <- eqT @e @DisplayPresentInfoKHR = Just f
    | otherwise = Nothing

instance (Extendss PresentInfoKHR es, PokeChain es) => ToCStruct (PresentInfoKHR es) where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (waitSemaphores)) :: Word32))
    pPWaitSemaphores' <- ContT $ allocaBytes @Semaphore ((Data.Vector.length (waitSemaphores)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (waitSemaphores)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Semaphore))) (pPWaitSemaphores')
    let pSwapchainsLength = Data.Vector.length $ (swapchains)
    lift $ unless ((Data.Vector.length $ (imageIndices)) == pSwapchainsLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pImageIndices and pSwapchains must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral pSwapchainsLength :: Word32))
    pPSwapchains' <- ContT $ allocaBytes @SwapchainKHR ((Data.Vector.length (swapchains)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSwapchains' `plusPtr` (8 * (i)) :: Ptr SwapchainKHR) (e)) (swapchains)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SwapchainKHR))) (pPSwapchains')
    pPImageIndices' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (imageIndices)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPImageIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (imageIndices)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPImageIndices')
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr Result))) (results)
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss PresentInfoKHR es, PeekChain es) => FromCStruct (PresentInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    waitSemaphoreCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pWaitSemaphores <- peek @(Ptr Semaphore) ((p `plusPtr` 24 :: Ptr (Ptr Semaphore)))
    pWaitSemaphores' <- generateM (fromIntegral waitSemaphoreCount) (\i -> peek @Semaphore ((pWaitSemaphores `advancePtrBytes` (8 * (i)) :: Ptr Semaphore)))
    swapchainCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pSwapchains <- peek @(Ptr SwapchainKHR) ((p `plusPtr` 40 :: Ptr (Ptr SwapchainKHR)))
    pSwapchains' <- generateM (fromIntegral swapchainCount) (\i -> peek @SwapchainKHR ((pSwapchains `advancePtrBytes` (8 * (i)) :: Ptr SwapchainKHR)))
    pImageIndices <- peek @(Ptr Word32) ((p `plusPtr` 48 :: Ptr (Ptr Word32)))
    pImageIndices' <- generateM (fromIntegral swapchainCount) (\i -> peek @Word32 ((pImageIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pResults <- peek @(Ptr Result) ((p `plusPtr` 56 :: Ptr (Ptr Result)))
    pure $ PresentInfoKHR
             next pWaitSemaphores' pSwapchains' pImageIndices' pResults

instance es ~ '[] => Zero (PresentInfoKHR es) where
  zero = PresentInfoKHR
           ()
           mempty
           mempty
           mempty
           zero


-- | VkDeviceGroupPresentCapabilitiesKHR - Present capabilities from other
-- physical devices
--
-- = Description
--
-- @modes@ always has 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR' set.
--
-- The present mode flags are also used when presenting an image, in
-- 'DeviceGroupPresentInfoKHR'::@mode@.
--
-- If a device group only includes a single physical device, then @modes@
-- /must/ equal 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'DeviceGroupPresentModeFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceGroupPresentCapabilitiesKHR'
data DeviceGroupPresentCapabilitiesKHR = DeviceGroupPresentCapabilitiesKHR
  { -- | @presentMask@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DEVICE_GROUP_SIZE' @uint32_t@ masks,
    -- where the mask at element i is non-zero if physical device i has a
    -- presentation engine, and where bit j is set in element i if physical
    -- device i /can/ present swapchain images from physical device j. If
    -- element i is non-zero, then bit i /must/ be set.
    presentMask :: Vector Word32
  , -- | @modes@ is a bitmask of 'DeviceGroupPresentModeFlagBitsKHR' indicating
    -- which device group presentation modes are supported.
    modes :: DeviceGroupPresentModeFlagsKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupPresentCapabilitiesKHR)
#endif
deriving instance Show DeviceGroupPresentCapabilitiesKHR

instance ToCStruct DeviceGroupPresentCapabilitiesKHR where
  withCStruct x f = allocaBytes 152 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupPresentCapabilitiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    unless ((Data.Vector.length $ (presentMask)) <= MAX_DEVICE_GROUP_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "presentMask is too long, a maximum of MAX_DEVICE_GROUP_SIZE elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DEVICE_GROUP_SIZE Word32)))) `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (presentMask)
    poke ((p `plusPtr` 144 :: Ptr DeviceGroupPresentModeFlagsKHR)) (modes)
    f
  cStructSize = 152
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 144 :: Ptr DeviceGroupPresentModeFlagsKHR)) (zero)
    f

instance FromCStruct DeviceGroupPresentCapabilitiesKHR where
  peekCStruct p = do
    presentMask <- generateM (MAX_DEVICE_GROUP_SIZE) (\i -> peek @Word32 (((lowerArrayPtr @Word32 ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DEVICE_GROUP_SIZE Word32)))) `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    modes <- peek @DeviceGroupPresentModeFlagsKHR ((p `plusPtr` 144 :: Ptr DeviceGroupPresentModeFlagsKHR))
    pure $ DeviceGroupPresentCapabilitiesKHR
             presentMask modes

instance Storable DeviceGroupPresentCapabilitiesKHR where
  sizeOf ~_ = 152
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceGroupPresentCapabilitiesKHR where
  zero = DeviceGroupPresentCapabilitiesKHR
           mempty
           zero


-- | VkImageSwapchainCreateInfoKHR - Specify that an image will be bound to
-- swapchain memory
--
-- == Valid Usage
--
-- -   #VUID-VkImageSwapchainCreateInfoKHR-swapchain-00995# If @swapchain@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the fields of
--     'Vulkan.Core10.Image.ImageCreateInfo' /must/ match the
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#swapchain-wsi-image-create-info implied image creation parameters>
--     of the swapchain
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageSwapchainCreateInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR'
--
-- -   #VUID-VkImageSwapchainCreateInfoKHR-swapchain-parameter# If
--     @swapchain@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
data ImageSwapchainCreateInfoKHR = ImageSwapchainCreateInfoKHR
  { -- | @swapchain@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a handle of a
    -- swapchain that the image will be bound to.
    swapchain :: SwapchainKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageSwapchainCreateInfoKHR)
#endif
deriving instance Show ImageSwapchainCreateInfoKHR

instance ToCStruct ImageSwapchainCreateInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSwapchainCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (swapchain)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ImageSwapchainCreateInfoKHR where
  peekCStruct p = do
    swapchain <- peek @SwapchainKHR ((p `plusPtr` 16 :: Ptr SwapchainKHR))
    pure $ ImageSwapchainCreateInfoKHR
             swapchain

instance Storable ImageSwapchainCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSwapchainCreateInfoKHR where
  zero = ImageSwapchainCreateInfoKHR
           zero


-- | VkBindImageMemorySwapchainInfoKHR - Structure specifying swapchain image
-- memory to bind to
--
-- = Description
--
-- If @swapchain@ is not @NULL@, the @swapchain@ and @imageIndex@ are used
-- to determine the memory that the image is bound to, instead of @memory@
-- and @memoryOffset@.
--
-- Memory /can/ be bound to a swapchain and use the @pDeviceIndices@ or
-- @pSplitInstanceBindRegions@ members of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo'.
--
-- == Valid Usage
--
-- -   #VUID-VkBindImageMemorySwapchainInfoKHR-imageIndex-01644#
--     @imageIndex@ /must/ be less than the number of images in @swapchain@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindImageMemorySwapchainInfoKHR-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR'
--
-- -   #VUID-VkBindImageMemorySwapchainInfoKHR-swapchain-parameter#
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
data BindImageMemorySwapchainInfoKHR = BindImageMemorySwapchainInfoKHR
  { -- | @swapchain@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a swapchain
    -- handle.
    swapchain :: SwapchainKHR
  , -- | @imageIndex@ is an image index within @swapchain@.
    imageIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindImageMemorySwapchainInfoKHR)
#endif
deriving instance Show BindImageMemorySwapchainInfoKHR

instance ToCStruct BindImageMemorySwapchainInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindImageMemorySwapchainInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (swapchain)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (imageIndex)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct BindImageMemorySwapchainInfoKHR where
  peekCStruct p = do
    swapchain <- peek @SwapchainKHR ((p `plusPtr` 16 :: Ptr SwapchainKHR))
    imageIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ BindImageMemorySwapchainInfoKHR
             swapchain imageIndex

instance Storable BindImageMemorySwapchainInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindImageMemorySwapchainInfoKHR where
  zero = BindImageMemorySwapchainInfoKHR
           zero
           zero


-- | VkAcquireNextImageInfoKHR - Structure specifying parameters of the
-- acquire
--
-- = Description
--
-- If 'acquireNextImageKHR' is used, the device mask is considered to
-- include all physical devices in the logical device.
--
-- Note
--
-- 'acquireNextImage2KHR' signals at most one semaphore, even if the
-- application requests waiting for multiple physical devices to be ready
-- via the @deviceMask@. However, only a single physical device /can/ wait
-- on that semaphore, since the semaphore becomes unsignaled when the wait
-- succeeds. For other physical devices to wait for the image to be ready,
-- it is necessary for the application to submit semaphore signal
-- operation(s) to that first physical device to signal additional
-- semaphore(s) after the wait succeeds, which the other physical device(s)
-- /can/ wait upon.
--
-- == Valid Usage
--
-- -   #VUID-VkAcquireNextImageInfoKHR-swapchain-01675# @swapchain@ /must/
--     not be in the retired state
--
-- -   #VUID-VkAcquireNextImageInfoKHR-semaphore-01288# If @semaphore@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ be unsignaled
--
-- -   #VUID-VkAcquireNextImageInfoKHR-semaphore-01781# If @semaphore@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ not have any
--     uncompleted signal or wait operations pending
--
-- -   #VUID-VkAcquireNextImageInfoKHR-fence-01289# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' it /must/ be unsignaled and
--     /must/ not be associated with any other queue command that has not
--     yet completed execution on that queue
--
-- -   #VUID-VkAcquireNextImageInfoKHR-semaphore-01782# @semaphore@ and
--     @fence@ /must/ not both be equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkAcquireNextImageInfoKHR-deviceMask-01290# @deviceMask@
--     /must/ be a valid device mask
--
-- -   #VUID-VkAcquireNextImageInfoKHR-deviceMask-01291# @deviceMask@
--     /must/ not be zero
--
-- -   #VUID-VkAcquireNextImageInfoKHR-semaphore-03266# @semaphore@ /must/
--     have a 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAcquireNextImageInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR'
--
-- -   #VUID-VkAcquireNextImageInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkAcquireNextImageInfoKHR-swapchain-parameter# @swapchain@
--     /must/ be a valid 'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-VkAcquireNextImageInfoKHR-semaphore-parameter# If @semaphore@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @semaphore@ /must/
--     be a valid 'Vulkan.Core10.Handles.Semaphore' handle
--
-- -   #VUID-VkAcquireNextImageInfoKHR-fence-parameter# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ be a valid
--     'Vulkan.Core10.Handles.Fence' handle
--
-- -   #VUID-VkAcquireNextImageInfoKHR-commonparent# Each of @fence@,
--     @semaphore@, and @swapchain@ that are valid handles of non-ignored
--     parameters /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- -   Host access to @semaphore@ /must/ be externally synchronized
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.Fence', 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SwapchainKHR', 'acquireNextImage2KHR'
data AcquireNextImageInfoKHR = AcquireNextImageInfoKHR
  { -- | @swapchain@ is a non-retired swapchain from which an image is acquired.
    swapchain :: SwapchainKHR
  , -- | @timeout@ specifies how long the function waits, in nanoseconds, if no
    -- image is available.
    timeout :: Word64
  , -- | @semaphore@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a semaphore
    -- to signal.
    semaphore :: Semaphore
  , -- | @fence@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a fence to
    -- signal.
    fence :: Fence
  , -- | @deviceMask@ is a mask of physical devices for which the swapchain image
    -- will be ready to use when the semaphore or fence is signaled.
    deviceMask :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AcquireNextImageInfoKHR)
#endif
deriving instance Show AcquireNextImageInfoKHR

instance ToCStruct AcquireNextImageInfoKHR where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AcquireNextImageInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (swapchain)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (timeout)
    poke ((p `plusPtr` 32 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 40 :: Ptr Fence)) (fence)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (deviceMask)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    f

instance FromCStruct AcquireNextImageInfoKHR where
  peekCStruct p = do
    swapchain <- peek @SwapchainKHR ((p `plusPtr` 16 :: Ptr SwapchainKHR))
    timeout <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    semaphore <- peek @Semaphore ((p `plusPtr` 32 :: Ptr Semaphore))
    fence <- peek @Fence ((p `plusPtr` 40 :: Ptr Fence))
    deviceMask <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pure $ AcquireNextImageInfoKHR
             swapchain timeout semaphore fence deviceMask

instance Storable AcquireNextImageInfoKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AcquireNextImageInfoKHR where
  zero = AcquireNextImageInfoKHR
           zero
           zero
           zero
           zero
           zero


-- | VkDeviceGroupPresentInfoKHR - Mode and mask controlling which physical
-- devices\' images are presented
--
-- = Description
--
-- If @mode@ is 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR', then each
-- element of @pDeviceMasks@ selects which instance of the swapchain image
-- is presented. Each element of @pDeviceMasks@ /must/ have exactly one bit
-- set, and the corresponding physical device /must/ have a presentation
-- engine as reported by 'DeviceGroupPresentCapabilitiesKHR'.
--
-- If @mode@ is 'DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR', then each
-- element of @pDeviceMasks@ selects which instance of the swapchain image
-- is presented. Each element of @pDeviceMasks@ /must/ have exactly one bit
-- set, and some physical device in the logical device /must/ include that
-- bit in its 'DeviceGroupPresentCapabilitiesKHR'::@presentMask@.
--
-- If @mode@ is 'DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR', then each element
-- of @pDeviceMasks@ selects which instances of the swapchain image are
-- component-wise summed and the sum of those images is presented. If the
-- sum in any component is outside the representable range, the value of
-- that component is undefined. Each element of @pDeviceMasks@ /must/ have
-- a value for which all set bits are set in one of the elements of
-- 'DeviceGroupPresentCapabilitiesKHR'::@presentMask@.
--
-- If @mode@ is 'DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR',
-- then each element of @pDeviceMasks@ selects which instance(s) of the
-- swapchain images are presented. For each bit set in each element of
-- @pDeviceMasks@, the corresponding physical device /must/ have a
-- presentation engine as reported by 'DeviceGroupPresentCapabilitiesKHR'.
--
-- If 'DeviceGroupPresentInfoKHR' is not provided or @swapchainCount@ is
-- zero then the masks are considered to be @1@. If
-- 'DeviceGroupPresentInfoKHR' is not provided, @mode@ is considered to be
-- 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR'.
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceGroupPresentInfoKHR-swapchainCount-01297#
--     @swapchainCount@ /must/ equal @0@ or
--     'PresentInfoKHR'::@swapchainCount@
--
-- -   #VUID-VkDeviceGroupPresentInfoKHR-mode-01298# If @mode@ is
--     'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR', then each element of
--     @pDeviceMasks@ /must/ have exactly one bit set, and the
--     corresponding element of
--     'DeviceGroupPresentCapabilitiesKHR'::@presentMask@ /must/ be
--     non-zero
--
-- -   #VUID-VkDeviceGroupPresentInfoKHR-mode-01299# If @mode@ is
--     'DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR', then each element of
--     @pDeviceMasks@ /must/ have exactly one bit set, and some physical
--     device in the logical device /must/ include that bit in its
--     'DeviceGroupPresentCapabilitiesKHR'::@presentMask@
--
-- -   #VUID-VkDeviceGroupPresentInfoKHR-mode-01300# If @mode@ is
--     'DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR', then each element of
--     @pDeviceMasks@ /must/ have a value for which all set bits are set in
--     one of the elements of
--     'DeviceGroupPresentCapabilitiesKHR'::@presentMask@
--
-- -   #VUID-VkDeviceGroupPresentInfoKHR-mode-01301# If @mode@ is
--     'DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR', then for
--     each bit set in each element of @pDeviceMasks@, the corresponding
--     element of 'DeviceGroupPresentCapabilitiesKHR'::@presentMask@ /must/
--     be non-zero
--
-- -   #VUID-VkDeviceGroupPresentInfoKHR-pDeviceMasks-01302# The value of
--     each element of @pDeviceMasks@ /must/ be equal to the device mask
--     passed in 'AcquireNextImageInfoKHR'::@deviceMask@ when the image
--     index was last acquired
--
-- -   #VUID-VkDeviceGroupPresentInfoKHR-mode-01303# @mode@ /must/ have
--     exactly one bit set, and that bit /must/ have been included in
--     'DeviceGroupSwapchainCreateInfoKHR'::@modes@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceGroupPresentInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR'
--
-- -   #VUID-VkDeviceGroupPresentInfoKHR-pDeviceMasks-parameter# If
--     @swapchainCount@ is not @0@, @pDeviceMasks@ /must/ be a valid
--     pointer to an array of @swapchainCount@ @uint32_t@ values
--
-- -   #VUID-VkDeviceGroupPresentInfoKHR-mode-parameter# @mode@ /must/ be a
--     valid 'DeviceGroupPresentModeFlagBitsKHR' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'DeviceGroupPresentModeFlagBitsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupPresentInfoKHR = DeviceGroupPresentInfoKHR
  { -- | @pDeviceMasks@ is a pointer to an array of device masks, one for each
    -- element of 'PresentInfoKHR'::pSwapchains.
    deviceMasks :: Vector Word32
  , -- | @mode@ is a 'DeviceGroupPresentModeFlagBitsKHR' value specifying the
    -- device group present mode that will be used for this present.
    mode :: DeviceGroupPresentModeFlagBitsKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupPresentInfoKHR)
#endif
deriving instance Show DeviceGroupPresentInfoKHR

instance ToCStruct DeviceGroupPresentInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupPresentInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceMasks)) :: Word32))
    pPDeviceMasks' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (deviceMasks)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (deviceMasks)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPDeviceMasks')
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceGroupPresentModeFlagBitsKHR)) (mode)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 32 :: Ptr DeviceGroupPresentModeFlagBitsKHR)) (zero)
    f

instance FromCStruct DeviceGroupPresentInfoKHR where
  peekCStruct p = do
    swapchainCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pDeviceMasks <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pDeviceMasks' <- generateM (fromIntegral swapchainCount) (\i -> peek @Word32 ((pDeviceMasks `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    mode <- peek @DeviceGroupPresentModeFlagBitsKHR ((p `plusPtr` 32 :: Ptr DeviceGroupPresentModeFlagBitsKHR))
    pure $ DeviceGroupPresentInfoKHR
             pDeviceMasks' mode

instance Zero DeviceGroupPresentInfoKHR where
  zero = DeviceGroupPresentInfoKHR
           mempty
           zero


-- | VkDeviceGroupSwapchainCreateInfoKHR - Structure specifying parameters of
-- a newly created swapchain object
--
-- = Description
--
-- If this structure is not present, @modes@ is considered to be
-- 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'DeviceGroupPresentModeFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupSwapchainCreateInfoKHR = DeviceGroupSwapchainCreateInfoKHR
  { -- | @modes@ is a bitfield of modes that the swapchain /can/ be used with.
    --
    -- #VUID-VkDeviceGroupSwapchainCreateInfoKHR-modes-parameter# @modes@
    -- /must/ be a valid combination of 'DeviceGroupPresentModeFlagBitsKHR'
    -- values
    --
    -- #VUID-VkDeviceGroupSwapchainCreateInfoKHR-modes-requiredbitmask# @modes@
    -- /must/ not be @0@
    modes :: DeviceGroupPresentModeFlagsKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupSwapchainCreateInfoKHR)
#endif
deriving instance Show DeviceGroupSwapchainCreateInfoKHR

instance ToCStruct DeviceGroupSwapchainCreateInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupSwapchainCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceGroupPresentModeFlagsKHR)) (modes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceGroupPresentModeFlagsKHR)) (zero)
    f

instance FromCStruct DeviceGroupSwapchainCreateInfoKHR where
  peekCStruct p = do
    modes <- peek @DeviceGroupPresentModeFlagsKHR ((p `plusPtr` 16 :: Ptr DeviceGroupPresentModeFlagsKHR))
    pure $ DeviceGroupSwapchainCreateInfoKHR
             modes

instance Storable DeviceGroupSwapchainCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceGroupSwapchainCreateInfoKHR where
  zero = DeviceGroupSwapchainCreateInfoKHR
           zero


type DeviceGroupPresentModeFlagsKHR = DeviceGroupPresentModeFlagBitsKHR

-- | VkDeviceGroupPresentModeFlagBitsKHR - Bitmask specifying supported
-- device group present modes
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'DeviceGroupPresentInfoKHR', 'DeviceGroupPresentModeFlagsKHR'
newtype DeviceGroupPresentModeFlagBitsKHR = DeviceGroupPresentModeFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR' specifies that any physical
-- device with a presentation engine /can/ present its own swapchain
-- images.
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR              = DeviceGroupPresentModeFlagBitsKHR 0x00000001
-- | 'DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR' specifies that any physical
-- device with a presentation engine /can/ present swapchain images from
-- any physical device in its @presentMask@.
pattern DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR             = DeviceGroupPresentModeFlagBitsKHR 0x00000002
-- | 'DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR' specifies that any physical
-- device with a presentation engine /can/ present the sum of swapchain
-- images from any physical devices in its @presentMask@.
pattern DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR                = DeviceGroupPresentModeFlagBitsKHR 0x00000004
-- | 'DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR' specifies that
-- multiple physical devices with a presentation engine /can/ each present
-- their own swapchain images.
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR = DeviceGroupPresentModeFlagBitsKHR 0x00000008

conNameDeviceGroupPresentModeFlagBitsKHR :: String
conNameDeviceGroupPresentModeFlagBitsKHR = "DeviceGroupPresentModeFlagBitsKHR"

enumPrefixDeviceGroupPresentModeFlagBitsKHR :: String
enumPrefixDeviceGroupPresentModeFlagBitsKHR = "DEVICE_GROUP_PRESENT_MODE_"

showTableDeviceGroupPresentModeFlagBitsKHR :: [(DeviceGroupPresentModeFlagBitsKHR, String)]
showTableDeviceGroupPresentModeFlagBitsKHR =
  [ (DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR             , "LOCAL_BIT_KHR")
  , (DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR            , "REMOTE_BIT_KHR")
  , (DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR               , "SUM_BIT_KHR")
  , (DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR, "LOCAL_MULTI_DEVICE_BIT_KHR")
  ]

instance Show DeviceGroupPresentModeFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixDeviceGroupPresentModeFlagBitsKHR
                            showTableDeviceGroupPresentModeFlagBitsKHR
                            conNameDeviceGroupPresentModeFlagBitsKHR
                            (\(DeviceGroupPresentModeFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DeviceGroupPresentModeFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixDeviceGroupPresentModeFlagBitsKHR
                          showTableDeviceGroupPresentModeFlagBitsKHR
                          conNameDeviceGroupPresentModeFlagBitsKHR
                          DeviceGroupPresentModeFlagBitsKHR


type SwapchainCreateFlagsKHR = SwapchainCreateFlagBitsKHR

-- | VkSwapchainCreateFlagBitsKHR - Bitmask controlling swapchain creation
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>,
-- 'SwapchainCreateFlagsKHR'
newtype SwapchainCreateFlagBitsKHR = SwapchainCreateFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR' specifies that the images of
-- the swapchain /can/ be used to create a
-- 'Vulkan.Core10.Handles.ImageView' with a different format than what the
-- swapchain was created with. The list of allowed image view formats is
-- specified by adding a
-- 'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
-- structure to the @pNext@ chain of 'SwapchainCreateInfoKHR'. In addition,
-- this flag also specifies that the swapchain /can/ be created with usage
-- flags that are not supported for the format the swapchain is created
-- with but are supported for at least one of the allowed image view
-- formats.
pattern SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR              = SwapchainCreateFlagBitsKHR 0x00000004
-- | 'SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR' specifies that
-- images created from the swapchain (i.e. with the @swapchain@ member of
-- 'ImageSwapchainCreateInfoKHR' set to this swapchain’s handle) /must/ use
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT'.
pattern SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = SwapchainCreateFlagBitsKHR 0x00000001
-- | 'SWAPCHAIN_CREATE_PROTECTED_BIT_KHR' specifies that images created from
-- the swapchain are protected images.
pattern SWAPCHAIN_CREATE_PROTECTED_BIT_KHR                   = SwapchainCreateFlagBitsKHR 0x00000002

conNameSwapchainCreateFlagBitsKHR :: String
conNameSwapchainCreateFlagBitsKHR = "SwapchainCreateFlagBitsKHR"

enumPrefixSwapchainCreateFlagBitsKHR :: String
enumPrefixSwapchainCreateFlagBitsKHR = "SWAPCHAIN_CREATE_"

showTableSwapchainCreateFlagBitsKHR :: [(SwapchainCreateFlagBitsKHR, String)]
showTableSwapchainCreateFlagBitsKHR =
  [ (SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR             , "MUTABLE_FORMAT_BIT_KHR")
  , (SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR, "SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR")
  , (SWAPCHAIN_CREATE_PROTECTED_BIT_KHR                  , "PROTECTED_BIT_KHR")
  ]

instance Show SwapchainCreateFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixSwapchainCreateFlagBitsKHR
                            showTableSwapchainCreateFlagBitsKHR
                            conNameSwapchainCreateFlagBitsKHR
                            (\(SwapchainCreateFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SwapchainCreateFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixSwapchainCreateFlagBitsKHR
                          showTableSwapchainCreateFlagBitsKHR
                          conNameSwapchainCreateFlagBitsKHR
                          SwapchainCreateFlagBitsKHR


type KHR_SWAPCHAIN_SPEC_VERSION = 70

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_SPEC_VERSION"
pattern KHR_SWAPCHAIN_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SWAPCHAIN_SPEC_VERSION = 70


type KHR_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_swapchain"

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_EXTENSION_NAME"
pattern KHR_SWAPCHAIN_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_swapchain"

