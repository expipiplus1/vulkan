{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_drm_format_modifier - device extension
--
-- == VK_EXT_image_drm_format_modifier
--
-- [__Name String__]
--     @VK_EXT_image_drm_format_modifier@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     159
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_bind_memory2@
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
--     -   Requires @VK_KHR_image_format_list@
--
--     -   Requires @VK_KHR_sampler_ycbcr_conversion@
--
-- [__Contact__]
--
--     -   Chad Versace
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_image_drm_format_modifier:%20&body=@chadversary%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-08-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Antoine Labour, Google
--
--     -   Bas Nieuwenhuizen, Google
--
--     -   Chad Versace, Google
--
--     -   James Jones, NVIDIA
--
--     -   Jason Ekstrand, Intel
--
--     -   Jőrg Wagner, ARM
--
--     -   Kristian Høgsberg Kristensen, Google
--
--     -   Ray Smith, ARM
--
-- == Description
--
-- This extension provides the ability to use /DRM format modifiers/ with
-- images, enabling Vulkan to better integrate with the Linux ecosystem of
-- graphics, video, and display APIs.
--
-- Its functionality closely overlaps with
-- @EGL_EXT_image_dma_buf_import_modifiers@<VK_EXT_image_drm_format_modifier-fn2.html 2>\<\/link>^
-- and
-- @EGL_MESA_image_dma_buf_export@<VK_EXT_image_drm_format_modifier-fn3.html 3>\<\/link>^.
-- Unlike the EGL extensions, this extension does not require the use of a
-- specific handle type (such as a dma_buf) for external memory and
-- provides more explicit control of image creation.
--
-- == Introduction to DRM Format Modifiers
--
-- A /DRM format modifier/ is a 64-bit, vendor-prefixed, semi-opaque
-- unsigned integer. Most /modifiers/ represent a concrete, vendor-specific
-- tiling format for images. Some exceptions are @DRM_FORMAT_MOD_LINEAR@
-- (which is not vendor-specific); @DRM_FORMAT_MOD_NONE@ (which is an alias
-- of @DRM_FORMAT_MOD_LINEAR@ due to historical accident); and
-- @DRM_FORMAT_MOD_INVALID@ (which does not represent a tiling format). The
-- /modifier’s/ vendor prefix consists of the 8 most significant bits. The
-- canonical list of /modifiers/ and vendor prefixes is found in
-- <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/include/uapi/drm/drm_fourcc.h drm_fourcc.h>
-- in the Linux kernel source. The other dominant source of /modifiers/ are
-- vendor kernel trees.
--
-- One goal of /modifiers/ in the Linux ecosystem is to enumerate for each
-- vendor a reasonably sized set of tiling formats that are appropriate for
-- images shared across processes, APIs, and\/or devices, where each
-- participating component may possibly be from different vendors. A
-- non-goal is to enumerate all tiling formats supported by all vendors.
-- Some tiling formats used internally by vendors are inappropriate for
-- sharing; no /modifiers/ should be assigned to such tiling formats.
--
-- Modifier values typically do not /describe/ memory layouts. More
-- precisely, a /modifier/\'s lower 56 bits usually have no structure.
-- Instead, modifiers /name/ memory layouts; they name a small set of
-- vendor-preferred layouts for image sharing. As a consequence, in each
-- vendor namespace the modifier values are often sequentially allocated
-- starting at 1.
--
-- Each /modifier/ is usually supported by a single vendor and its name
-- matches the pattern @{VENDOR}_FORMAT_MOD_*@ or
-- @DRM_FORMAT_MOD_{VENDOR}_*@. Examples are @I915_FORMAT_MOD_X_TILED@ and
-- @DRM_FORMAT_MOD_BROADCOM_VC4_T_TILED@. An exception is
-- @DRM_FORMAT_MOD_LINEAR@, which is supported by most vendors.
--
-- Many APIs in Linux use /modifiers/ to negotiate and specify the memory
-- layout of shared images. For example, a Wayland compositor and Wayland
-- client may, by relaying /modifiers/ over the Wayland protocol
-- @zwp_linux_dmabuf_v1@, negotiate a vendor-specific tiling format for a
-- shared @wl_buffer@. The client may allocate the underlying memory for
-- the @wl_buffer@ with GBM, providing the chosen /modifier/ to
-- @gbm_bo_create_with_modifiers@. The client may then import the
-- @wl_buffer@ into Vulkan for producing image content, providing the
-- resource’s dma_buf to
-- 'Vulkan.Extensions.VK_KHR_external_memory_fd.ImportMemoryFdInfoKHR' and
-- its /modifier/ to 'ImageDrmFormatModifierExplicitCreateInfoEXT'. The
-- compositor may then import the @wl_buffer@ into OpenGL for sampling,
-- providing the resource’s dma_buf and /modifier/ to @eglCreateImage@. The
-- compositor may also bypass OpenGL and submit the @wl_buffer@ directly to
-- the kernel’s display API, providing the dma_buf and /modifier/ through
-- @drm_mode_fb_cmd2@.
--
-- == Format Translation
--
-- /Modifier/-capable APIs often pair /modifiers/ with DRM formats, which
-- are defined in
-- <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/include/uapi/drm/drm_fourcc.h drm_fourcc.h>.
-- However, @VK_EXT_image_drm_format_modifier@ uses
-- 'Vulkan.Core10.Enums.Format.Format' instead of DRM formats. The
-- application must convert between 'Vulkan.Core10.Enums.Format.Format' and
-- DRM format when it sends or receives a DRM format to or from an external
-- API.
--
-- The mapping from 'Vulkan.Core10.Enums.Format.Format' to DRM format is
-- lossy. Therefore, when receiving a DRM format from an external API,
-- often the application must use information from the external API to
-- accurately map the DRM format to a 'Vulkan.Core10.Enums.Format.Format'.
-- For example, DRM formats do not distinguish between RGB and sRGB (as of
-- 2018-03-28); external information is required to identify the image’s
-- colorspace.
--
-- The mapping between 'Vulkan.Core10.Enums.Format.Format' and DRM format
-- is also incomplete. For some DRM formats there exist no corresponding
-- Vulkan format, and for some Vulkan formats there exist no corresponding
-- DRM format.
--
-- == Usage Patterns
--
-- Three primary usage patterns are intended for this extension:
--
-- -   __Negotiation.__ The application negotiates with /modifier/-aware,
--     external components to determine sets of image creation parameters
--     supported among all components.
--
--     In the Linux ecosystem, the negotiation usually assumes the image is
--     a 2D, single-sampled, non-mipmapped, non-array image; this extension
--     permits that assumption but does not require it. The result of the
--     negotiation usually resembles a set of tuples such as /(drmFormat,
--     drmFormatModifier)/, where each participating component supports all
--     tuples in the set.
--
--     Many details of this negotiation—such as the protocol used during
--     negotiation, the set of image creation parameters expressable in the
--     protocol, and how the protocol chooses which process and which API
--     will create the image—are outside the scope of this specification.
--
--     In this extension,
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2'
--     with 'DrmFormatModifierPropertiesListEXT' serves a primary role
--     during the negotiation, and
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--     with 'PhysicalDeviceImageDrmFormatModifierInfoEXT' serves a
--     secondary role.
--
-- -   __Import.__ The application imports an image with a /modifier/.
--
--     In this pattern, the application receives from an external source
--     the image’s memory and its creation parameters, which are often the
--     result of the negotiation described above. Some image creation
--     parameters are implicitly defined by the external source; for
--     example, 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' is often
--     assumed. Some image creation parameters are usually explicit, such
--     as the image’s @format@, @drmFormatModifier@, and @extent@; and each
--     plane’s @offset@ and @rowPitch@.
--
--     Before creating the image, the application first verifies that the
--     physical device supports the received creation parameters by
--     querying
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2'
--     with 'DrmFormatModifierPropertiesListEXT' and
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--     with 'PhysicalDeviceImageDrmFormatModifierInfoEXT'. Then the
--     application creates the image by chaining
--     'ImageDrmFormatModifierExplicitCreateInfoEXT' and
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     onto 'Vulkan.Core10.Image.ImageCreateInfo'.
--
-- -   __Export.__ The application creates an image and allocates its
--     memory. Then the application exports to /modifier/-aware consumers
--     the image’s memory handles; its creation parameters; its /modifier/;
--     and the <VkSubresourceLayout.html offset>,
--     <VkSubresourceLayout.html size>, and
--     <VkSubresourceLayout.html rowPitch> of each /memory plane/.
--
--     In this pattern, the Vulkan device is the authority for the image;
--     it is the allocator of the image’s memory and the decider of the
--     image’s creation parameters. When choosing the image’s creation
--     parameters, the application usually chooses a tuple /(format,
--     drmFormatModifier)/ from the result of the negotiation described
--     above. The negotiation’s result often contains multiple tuples that
--     share the same format but differ in their /modifier/. In this case,
--     the application should defer the choice of the image’s /modifier/ to
--     the Vulkan implementation by providing all such /modifiers/ to
--     'ImageDrmFormatModifierListCreateInfoEXT'::@pDrmFormatModifiers@;
--     and the implementation should choose from @pDrmFormatModifiers@ the
--     optimal /modifier/ in consideration with the other image parameters.
--
--     The application creates the image by chaining
--     'ImageDrmFormatModifierListCreateInfoEXT' and
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     onto 'Vulkan.Core10.Image.ImageCreateInfo'. The protocol and APIs by
--     which the application will share the image with external consumers
--     will likely determine the value of
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@.
--     The implementation chooses for the image an optimal /modifier/ from
--     'ImageDrmFormatModifierListCreateInfoEXT'::@pDrmFormatModifiers@.
--     The application then queries the implementation-chosen /modifier/
--     with 'getImageDrmFormatModifierPropertiesEXT', and queries the
--     memory layout of each plane with
--     'Vulkan.Core10.Image.getImageSubresourceLayout'.
--
--     The application then allocates the image’s memory with
--     'Vulkan.Core10.Memory.MemoryAllocateInfo', adding chained extending
--     structures for external memory; binds it to the image; and exports
--     the memory, for example, with
--     'Vulkan.Extensions.VK_KHR_external_memory_fd.getMemoryFdKHR'.
--
--     Finally, the application sends the image’s creation parameters, its
--     /modifier/, its per-plane memory layout, and the exported memory
--     handle to the external consumers. The details of how the application
--     transmits this information to external consumers is outside the
--     scope of this specification.
--
-- == Prior Art
--
-- Extension
-- @EGL_EXT_image_dma_buf_import@<VK_EXT_image_drm_format_modifier-fn1.html 1>\<\/link>^
-- introduced the ability to create an @EGLImage@ by importing for each
-- plane a dma_buf, offset, and row pitch.
--
-- Later, extension
-- @EGL_EXT_image_dma_buf_import_modifiers@<VK_EXT_image_drm_format_modifier-fn2.html 2>\<\/link>^
-- introduced the ability to query which combination of formats and
-- /modifiers/ the implementation supports and to specify /modifiers/
-- during creation of the @EGLImage@.
--
-- Extension
-- @EGL_MESA_image_dma_buf_export@<VK_EXT_image_drm_format_modifier-fn3.html 3>\<\/link>^
-- is the inverse of @EGL_EXT_image_dma_buf_import_modifiers@.
--
-- The Linux kernel modesetting API (KMS), when configuring the display’s
-- framebuffer with @struct
-- drm_mode_fb_cmd2@<VK_EXT_image_drm_format_modifier-fn4.html 4>\<\/link>^,
-- allows one to specify the frambuffer’s /modifier/ as well as a per-plane
-- memory handle, offset, and row pitch.
--
-- GBM, a graphics buffer manager for Linux, allows creation of a @gbm_bo@
-- (that is, a graphics /buffer object/) by importing data similar to that
-- in
-- @EGL_EXT_image_dma_buf_import_modifiers@<VK_EXT_image_drm_format_modifier-fn1.html 1>\<\/link>^;
-- and symmetrically allows exporting the same data from the @gbm_bo@. See
-- the references to /modifier/ and /plane/ in
-- @gbm.h@<VK_EXT_image_drm_format_modifier-fn5.html 5>\<\/link>^.
--
-- == New Commands
--
-- -   'getImageDrmFormatModifierPropertiesEXT'
--
-- == New Structures
--
-- -   'DrmFormatModifierPropertiesEXT'
--
-- -   'ImageDrmFormatModifierPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2':
--
--     -   'DrmFormatModifierPropertiesListEXT'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'ImageDrmFormatModifierExplicitCreateInfoEXT'
--
--     -   'ImageDrmFormatModifierListCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2':
--
--     -   'PhysicalDeviceImageDrmFormatModifierInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageTiling.ImageTiling':
--
--     -   'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT'
--
-- == Issues
--
-- 1) Should this extension define a single DRM format modifier per
-- 'Vulkan.Core10.Handles.Image'? Or define one per plane?
--
-- +
--
-- __RESOLVED__: There exists a single DRM format modifier per
-- 'Vulkan.Core10.Handles.Image'.
--
-- __DISCUSSION__: Prior art, such as
-- @EGL_EXT_image_dma_buf_import_modifiers@<VK_EXT_image_drm_format_modifier-fn2.html 2>\<\/link>^,
-- @struct drm_mode_fb_cmd2@<VK_EXT_image_drm_format_modifier-fn4.html 4>\<\/link>^,
-- and @struct
-- gbm_import_fd_modifier_data@<VK_EXT_image_drm_format_modifier-fn5.html 5>\<\/link>^,
-- allows defining one /modifier/ per plane. However, developers of the GBM
-- and kernel APIs concede it was a mistake. Beginning in Linux 4.10, the
-- kernel requires that the application provide the same DRM format
-- /modifier/ for each plane. (See Linux commit
-- <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=bae781b259269590109e8a4a8227331362b88212 bae781b259269590109e8a4a8227331362b88212>).
-- And GBM provides an entry point, @gbm_bo_get_modifier@, for querying the
-- /modifier/ of the image but does not provide one to query the modifier
-- of individual planes.
--
-- 2) When creating an image with
-- 'ImageDrmFormatModifierExplicitCreateInfoEXT', which is typically used
-- when /importing/ an image, should the application explicitly provide the
-- size of each plane?
--
-- +
--
-- __RESOLVED__: No. The application /must/ not provide the size. To
-- enforce this, the API requires that
-- 'ImageDrmFormatModifierExplicitCreateInfoEXT'::@pPlaneLayouts->size@
-- /must/ be 0.
--
-- __DISCUSSION__: Prior art, such as
-- @EGL_EXT_image_dma_buf_import_modifiers@<VK_EXT_image_drm_format_modifier-fn2.html 2>\<\/link>^,
-- @struct drm_mode_fb_cmd2@<VK_EXT_image_drm_format_modifier-fn4.html 4>\<\/link>^,
-- and @struct
-- gbm_import_fd_modifier_data@<VK_EXT_image_drm_format_modifier-fn5.html 5>\<\/link>^,
-- omits from the API the size of each plane. Instead, the APIs infer each
-- plane’s size from the import parameters, which include the image’s pixel
-- format and a dma_buf, offset, and row pitch for each plane.
--
-- However, Vulkan differs from EGL and GBM with regards to image creation
-- in the following ways:
--
-- -   __Undedicated allocation by default.__ When importing or exporting a
--     set of dma_bufs as an @EGLImage@ or @gbm_bo@, common practice
--     mandates that each dma_buf’s memory be dedicated (in the sense of
--     @VK_KHR_dedicated_allocation@) to the image (though not necessarily
--     dedicated to a single plane). In particular, neither the GBM
--     documentation nor the EGL extension specifications explicitly state
--     this requirement, but in light of common practice this is likely due
--     to under-specification rather than intentional omission. In
--     contrast, @VK_EXT_image_drm_format_modifier@ permits, but does not
--     require, the implementation to require dedicated allocations for
--     images created with
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'.
--
-- -   __Separation of image creation and memory allocation.__ When
--     importing a set of dma_bufs as an @EGLImage@ or @gbm_bo@, EGL and
--     GBM create the image resource and bind it to memory (the dma_bufs)
--     simultaneously. This allows EGL and GBM to query each dma_buf’s size
--     during image creation. In Vulkan, image creation and memory
--     allocation are independent unless a dedicated allocation is used (as
--     in @VK_KHR_dedicated_allocation@). Therefore, without requiring
--     dedicated allocation, Vulkan cannot query the size of each dma_buf
--     (or other external handle) when calculating the image’s memory
--     layout. Even if dedication allocation were required, Vulkan cannot
--     calculate the image’s memory layout until after the image is bound
--     to its dma_ufs.
--
-- The above differences complicate the potential inference of plane size
-- in Vulkan. Consider the following problematic cases:
--
-- -   __Padding.__ Some plane of the image may require
--     implementation-dependent padding.
--
-- -   __Metadata.__ For some /modifiers/, the image may have a metadata
--     plane which requires a non-trivial calculation to determine its
--     size.
--
-- -   __Mipmapped, array, and 3D images.__ The implementation may support
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--     for images whose @mipLevels@, @arrayLayers@, or @depth@ is greater
--     than 1. For such images with certain /modifiers/, the calculation of
--     each plane’s size may be non-trivial.
--
-- However, an application-provided plane size solves none of the above
-- problems.
--
-- For simplicity, consider an external image with a single memory plane.
-- The implementation is obviously capable calculating the image’s size
-- when its tiling is
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'. Likewise, any
-- reasonable implementation is capable of calculating the image’s size
-- when its tiling uses a supported /modifier/.
--
-- Suppose that the external image’s size is smaller than the
-- implementation-calculated size. If the application provided the external
-- image’s size to 'Vulkan.Core10.Image.createImage', the implementation
-- would observe the mismatched size and recognize its inability to
-- comprehend the external image’s layout (unless the implementation used
-- the application-provided size to select a refinement of the tiling
-- layout indicated by the /modifier/, which is strongly discouraged). The
-- implementation would observe the conflict, and reject image creation
-- with
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT'.
-- On the other hand, if the application did not provide the external
-- image’s size to 'Vulkan.Core10.Image.createImage', then the application
-- would observe after calling
-- 'Vulkan.Core10.MemoryManagement.getImageMemoryRequirements' that the
-- external image’s size is less than the size required by the
-- implementation. The application would observe the conflict and refuse to
-- bind the 'Vulkan.Core10.Handles.Image' to the external memory. In both
-- cases, the result is explicit failure.
--
-- Suppose that the external image’s size is larger than the
-- implementation-calculated size. If the application provided the external
-- image’s size to 'Vulkan.Core10.Image.createImage', for reasons similar
-- to above the implementation would observe the mismatched size and
-- recognize its inability to comprehend the image data residing in the
-- extra size. The implementation, however, must assume that image data
-- resides in the entire size provided by the application. The
-- implementation would observe the conflict and reject image creation with
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT'.
-- On the other hand, if the application did not provide the external
-- image’s size to 'Vulkan.Core10.Image.createImage', then the application
-- would observe after calling
-- 'Vulkan.Core10.MemoryManagement.getImageMemoryRequirements' that the
-- external image’s size is larger than the implementation-usable size. The
-- application would observe the conflict and refuse to bind the
-- 'Vulkan.Core10.Handles.Image' to the external memory. In both cases, the
-- result is explicit failure.
--
-- Therefore, an application-provided size provides no benefit, and this
-- extension should not require it. This decision renders
-- 'Vulkan.Core10.Image.SubresourceLayout'::@size@ an unused field during
-- image creation, and thus introduces a risk that implementations may
-- require applications to submit sideband creation parameters in the
-- unused field. To prevent implementations from relying on sideband data,
-- this extension /requires/ the application to set @size@ to 0.
--
-- === References
--
-- 1.  #VK_EXT_image_drm_format_modifier-fn1#
--     <https://www.khronos.org/registry/EGL/extensions/EXT/EGL_EXT_image_dma_buf_import.txt EGL_EXT_image_dma_buf_import>
--
-- 2.  #VK_EXT_image_drm_format_modifier-fn2#
--     <https://www.khronos.org/registry/EGL/extensions/EXT/EGL_EXT_image_dma_buf_import_modifiers.txt EGL_EXT_image_dma_buf_import_modifiers>
--
-- 3.  #VK_EXT_image_drm_format_modifier-fn3#
--     <https://www.khronos.org/registry/EGL/extensions/MESA/EGL_MESA_image_dma_buf_export.txt EGL_MESA_image_dma_buf_export>
--
-- 4.  #VK_EXT_image_drm_format_modifier-fn4#
--     <https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/tree/include/uapi/drm/drm_mode.h?id=refs/tags/v4.10#n392 struct
--     drm_mode_fb_cmd2>
--
-- 5.  #VK_EXT_image_drm_format_modifier-fn5#
--     <https://cgit.freedesktop.org/mesa/mesa/tree/src/gbm/main/gbm.h?id=refs/tags/mesa-18.0.0-rc1 gbm.h>
--
-- === Version History
--
-- -   Revision 1, 2018-08-29 (Chad Versace)
--
--     -   First stable revision
--
-- = See Also
--
-- 'DrmFormatModifierPropertiesEXT', 'DrmFormatModifierPropertiesListEXT',
-- 'ImageDrmFormatModifierExplicitCreateInfoEXT',
-- 'ImageDrmFormatModifierListCreateInfoEXT',
-- 'ImageDrmFormatModifierPropertiesEXT',
-- 'PhysicalDeviceImageDrmFormatModifierInfoEXT',
-- 'getImageDrmFormatModifierPropertiesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_drm_format_modifier  ( getImageDrmFormatModifierPropertiesEXT
                                                           , DrmFormatModifierPropertiesListEXT(..)
                                                           , DrmFormatModifierPropertiesEXT(..)
                                                           , PhysicalDeviceImageDrmFormatModifierInfoEXT(..)
                                                           , ImageDrmFormatModifierListCreateInfoEXT(..)
                                                           , ImageDrmFormatModifierExplicitCreateInfoEXT(..)
                                                           , ImageDrmFormatModifierPropertiesEXT(..)
                                                           , EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION
                                                           , pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION
                                                           , EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME
                                                           , pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME
                                                           ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageDrmFormatModifierPropertiesEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SharingMode (SharingMode)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Image (SubresourceLayout)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageDrmFormatModifierPropertiesEXT
  :: FunPtr (Ptr Device_T -> Image -> Ptr ImageDrmFormatModifierPropertiesEXT -> IO Result) -> Ptr Device_T -> Image -> Ptr ImageDrmFormatModifierPropertiesEXT -> IO Result

-- | vkGetImageDrmFormatModifierPropertiesEXT - Returns an image’s DRM format
-- modifier
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
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Image',
-- 'ImageDrmFormatModifierPropertiesEXT'
getImageDrmFormatModifierPropertiesEXT :: forall io
                                        . (MonadIO io)
                                       => -- | @device@ is the logical device that owns the image.
                                          --
                                          -- #VUID-vkGetImageDrmFormatModifierPropertiesEXT-device-parameter#
                                          -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                          Device
                                       -> -- | @image@ is the queried image.
                                          --
                                          -- #VUID-vkGetImageDrmFormatModifierPropertiesEXT-image-02272# @image@
                                          -- /must/ have been created with <VkImageCreateInfo.html tiling> equal to
                                          -- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
                                          --
                                          -- #VUID-vkGetImageDrmFormatModifierPropertiesEXT-image-parameter# @image@
                                          -- /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
                                          --
                                          -- #VUID-vkGetImageDrmFormatModifierPropertiesEXT-image-parent# @image@
                                          -- /must/ have been created, allocated, or retrieved from @device@
                                          Image
                                       -> io (ImageDrmFormatModifierPropertiesEXT)
getImageDrmFormatModifierPropertiesEXT device image = liftIO . evalContT $ do
  let vkGetImageDrmFormatModifierPropertiesEXTPtr = pVkGetImageDrmFormatModifierPropertiesEXT (deviceCmds (device :: Device))
  lift $ unless (vkGetImageDrmFormatModifierPropertiesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageDrmFormatModifierPropertiesEXT is null" Nothing Nothing
  let vkGetImageDrmFormatModifierPropertiesEXT' = mkVkGetImageDrmFormatModifierPropertiesEXT vkGetImageDrmFormatModifierPropertiesEXTPtr
  pPProperties <- ContT (withZeroCStruct @ImageDrmFormatModifierPropertiesEXT)
  r <- lift $ traceAroundEvent "vkGetImageDrmFormatModifierPropertiesEXT" (vkGetImageDrmFormatModifierPropertiesEXT' (deviceHandle (device)) (image) (pPProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pProperties <- lift $ peekCStruct @ImageDrmFormatModifierPropertiesEXT pPProperties
  pure $ (pProperties)


-- | VkDrmFormatModifierPropertiesListEXT - Structure specifying the list of
-- DRM format modifiers supported for a format
--
-- = Description
--
-- If @pDrmFormatModifierProperties@ is @NULL@, then the function returns
-- in @drmFormatModifierCount@ the number of modifiers compatible with the
-- queried @format@. Otherwise, the application /must/ set
-- @drmFormatModifierCount@ to the length of the array
-- @pDrmFormatModifierProperties@; the function will write at most
-- @drmFormatModifierCount@ elements to the array, and will return in
-- @drmFormatModifierCount@ the number of elements written.
--
-- Among the elements in array @pDrmFormatModifierProperties@, each
-- returned @drmFormatModifier@ /must/ be unique.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DrmFormatModifierPropertiesEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DrmFormatModifierPropertiesListEXT = DrmFormatModifierPropertiesListEXT
  { -- | @drmFormatModifierCount@ is an inout parameter related to the number of
    -- modifiers compatible with the @format@, as described below.
    drmFormatModifierCount :: Word32
  , -- | @pDrmFormatModifierProperties@ is either @NULL@ or an array of
    -- 'DrmFormatModifierPropertiesEXT' structures.
    drmFormatModifierProperties :: Ptr DrmFormatModifierPropertiesEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrmFormatModifierPropertiesListEXT)
#endif
deriving instance Show DrmFormatModifierPropertiesListEXT

instance ToCStruct DrmFormatModifierPropertiesListEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrmFormatModifierPropertiesListEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (drmFormatModifierCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr DrmFormatModifierPropertiesEXT))) (drmFormatModifierProperties)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DrmFormatModifierPropertiesListEXT where
  peekCStruct p = do
    drmFormatModifierCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pDrmFormatModifierProperties <- peek @(Ptr DrmFormatModifierPropertiesEXT) ((p `plusPtr` 24 :: Ptr (Ptr DrmFormatModifierPropertiesEXT)))
    pure $ DrmFormatModifierPropertiesListEXT
             drmFormatModifierCount pDrmFormatModifierProperties

instance Storable DrmFormatModifierPropertiesListEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrmFormatModifierPropertiesListEXT where
  zero = DrmFormatModifierPropertiesListEXT
           zero
           zero


-- | VkDrmFormatModifierPropertiesEXT - Structure specifying properties of a
-- format when combined with a DRM format modifier
--
-- = Description
--
-- The returned @drmFormatModifierTilingFeatures@ /must/ contain at least
-- one bit.
--
-- The implementation /must/ not return @DRM_FORMAT_MOD_INVALID@ in
-- @drmFormatModifier@.
--
-- An image’s /memory planecount/ (as returned by
-- @drmFormatModifierPlaneCount@) is distinct from its /format planecount/
-- (in the sense of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
-- Y′CBCR formats). In
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags', each
-- @VK_IMAGE_ASPECT_MEMORY_PLANE@//i/_BIT_EXT represents a _memory plane/
-- and each @VK_IMAGE_ASPECT_PLANE@//i/_BIT a _format plane/.
--
-- An image’s set of /format planes/ is an ordered partition of the image’s
-- __content__ into separable groups of format channels. The ordered
-- partition is encoded in the name of each
-- 'Vulkan.Core10.Enums.Format.Format'. For example,
-- 'Vulkan.Core10.Enums.Format.FORMAT_G8_B8R8_2PLANE_420_UNORM' contains
-- two /format planes/; the first plane contains the green channel and the
-- second plane contains the blue channel and red channel. If the format
-- name does not contain @PLANE@, then the format contains a single plane;
-- for example, 'Vulkan.Core10.Enums.Format.FORMAT_R8G8B8A8_UNORM'. Some
-- commands, such as
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage', do not
-- operate on all format channels in the image, but instead operate only on
-- the /format planes/ explicitly chosen by the application and operate on
-- each /format plane/ independently.
--
-- An image’s set of /memory planes/ is an ordered partition of the image’s
-- __memory__ rather than the image’s __content__. Each /memory plane/ is a
-- contiguous range of memory. The union of an image’s /memory planes/ is
-- not necessarily contiguous.
--
-- If an image is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-linear-resource linear>,
-- then the partition is the same for /memory planes/ and for /format
-- planes/. Therefore, if the returned @drmFormatModifier@ is
-- @DRM_FORMAT_MOD_LINEAR@, then @drmFormatModifierPlaneCount@ /must/ equal
-- the /format planecount/, and @drmFormatModifierTilingFeatures@ /must/ be
-- identical to the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2'::@linearTilingFeatures@
-- returned in the same @pNext@ chain.
--
-- If an image is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-linear-resource non-linear>,
-- then the partition of the image’s __memory__ into /memory planes/ is
-- implementation-specific and /may/ be unrelated to the partition of the
-- image’s __content__ into /format planes/. For example, consider an image
-- whose @format@ is
-- 'Vulkan.Core10.Enums.Format.FORMAT_G8_B8_R8_3PLANE_420_UNORM', @tiling@
-- is
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
-- whose @drmFormatModifier@ is not @DRM_FORMAT_MOD_LINEAR@, and @flags@
-- lacks
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'. The
-- image has 3 /format planes/, and commands such
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage' act on each
-- /format plane/ independently as if the data of each /format plane/ were
-- separable from the data of the other planes. In a straightforward
-- implementation, the implementation /may/ store the image’s content in 3
-- adjacent /memory planes/ where each /memory plane/ corresponds exactly
-- to a /format plane/. However, the implementation /may/ also store the
-- image’s content in a single /memory plane/ where all format channels are
-- combined using an implementation-private block-compressed format; or the
-- implementation /may/ store the image’s content in a collection of 7
-- adjacent /memory planes/ using an implementation-private sharding
-- technique. Because the image is non-linear and non-disjoint, the
-- implementation has much freedom when choosing the image’s placement in
-- memory.
--
-- The /memory planecount/ applies to function parameters and structures
-- only when the API specifies an explicit requirement on
-- @drmFormatModifierPlaneCount@. In all other cases, the /memory
-- planecount/ is ignored.
--
-- = See Also
--
-- 'DrmFormatModifierPropertiesListEXT',
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlags'
data DrmFormatModifierPropertiesEXT = DrmFormatModifierPropertiesEXT
  { -- | @drmFormatModifier@ is a /Linux DRM format modifier/.
    drmFormatModifier :: Word64
  , -- | @drmFormatModifierPlaneCount@ is the number of /memory planes/ in any
    -- image created with @format@ and @drmFormatModifier@. An image’s /memory
    -- planecount/ is distinct from its /format planecount/, as explained
    -- below.
    drmFormatModifierPlaneCount :: Word32
  , -- | @drmFormatModifierTilingFeatures@ is a bitmask of
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits' that
    -- are supported by any image created with @format@ and
    -- @drmFormatModifier@.
    drmFormatModifierTilingFeatures :: FormatFeatureFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrmFormatModifierPropertiesEXT)
#endif
deriving instance Show DrmFormatModifierPropertiesEXT

instance ToCStruct DrmFormatModifierPropertiesEXT where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrmFormatModifierPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word64)) (drmFormatModifier)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (drmFormatModifierPlaneCount)
    poke ((p `plusPtr` 12 :: Ptr FormatFeatureFlags)) (drmFormatModifierTilingFeatures)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr FormatFeatureFlags)) (zero)
    f

instance FromCStruct DrmFormatModifierPropertiesEXT where
  peekCStruct p = do
    drmFormatModifier <- peek @Word64 ((p `plusPtr` 0 :: Ptr Word64))
    drmFormatModifierPlaneCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    drmFormatModifierTilingFeatures <- peek @FormatFeatureFlags ((p `plusPtr` 12 :: Ptr FormatFeatureFlags))
    pure $ DrmFormatModifierPropertiesEXT
             drmFormatModifier drmFormatModifierPlaneCount drmFormatModifierTilingFeatures

instance Storable DrmFormatModifierPropertiesEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrmFormatModifierPropertiesEXT where
  zero = DrmFormatModifierPropertiesEXT
           zero
           zero
           zero


-- | VkPhysicalDeviceImageDrmFormatModifierInfoEXT - Structure specifying a
-- DRM format modifier as image creation parameter
--
-- = Description
--
-- If the @drmFormatModifier@ is incompatible with the parameters specified
-- in
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
-- and its @pNext@ chain, then
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
-- returns 'Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'. The
-- implementation /must/ support the query of any @drmFormatModifier@,
-- including unknown and invalid modifier values.
--
-- == Valid Usage
--
-- -   #VUID-VkPhysicalDeviceImageDrmFormatModifierInfoEXT-sharingMode-02314#
--     If @sharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT', then
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values
--
-- -   #VUID-VkPhysicalDeviceImageDrmFormatModifierInfoEXT-sharingMode-02315#
--     If @sharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT', then
--     @queueFamilyIndexCount@ /must/ be greater than @1@
--
-- -   #VUID-VkPhysicalDeviceImageDrmFormatModifierInfoEXT-sharingMode-02316#
--     If @sharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT', each
--     element of @pQueueFamilyIndices@ /must/ be unique and /must/ be less
--     than the @pQueueFamilyPropertyCount@ returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2'
--     for the @physicalDevice@ that was used to create @device@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceImageDrmFormatModifierInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT'
--
-- -   #VUID-VkPhysicalDeviceImageDrmFormatModifierInfoEXT-sharingMode-parameter#
--     @sharingMode@ /must/ be a valid
--     'Vulkan.Core10.Enums.SharingMode.SharingMode' value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.SharingMode.SharingMode',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageDrmFormatModifierInfoEXT = PhysicalDeviceImageDrmFormatModifierInfoEXT
  { -- | @drmFormatModifier@ is the image’s /Linux DRM format modifier/,
    -- corresponding to
    -- 'ImageDrmFormatModifierExplicitCreateInfoEXT'::@modifier@ or to
    -- 'ImageDrmFormatModifierListCreateInfoEXT'::@pModifiers@.
    drmFormatModifier :: Word64
  , -- | @sharingMode@ specifies how the image will be accessed by multiple queue
    -- families.
    sharingMode :: SharingMode
  , -- | @pQueueFamilyIndices@ is a list of queue families that will access the
    -- image (ignored if @sharingMode@ is not
    -- 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT').
    queueFamilyIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageDrmFormatModifierInfoEXT)
#endif
deriving instance Show PhysicalDeviceImageDrmFormatModifierInfoEXT

instance ToCStruct PhysicalDeviceImageDrmFormatModifierInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageDrmFormatModifierInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word64)) (drmFormatModifier)
    lift $ poke ((p `plusPtr` 24 :: Ptr SharingMode)) (sharingMode)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueFamilyIndices)) :: Word32))
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (queueFamilyIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (queueFamilyIndices)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr SharingMode)) (zero)
    f

instance FromCStruct PhysicalDeviceImageDrmFormatModifierInfoEXT where
  peekCStruct p = do
    drmFormatModifier <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    sharingMode <- peek @SharingMode ((p `plusPtr` 24 :: Ptr SharingMode))
    queueFamilyIndexCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pQueueFamilyIndices <- peek @(Ptr Word32) ((p `plusPtr` 32 :: Ptr (Ptr Word32)))
    pQueueFamilyIndices' <- generateM (fromIntegral queueFamilyIndexCount) (\i -> peek @Word32 ((pQueueFamilyIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ PhysicalDeviceImageDrmFormatModifierInfoEXT
             drmFormatModifier sharingMode pQueueFamilyIndices'

instance Zero PhysicalDeviceImageDrmFormatModifierInfoEXT where
  zero = PhysicalDeviceImageDrmFormatModifierInfoEXT
           zero
           zero
           mempty


-- | VkImageDrmFormatModifierListCreateInfoEXT - Specify that an image must
-- be created with a DRM format modifier from the provided list
--
-- == Valid Usage
--
-- -   #VUID-VkImageDrmFormatModifierListCreateInfoEXT-pDrmFormatModifiers-02263#
--     Each /modifier/ in @pDrmFormatModifiers@ /must/ be compatible with
--     the parameters in 'Vulkan.Core10.Image.ImageCreateInfo' and its
--     @pNext@ chain, as determined by querying
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
--     extended with 'PhysicalDeviceImageDrmFormatModifierInfoEXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageDrmFormatModifierListCreateInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT'
--
-- -   #VUID-VkImageDrmFormatModifierListCreateInfoEXT-pDrmFormatModifiers-parameter#
--     @pDrmFormatModifiers@ /must/ be a valid pointer to an array of
--     @drmFormatModifierCount@ @uint64_t@ values
--
-- -   #VUID-VkImageDrmFormatModifierListCreateInfoEXT-drmFormatModifierCount-arraylength#
--     @drmFormatModifierCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageDrmFormatModifierListCreateInfoEXT = ImageDrmFormatModifierListCreateInfoEXT
  { -- | @pDrmFormatModifiers@ is a pointer to an array of /Linux DRM format
    -- modifiers/.
    drmFormatModifiers :: Vector Word64 }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageDrmFormatModifierListCreateInfoEXT)
#endif
deriving instance Show ImageDrmFormatModifierListCreateInfoEXT

instance ToCStruct ImageDrmFormatModifierListCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageDrmFormatModifierListCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (drmFormatModifiers)) :: Word32))
    pPDrmFormatModifiers' <- ContT $ allocaBytesAligned @Word64 ((Data.Vector.length (drmFormatModifiers)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDrmFormatModifiers' `plusPtr` (8 * (i)) :: Ptr Word64) (e)) (drmFormatModifiers)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word64))) (pPDrmFormatModifiers')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ImageDrmFormatModifierListCreateInfoEXT where
  peekCStruct p = do
    drmFormatModifierCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pDrmFormatModifiers <- peek @(Ptr Word64) ((p `plusPtr` 24 :: Ptr (Ptr Word64)))
    pDrmFormatModifiers' <- generateM (fromIntegral drmFormatModifierCount) (\i -> peek @Word64 ((pDrmFormatModifiers `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pure $ ImageDrmFormatModifierListCreateInfoEXT
             pDrmFormatModifiers'

instance Zero ImageDrmFormatModifierListCreateInfoEXT where
  zero = ImageDrmFormatModifierListCreateInfoEXT
           mempty


-- | VkImageDrmFormatModifierExplicitCreateInfoEXT - Specify that an image be
-- created with the provided DRM format modifier and explicit memory layout
--
-- = Description
--
-- The @i@th member of @pPlaneLayouts@ describes the layout of the image’s
-- @i@th /memory plane/ (that is,
-- @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@). In each element of
-- @pPlaneLayouts@, the implementation /must/ ignore @size@. The
-- implementation calculates the size of each plane, which the application
-- /can/ query with 'Vulkan.Core10.Image.getImageSubresourceLayout'.
--
-- When creating an image with
-- 'ImageDrmFormatModifierExplicitCreateInfoEXT', it is the application’s
-- responsibility to satisfy all valid usage requirements. However, the
-- implementation /must/ validate that the provided @pPlaneLayouts@, when
-- combined with the provided @drmFormatModifier@ and other creation
-- parameters in 'Vulkan.Core10.Image.ImageCreateInfo' and its @pNext@
-- chain, produce a valid image. (This validation is necessarily
-- implementation-dependent and outside the scope of Vulkan, and therefore
-- not described by valid usage requirements). If this validation fails,
-- then 'Vulkan.Core10.Image.createImage' returns
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT'.
--
-- == Valid Usage
--
-- -   #VUID-VkImageDrmFormatModifierExplicitCreateInfoEXT-drmFormatModifier-02264#
--     @drmFormatModifier@ /must/ be compatible with the parameters in
--     'Vulkan.Core10.Image.ImageCreateInfo' and its @pNext@ chain, as
--     determined by querying
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
--     extended with 'PhysicalDeviceImageDrmFormatModifierInfoEXT'
--
-- -   #VUID-VkImageDrmFormatModifierExplicitCreateInfoEXT-drmFormatModifierPlaneCount-02265#
--     @drmFormatModifierPlaneCount@ /must/ be equal to the
--     'DrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
--     associated with 'Vulkan.Core10.Image.ImageCreateInfo'::@format@ and
--     @drmFormatModifier@, as found by querying
--     'DrmFormatModifierPropertiesListEXT'
--
-- -   #VUID-VkImageDrmFormatModifierExplicitCreateInfoEXT-size-02267# For
--     each element of @pPlaneLayouts@, @size@ /must/ be 0
--
-- -   #VUID-VkImageDrmFormatModifierExplicitCreateInfoEXT-arrayPitch-02268#
--     For each element of @pPlaneLayouts@, @arrayPitch@ /must/ be 0 if
--     'Vulkan.Core10.Image.ImageCreateInfo'::@arrayLayers@ is 1
--
-- -   #VUID-VkImageDrmFormatModifierExplicitCreateInfoEXT-depthPitch-02269#
--     For each element of @pPlaneLayouts@, @depthPitch@ /must/ be 0 if
--     'Vulkan.Core10.Image.ImageCreateInfo'::@extent.depth@ is 1
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageDrmFormatModifierExplicitCreateInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT'
--
-- -   #VUID-VkImageDrmFormatModifierExplicitCreateInfoEXT-pPlaneLayouts-parameter#
--     If @drmFormatModifierPlaneCount@ is not @0@, @pPlaneLayouts@ /must/
--     be a valid pointer to an array of @drmFormatModifierPlaneCount@
--     'Vulkan.Core10.Image.SubresourceLayout' structures
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.Image.SubresourceLayout'
data ImageDrmFormatModifierExplicitCreateInfoEXT = ImageDrmFormatModifierExplicitCreateInfoEXT
  { -- | @drmFormatModifier@ is the /Linux DRM format modifier/ with which the
    -- image will be created.
    drmFormatModifier :: Word64
  , -- | @pPlaneLayouts@ is a pointer to an array of
    -- 'Vulkan.Core10.Image.SubresourceLayout' structures describing the
    -- image’s /memory planes/.
    planeLayouts :: Vector SubresourceLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageDrmFormatModifierExplicitCreateInfoEXT)
#endif
deriving instance Show ImageDrmFormatModifierExplicitCreateInfoEXT

instance ToCStruct ImageDrmFormatModifierExplicitCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageDrmFormatModifierExplicitCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word64)) (drmFormatModifier)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (planeLayouts)) :: Word32))
    pPPlaneLayouts' <- ContT $ allocaBytesAligned @SubresourceLayout ((Data.Vector.length (planeLayouts)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPlaneLayouts' `plusPtr` (40 * (i)) :: Ptr SubresourceLayout) (e)) (planeLayouts)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr SubresourceLayout))) (pPPlaneLayouts')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    f

instance FromCStruct ImageDrmFormatModifierExplicitCreateInfoEXT where
  peekCStruct p = do
    drmFormatModifier <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    drmFormatModifierPlaneCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pPlaneLayouts <- peek @(Ptr SubresourceLayout) ((p `plusPtr` 32 :: Ptr (Ptr SubresourceLayout)))
    pPlaneLayouts' <- generateM (fromIntegral drmFormatModifierPlaneCount) (\i -> peekCStruct @SubresourceLayout ((pPlaneLayouts `advancePtrBytes` (40 * (i)) :: Ptr SubresourceLayout)))
    pure $ ImageDrmFormatModifierExplicitCreateInfoEXT
             drmFormatModifier pPlaneLayouts'

instance Zero ImageDrmFormatModifierExplicitCreateInfoEXT where
  zero = ImageDrmFormatModifierExplicitCreateInfoEXT
           zero
           mempty


-- | VkImageDrmFormatModifierPropertiesEXT - Properties of an image’s Linux
-- DRM format modifier
--
-- = Description
--
-- If the @image@ was created with
-- 'ImageDrmFormatModifierListCreateInfoEXT', then the returned
-- @drmFormatModifier@ /must/ belong to the list of modifiers provided at
-- time of image creation in
-- 'ImageDrmFormatModifierListCreateInfoEXT'::@pDrmFormatModifiers@. If the
-- @image@ was created with 'ImageDrmFormatModifierExplicitCreateInfoEXT',
-- then the returned @drmFormatModifier@ /must/ be the modifier provided at
-- time of image creation in
-- 'ImageDrmFormatModifierExplicitCreateInfoEXT'::@drmFormatModifier@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageDrmFormatModifierPropertiesEXT'
data ImageDrmFormatModifierPropertiesEXT = ImageDrmFormatModifierPropertiesEXT
  { -- | @drmFormatModifier@ returns the image’s
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-drm-format-modifier Linux DRM format modifier>.
    drmFormatModifier :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageDrmFormatModifierPropertiesEXT)
#endif
deriving instance Show ImageDrmFormatModifierPropertiesEXT

instance ToCStruct ImageDrmFormatModifierPropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageDrmFormatModifierPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (drmFormatModifier)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    f

instance FromCStruct ImageDrmFormatModifierPropertiesEXT where
  peekCStruct p = do
    drmFormatModifier <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    pure $ ImageDrmFormatModifierPropertiesEXT
             drmFormatModifier

instance Storable ImageDrmFormatModifierPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageDrmFormatModifierPropertiesEXT where
  zero = ImageDrmFormatModifierPropertiesEXT
           zero


type EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION"
pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION = 1


type EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME = "VK_EXT_image_drm_format_modifier"

-- No documentation found for TopLevel "VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME"
pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME = "VK_EXT_image_drm_format_modifier"

