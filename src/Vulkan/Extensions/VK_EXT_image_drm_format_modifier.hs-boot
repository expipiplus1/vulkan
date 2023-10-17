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
--     2
--
-- [__Extension and Version Dependencies__]
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_bind_memory2 VK_KHR_bind_memory2>
--              and
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--              and
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_sampler_ycbcr_conversion VK_KHR_sampler_ycbcr_conversion>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_image_format_list VK_KHR_image_format_list>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Version 1.2>
--
-- [__Contact__]
--
--     -   Lina Versace
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_drm_format_modifier] @versalinyaa%0A*Here describe the issue or question you have about the VK_EXT_image_drm_format_modifier extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-09-30
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
--     -   Lina Versace, Google
--
--     -   James Jones, NVIDIA
--
--     -   Faith Ekstrand, Intel
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
-- @EGL_EXT_image_dma_buf_import_modifiers@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn2 2>^
-- and
-- @EGL_MESA_image_dma_buf_export@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn3 3>^.
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
--     Many details of this negotiation - such as the protocol used during
--     negotiation, the set of image creation parameters expressible in the
--     protocol, and how the protocol chooses which process and which API
--     will create the image - are outside the scope of this specification.
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
-- @EGL_EXT_image_dma_buf_import@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn1 1>^
-- introduced the ability to create an @EGLImage@ by importing for each
-- plane a dma_buf, offset, and row pitch.
--
-- Later, extension
-- @EGL_EXT_image_dma_buf_import_modifiers@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn2 2>^
-- introduced the ability to query which combination of formats and
-- /modifiers/ the implementation supports and to specify /modifiers/
-- during creation of the @EGLImage@.
--
-- Extension
-- @EGL_MESA_image_dma_buf_export@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn3 3>^
-- is the inverse of @EGL_EXT_image_dma_buf_import_modifiers@.
--
-- The Linux kernel modesetting API (KMS), when configuring the display’s
-- framebuffer with
-- @struct drm_mode_fb_cmd2@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn4 4>^,
-- allows one to specify the framebuffer’s /modifier/ as well as a
-- per-plane memory handle, offset, and row pitch.
--
-- GBM, a graphics buffer manager for Linux, allows creation of a @gbm_bo@
-- (that is, a graphics /buffer object/) by importing data similar to that
-- in
-- @EGL_EXT_image_dma_buf_import_modifiers@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn1 1>^;
-- and symmetrically allows exporting the same data from the @gbm_bo@. See
-- the references to /modifier/ and /plane/ in
-- @gbm.h@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn5 5>^.
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
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
-- is supported:
--
-- -   'DrmFormatModifierProperties2EXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2':
--
--     -   'DrmFormatModifierPropertiesList2EXT'
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
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_2_EXT'
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
-- @EGL_EXT_image_dma_buf_import_modifiers@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn2 2>^,
-- @struct drm_mode_fb_cmd2@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn4 4>^,
-- and
-- @struct gbm_import_fd_modifier_data@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn5 5>^,
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
-- @EGL_EXT_image_dma_buf_import_modifiers@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn2 2>^,
-- @struct drm_mode_fb_cmd2@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn4 4>^,
-- and
-- @struct gbm_import_fd_modifier_data@<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier-fn5 5>^,
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
--     <https://registry.khronos.org/EGL/extensions/EXT/EGL_EXT_image_dma_buf_import.txt EGL_EXT_image_dma_buf_import>
--
-- 2.  #VK_EXT_image_drm_format_modifier-fn2#
--     <https://registry.khronos.org/EGL/extensions/EXT/EGL_EXT_image_dma_buf_import_modifiers.txt EGL_EXT_image_dma_buf_import_modifiers>
--
-- 3.  #VK_EXT_image_drm_format_modifier-fn3#
--     <https://registry.khronos.org/EGL/extensions/MESA/EGL_MESA_image_dma_buf_export.txt EGL_MESA_image_dma_buf_export>
--
-- 4.  #VK_EXT_image_drm_format_modifier-fn4#
--     <https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/tree/include/uapi/drm/drm_mode.h?id=refs/tags/v4.10#n392 struct drm_mode_fb_cmd2>
--
-- 5.  #VK_EXT_image_drm_format_modifier-fn5#
--     <https://cgit.freedesktop.org/mesa/mesa/tree/src/gbm/main/gbm.h?id=refs/tags/mesa-18.0.0-rc1 gbm.h>
--
-- === Version History
--
-- -   Revision 1, 2018-08-29 (Lina Versace)
--
--     -   First stable revision
--
-- -   Revision 2, 2021-09-30 (Jon Leech)
--
--     -   Add interaction with @VK_KHR_format_feature_flags2@ to @vk.xml@
--
-- == See Also
--
-- 'DrmFormatModifierPropertiesEXT', 'DrmFormatModifierPropertiesListEXT',
-- 'ImageDrmFormatModifierExplicitCreateInfoEXT',
-- 'ImageDrmFormatModifierListCreateInfoEXT',
-- 'ImageDrmFormatModifierPropertiesEXT',
-- 'PhysicalDeviceImageDrmFormatModifierInfoEXT',
-- 'getImageDrmFormatModifierPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_drm_format_modifier Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_drm_format_modifier  ( DrmFormatModifierProperties2EXT
                                                           , DrmFormatModifierPropertiesEXT
                                                           , DrmFormatModifierPropertiesList2EXT
                                                           , DrmFormatModifierPropertiesListEXT
                                                           , ImageDrmFormatModifierExplicitCreateInfoEXT
                                                           , ImageDrmFormatModifierListCreateInfoEXT
                                                           , ImageDrmFormatModifierPropertiesEXT
                                                           , PhysicalDeviceImageDrmFormatModifierInfoEXT
                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DrmFormatModifierProperties2EXT

instance ToCStruct DrmFormatModifierProperties2EXT
instance Show DrmFormatModifierProperties2EXT

instance FromCStruct DrmFormatModifierProperties2EXT


data DrmFormatModifierPropertiesEXT

instance ToCStruct DrmFormatModifierPropertiesEXT
instance Show DrmFormatModifierPropertiesEXT

instance FromCStruct DrmFormatModifierPropertiesEXT


data DrmFormatModifierPropertiesList2EXT

instance ToCStruct DrmFormatModifierPropertiesList2EXT
instance Show DrmFormatModifierPropertiesList2EXT

instance FromCStruct DrmFormatModifierPropertiesList2EXT


data DrmFormatModifierPropertiesListEXT

instance ToCStruct DrmFormatModifierPropertiesListEXT
instance Show DrmFormatModifierPropertiesListEXT

instance FromCStruct DrmFormatModifierPropertiesListEXT


data ImageDrmFormatModifierExplicitCreateInfoEXT

instance ToCStruct ImageDrmFormatModifierExplicitCreateInfoEXT
instance Show ImageDrmFormatModifierExplicitCreateInfoEXT

instance FromCStruct ImageDrmFormatModifierExplicitCreateInfoEXT


data ImageDrmFormatModifierListCreateInfoEXT

instance ToCStruct ImageDrmFormatModifierListCreateInfoEXT
instance Show ImageDrmFormatModifierListCreateInfoEXT

instance FromCStruct ImageDrmFormatModifierListCreateInfoEXT


data ImageDrmFormatModifierPropertiesEXT

instance ToCStruct ImageDrmFormatModifierPropertiesEXT
instance Show ImageDrmFormatModifierPropertiesEXT

instance FromCStruct ImageDrmFormatModifierPropertiesEXT


data PhysicalDeviceImageDrmFormatModifierInfoEXT

instance ToCStruct PhysicalDeviceImageDrmFormatModifierInfoEXT
instance Show PhysicalDeviceImageDrmFormatModifierInfoEXT

instance FromCStruct PhysicalDeviceImageDrmFormatModifierInfoEXT

