{-# language CPP #-}
-- No documentation found for Chapter "Sampler"
module Vulkan.Core10.Sampler  ( createSampler
                              , withSampler
                              , destroySampler
                              , SamplerCreateInfo(..)
                              , Sampler(..)
                              , BorderColor(..)
                              , Filter(..)
                              , SamplerMipmapMode(..)
                              , SamplerAddressMode(..)
                              , SamplerCreateFlagBits(..)
                              , SamplerCreateFlags
                              ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.BorderColor (BorderColor)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Enums.CompareOp (CompareOp)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateSampler))
import Vulkan.Dynamic (DeviceCmds(pVkDestroySampler))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Filter (Filter)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Sampler)
import Vulkan.Core10.Handles (Sampler(..))
import Vulkan.Core10.Enums.SamplerAddressMode (SamplerAddressMode)
import Vulkan.Core10.Enums.SamplerCreateFlagBits (SamplerCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_custom_border_color (SamplerCustomBorderColorCreateInfoEXT)
import Vulkan.Core10.Enums.SamplerMipmapMode (SamplerMipmapMode)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax (SamplerReductionModeCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionInfo)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.BorderColor (BorderColor(..))
import Vulkan.Core10.Enums.Filter (Filter(..))
import Vulkan.Core10.Handles (Sampler(..))
import Vulkan.Core10.Enums.SamplerAddressMode (SamplerAddressMode(..))
import Vulkan.Core10.Enums.SamplerCreateFlagBits (SamplerCreateFlagBits(..))
import Vulkan.Core10.Enums.SamplerCreateFlagBits (SamplerCreateFlags)
import Vulkan.Core10.Enums.SamplerMipmapMode (SamplerMipmapMode(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSampler
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct SamplerCreateInfo) -> Ptr AllocationCallbacks -> Ptr Sampler -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct SamplerCreateInfo) -> Ptr AllocationCallbacks -> Ptr Sampler -> IO Result

-- | vkCreateSampler - Create a new sampler object
--
-- == Valid Usage
--
-- -   #VUID-vkCreateSampler-maxSamplerAllocationCount-04110# There /must/
--     be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxSamplerAllocationCount@
--     VkSampler objects currently created on the device.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateSampler-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateSampler-pCreateInfo-parameter# @pCreateInfo@ /must/ be
--     a valid pointer to a valid 'SamplerCreateInfo' structure
--
-- -   #VUID-vkCreateSampler-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateSampler-pSampler-parameter# @pSampler@ /must/ be a
--     valid pointer to a 'Vulkan.Core10.Handles.Sampler' handle
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
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Sampler',
-- 'SamplerCreateInfo'
createSampler :: forall a io
               . (Extendss SamplerCreateInfo a, PokeChain a, MonadIO io)
              => -- | @device@ is the logical device that creates the sampler.
                 Device
              -> -- | @pCreateInfo@ is a pointer to a 'SamplerCreateInfo' structure specifying
                 -- the state of the sampler object.
                 (SamplerCreateInfo a)
              -> -- | @pAllocator@ controls host memory allocation as described in the
                 -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                 -- chapter.
                 ("allocator" ::: Maybe AllocationCallbacks)
              -> io (Sampler)
createSampler device createInfo allocator = liftIO . evalContT $ do
  let vkCreateSamplerPtr = pVkCreateSampler (deviceCmds (device :: Device))
  lift $ unless (vkCreateSamplerPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateSampler is null" Nothing Nothing
  let vkCreateSampler' = mkVkCreateSampler vkCreateSamplerPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSampler <- ContT $ bracket (callocBytes @Sampler 8) free
  r <- lift $ traceAroundEvent "vkCreateSampler" (vkCreateSampler' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPSampler))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSampler <- lift $ peek @Sampler pPSampler
  pure $ (pSampler)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSampler' and 'destroySampler'
--
-- To ensure that 'destroySampler' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSampler :: forall a io r . (Extendss SamplerCreateInfo a, PokeChain a, MonadIO io) => Device -> SamplerCreateInfo a -> Maybe AllocationCallbacks -> (io Sampler -> (Sampler -> io ()) -> r) -> r
withSampler device pCreateInfo pAllocator b =
  b (createSampler device pCreateInfo pAllocator)
    (\(o0) -> destroySampler device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySampler
  :: FunPtr (Ptr Device_T -> Sampler -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Sampler -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroySampler - Destroy a sampler object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroySampler-sampler-01082# All submitted commands that
--     refer to @sampler@ /must/ have completed execution
--
-- -   #VUID-vkDestroySampler-sampler-01083# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @sampler@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroySampler-sampler-01084# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @sampler@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroySampler-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroySampler-sampler-parameter# If @sampler@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @sampler@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Sampler' handle
--
-- -   #VUID-vkDestroySampler-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroySampler-sampler-parent# If @sampler@ is a valid
--     handle, it /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @sampler@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Sampler'
destroySampler :: forall io
                . (MonadIO io)
               => -- | @device@ is the logical device that destroys the sampler.
                  Device
               -> -- | @sampler@ is the sampler to destroy.
                  Sampler
               -> -- | @pAllocator@ controls host memory allocation as described in the
                  -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                  -- chapter.
                  ("allocator" ::: Maybe AllocationCallbacks)
               -> io ()
destroySampler device sampler allocator = liftIO . evalContT $ do
  let vkDestroySamplerPtr = pVkDestroySampler (deviceCmds (device :: Device))
  lift $ unless (vkDestroySamplerPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroySampler is null" Nothing Nothing
  let vkDestroySampler' = mkVkDestroySampler vkDestroySamplerPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroySampler" (vkDestroySampler' (deviceHandle (device)) (sampler) pAllocator)
  pure $ ()


-- | VkSamplerCreateInfo - Structure specifying parameters of a newly created
-- sampler
--
-- = Description
--
-- Mapping of OpenGL to Vulkan filter modes
--
-- @magFilter@ values of 'Vulkan.Core10.Enums.Filter.FILTER_NEAREST' and
-- 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' directly correspond to
-- @GL_NEAREST@ and @GL_LINEAR@ magnification filters. @minFilter@ and
-- @mipmapMode@ combine to correspond to the similarly named OpenGL
-- minification filter of @GL_minFilter_MIPMAP_mipmapMode@ (e.g.
-- @minFilter@ of 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
-- @mipmapMode@ of
-- 'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_NEAREST'
-- correspond to @GL_LINEAR_MIPMAP_NEAREST@).
--
-- There are no Vulkan filter modes that directly correspond to OpenGL
-- minification filters of @GL_LINEAR@ or @GL_NEAREST@, but they /can/ be
-- emulated using
-- 'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_NEAREST',
-- @minLod@ = 0, and @maxLod@ = 0.25, and using @minFilter@ =
-- 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' or @minFilter@ =
-- 'Vulkan.Core10.Enums.Filter.FILTER_NEAREST', respectively.
--
-- Note that using a @maxLod@ of zero would cause
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-texel-filtering magnification>
-- to always be performed, and the @magFilter@ to always be used. This is
-- valid, just not an exact match for OpenGL behavior. Clamping the maximum
-- LOD to 0.25 allows the λ value to be non-zero and minification to be
-- performed, while still always rounding down to the base level. If the
-- @minFilter@ and @magFilter@ are equal, then using a @maxLod@ of zero
-- also works.
--
-- The maximum number of sampler objects which /can/ be simultaneously
-- created on a device is implementation-dependent and specified by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxSamplerAllocationCount maxSamplerAllocationCount>
-- member of the 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'
-- structure.
--
-- Note
--
-- For historical reasons, if @maxSamplerAllocationCount@ is exceeded, some
-- implementations may return
-- 'Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'. Exceeding this
-- limit will result in undefined behavior, and an application should not
-- rely on the use of the returned error code in order to identify when the
-- limit is reached.
--
-- Since 'Vulkan.Core10.Handles.Sampler' is a non-dispatchable handle type,
-- implementations /may/ return the same handle for sampler state vectors
-- that are identical. In such cases, all such objects would only count
-- once against the @maxSamplerAllocationCount@ limit.
--
-- == Valid Usage
--
-- -   #VUID-VkSamplerCreateInfo-mipLodBias-01069# The absolute value of
--     @mipLodBias@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxSamplerLodBias@
--
-- -   #VUID-VkSamplerCreateInfo-samplerMipLodBias-04467# If the
--     @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@samplerMipLodBias@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', @mipLodBias@ /must/ be
--     zero.
--
-- -   #VUID-VkSamplerCreateInfo-maxLod-01973# @maxLod@ /must/ be greater
--     than or equal to @minLod@
--
-- -   #VUID-VkSamplerCreateInfo-anisotropyEnable-01070# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-samplerAnisotropy anisotropic sampling>
--     feature is not enabled, @anisotropyEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkSamplerCreateInfo-anisotropyEnable-01071# If
--     @anisotropyEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     @maxAnisotropy@ /must/ be between @1.0@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxSamplerAnisotropy@,
--     inclusive
--
-- -   #VUID-VkSamplerCreateInfo-minFilter-01645# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
--     is enabled and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#potential-format-features potential format features>
--     of the sampler Y′CBCR conversion do not support
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT',
--     @minFilter@ and @magFilter@ /must/ be equal to the sampler Y′CBCR
--     conversion’s @chromaFilter@
--
-- -   #VUID-VkSamplerCreateInfo-unnormalizedCoordinates-01072# If
--     @unnormalizedCoordinates@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     @minFilter@ and @magFilter@ /must/ be equal
--
-- -   #VUID-VkSamplerCreateInfo-unnormalizedCoordinates-01073# If
--     @unnormalizedCoordinates@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     @mipmapMode@ /must/ be
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_NEAREST'
--
-- -   #VUID-VkSamplerCreateInfo-unnormalizedCoordinates-01074# If
--     @unnormalizedCoordinates@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     @minLod@ and @maxLod@ /must/ be zero
--
-- -   #VUID-VkSamplerCreateInfo-unnormalizedCoordinates-01075# If
--     @unnormalizedCoordinates@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     @addressModeU@ and @addressModeV@ /must/ each be either
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--     or
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER'
--
-- -   #VUID-VkSamplerCreateInfo-unnormalizedCoordinates-01076# If
--     @unnormalizedCoordinates@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     @anisotropyEnable@ /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkSamplerCreateInfo-unnormalizedCoordinates-01077# If
--     @unnormalizedCoordinates@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     @compareEnable@ /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkSamplerCreateInfo-addressModeU-01078# If any of
--     @addressModeU@, @addressModeV@ or @addressModeW@ are
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER',
--     @borderColor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BorderColor.BorderColor' value
--
-- -   #VUID-VkSamplerCreateInfo-addressModeU-01646# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
--     is enabled, @addressModeU@, @addressModeV@, and @addressModeW@
--     /must/ be
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE',
--     @anisotropyEnable@ /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE',
--     and @unnormalizedCoordinates@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkSamplerCreateInfo-None-01647# The sampler reduction mode
--     /must/ be set to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE'
--     if
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
--     is enabled
--
-- -   #VUID-VkSamplerCreateInfo-addressModeU-01079# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-samplerMirrorClampToEdge samplerMirrorClampToEdge>
--     is not enabled, and if the @VK_KHR_sampler_mirror_clamp_to_edge@
--     extension is not enabled, @addressModeU@, @addressModeV@ and
--     @addressModeW@ /must/ not be
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE'
--
-- -   #VUID-VkSamplerCreateInfo-compareEnable-01080# If @compareEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', @compareOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.CompareOp.CompareOp' value
--
-- -   #VUID-VkSamplerCreateInfo-magFilter-01081# If either @magFilter@ or
--     @minFilter@ is
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT',
--     @anisotropyEnable@ /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkSamplerCreateInfo-compareEnable-01423# If @compareEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the @reductionMode@ member of
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'
--     /must/ be
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE'
--
-- -   #VUID-VkSamplerCreateInfo-flags-02574# If @flags@ includes
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @minFilter@ and @magFilter@ /must/ be equal
--
-- -   #VUID-VkSamplerCreateInfo-flags-02575# If @flags@ includes
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @mipmapMode@ /must/ be
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_NEAREST'
--
-- -   #VUID-VkSamplerCreateInfo-flags-02576# If @flags@ includes
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @minLod@ and @maxLod@ /must/ be zero
--
-- -   #VUID-VkSamplerCreateInfo-flags-02577# If @flags@ includes
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @addressModeU@ and @addressModeV@ /must/ each be either
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--     or
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER'
--
-- -   #VUID-VkSamplerCreateInfo-flags-02578# If @flags@ includes
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @anisotropyEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkSamplerCreateInfo-flags-02579# If @flags@ includes
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @compareEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkSamplerCreateInfo-flags-02580# If @flags@ includes
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @unnormalizedCoordinates@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkSamplerCreateInfo-borderColor-04011# If @borderColor@ is one
--     of 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT'
--     or 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT',
--     then a
--     'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT'
--     /must/ be present in the @pNext@ chain
--
-- -   #VUID-VkSamplerCreateInfo-customBorderColors-04085# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-customBorderColors customBorderColors>
--     feature is not enabled, @borderColor@ /must/ not be
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT' or
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
--
-- -   #VUID-VkSamplerCreateInfo-borderColor-04442# If @borderColor@ is one
--     of 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT'
--     or 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT',
--     and
--     'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT'::@format@
--     is not 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED',
--     'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT'::@customBorderColor@
--     /must/ be within the range of values representable in @format@.
--
-- -   #VUID-VkSamplerCreateInfo-None-04012# The maximum number of samplers
--     with custom border colors which /can/ be simultaneously created on a
--     device is implementation-dependent and specified by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxCustomBorderColorSamplers maxCustomBorderColorSamplers>
--     member of the
--     'Vulkan.Extensions.VK_EXT_custom_border_color.PhysicalDeviceCustomBorderColorPropertiesEXT'
--     structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSamplerCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_CREATE_INFO'
--
-- -   #VUID-VkSamplerCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT',
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo',
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--
-- -   #VUID-VkSamplerCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkSamplerCreateInfo-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlagBits'
--     values
--
-- -   #VUID-VkSamplerCreateInfo-magFilter-parameter# @magFilter@ /must/ be
--     a valid 'Vulkan.Core10.Enums.Filter.Filter' value
--
-- -   #VUID-VkSamplerCreateInfo-minFilter-parameter# @minFilter@ /must/ be
--     a valid 'Vulkan.Core10.Enums.Filter.Filter' value
--
-- -   #VUID-VkSamplerCreateInfo-mipmapMode-parameter# @mipmapMode@ /must/
--     be a valid 'Vulkan.Core10.Enums.SamplerMipmapMode.SamplerMipmapMode'
--     value
--
-- -   #VUID-VkSamplerCreateInfo-addressModeU-parameter# @addressModeU@
--     /must/ be a valid
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' value
--
-- -   #VUID-VkSamplerCreateInfo-addressModeV-parameter# @addressModeV@
--     /must/ be a valid
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' value
--
-- -   #VUID-VkSamplerCreateInfo-addressModeW-parameter# @addressModeW@
--     /must/ be a valid
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' value
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.BorderColor.BorderColor',
-- 'Vulkan.Core10.Enums.CompareOp.CompareOp',
-- 'Vulkan.Core10.Enums.Filter.Filter',
-- 'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode',
-- 'Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlags',
-- 'Vulkan.Core10.Enums.SamplerMipmapMode.SamplerMipmapMode',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createSampler'
data SamplerCreateInfo (es :: [Type]) = SamplerCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlagBits'
    -- describing additional parameters of the sampler.
    flags :: SamplerCreateFlags
  , -- | @magFilter@ is a 'Vulkan.Core10.Enums.Filter.Filter' value specifying
    -- the magnification filter to apply to lookups.
    magFilter :: Filter
  , -- | @minFilter@ is a 'Vulkan.Core10.Enums.Filter.Filter' value specifying
    -- the minification filter to apply to lookups.
    minFilter :: Filter
  , -- | @mipmapMode@ is a
    -- 'Vulkan.Core10.Enums.SamplerMipmapMode.SamplerMipmapMode' value
    -- specifying the mipmap filter to apply to lookups.
    mipmapMode :: SamplerMipmapMode
  , -- | @addressModeU@ is a
    -- 'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' value
    -- specifying the addressing mode for outside [0..1] range for U
    -- coordinate.
    addressModeU :: SamplerAddressMode
  , -- | @addressModeV@ is a
    -- 'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' value
    -- specifying the addressing mode for outside [0..1] range for V
    -- coordinate.
    addressModeV :: SamplerAddressMode
  , -- | @addressModeW@ is a
    -- 'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' value
    -- specifying the addressing mode for outside [0..1] range for W
    -- coordinate.
    addressModeW :: SamplerAddressMode
  , -- | #samplers-mipLodBias# @mipLodBias@ is the bias to be added to mipmap LOD
    -- (level-of-detail) calculation and bias provided by image sampling
    -- functions in SPIR-V, as described in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-level-of-detail-operation Level-of-Detail Operation>
    -- section.
    mipLodBias :: Float
  , -- | #samplers-maxAnisotropy# @anisotropyEnable@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' to enable anisotropic filtering,
    -- as described in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-texel-anisotropic-filtering Texel Anisotropic Filtering>
    -- section, or 'Vulkan.Core10.FundamentalTypes.FALSE' otherwise.
    anisotropyEnable :: Bool
  , -- | @maxAnisotropy@ is the anisotropy value clamp used by the sampler when
    -- @anisotropyEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE'. If
    -- @anisotropyEnable@ is 'Vulkan.Core10.FundamentalTypes.FALSE',
    -- @maxAnisotropy@ is ignored.
    maxAnisotropy :: Float
  , -- | @compareEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' to enable
    -- comparison against a reference value during lookups, or
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' otherwise.
    --
    -- -   Note: Some implementations will default to shader state if this
    --     member does not match.
    compareEnable :: Bool
  , -- | @compareOp@ is a 'Vulkan.Core10.Enums.CompareOp.CompareOp' value
    -- specifying the comparison function to apply to fetched data before
    -- filtering as described in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation Depth Compare Operation>
    -- section.
    compareOp :: CompareOp
  , -- | @minLod@ is used to clamp the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-level-of-detail-operation minimum of the computed LOD value>.
    minLod :: Float
  , -- | @maxLod@ is used to clamp the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-level-of-detail-operation maximum of the computed LOD value>.
    -- To avoid clamping the maximum value, set @maxLod@ to the constant
    -- 'Vulkan.Core10.APIConstants.LOD_CLAMP_NONE'.
    maxLod :: Float
  , -- | @borderColor@ is a 'Vulkan.Core10.Enums.BorderColor.BorderColor' value
    -- specifying the predefined border color to use.
    borderColor :: BorderColor
  , -- | #samplers-unnormalizedCoordinates# @unnormalizedCoordinates@ controls
    -- whether to use unnormalized or normalized texel coordinates to address
    -- texels of the image. When set to 'Vulkan.Core10.FundamentalTypes.TRUE',
    -- the range of the image coordinates used to lookup the texel is in the
    -- range of zero to the image dimensions for x, y and z. When set to
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' the range of image coordinates is
    -- zero to one.
    --
    -- When @unnormalizedCoordinates@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
    -- images the sampler is used with in the shader have the following
    -- requirements:
    --
    -- -   The @viewType@ /must/ be either
    --     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D' or
    --     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'.
    --
    -- -   The image view /must/ have a single layer and a single mip level.
    --
    -- When @unnormalizedCoordinates@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
    -- image built-in functions in the shader that use the sampler have the
    -- following requirements:
    --
    -- -   The functions /must/ not use projection.
    --
    -- -   The functions /must/ not use offsets.
    unnormalizedCoordinates :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SamplerCreateInfo es)

instance Extensible SamplerCreateInfo where
  extensibleTypeName = "SamplerCreateInfo"
  setNext x next = x{next = next}
  getNext SamplerCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SamplerCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SamplerCustomBorderColorCreateInfoEXT = Just f
    | Just Refl <- eqT @e @SamplerReductionModeCreateInfo = Just f
    | Just Refl <- eqT @e @SamplerYcbcrConversionInfo = Just f
    | otherwise = Nothing

instance (Extendss SamplerCreateInfo es, PokeChain es) => ToCStruct (SamplerCreateInfo es) where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SamplerCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SamplerCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Filter)) (magFilter)
    lift $ poke ((p `plusPtr` 24 :: Ptr Filter)) (minFilter)
    lift $ poke ((p `plusPtr` 28 :: Ptr SamplerMipmapMode)) (mipmapMode)
    lift $ poke ((p `plusPtr` 32 :: Ptr SamplerAddressMode)) (addressModeU)
    lift $ poke ((p `plusPtr` 36 :: Ptr SamplerAddressMode)) (addressModeV)
    lift $ poke ((p `plusPtr` 40 :: Ptr SamplerAddressMode)) (addressModeW)
    lift $ poke ((p `plusPtr` 44 :: Ptr CFloat)) (CFloat (mipLodBias))
    lift $ poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (anisotropyEnable))
    lift $ poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (maxAnisotropy))
    lift $ poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (compareEnable))
    lift $ poke ((p `plusPtr` 60 :: Ptr CompareOp)) (compareOp)
    lift $ poke ((p `plusPtr` 64 :: Ptr CFloat)) (CFloat (minLod))
    lift $ poke ((p `plusPtr` 68 :: Ptr CFloat)) (CFloat (maxLod))
    lift $ poke ((p `plusPtr` 72 :: Ptr BorderColor)) (borderColor)
    lift $ poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (unnormalizedCoordinates))
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Filter)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Filter)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr SamplerMipmapMode)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr SamplerAddressMode)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr SamplerAddressMode)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr SamplerAddressMode)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 60 :: Ptr CompareOp)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 68 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 72 :: Ptr BorderColor)) (zero)
    lift $ poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance (Extendss SamplerCreateInfo es, PeekChain es) => FromCStruct (SamplerCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @SamplerCreateFlags ((p `plusPtr` 16 :: Ptr SamplerCreateFlags))
    magFilter <- peek @Filter ((p `plusPtr` 20 :: Ptr Filter))
    minFilter <- peek @Filter ((p `plusPtr` 24 :: Ptr Filter))
    mipmapMode <- peek @SamplerMipmapMode ((p `plusPtr` 28 :: Ptr SamplerMipmapMode))
    addressModeU <- peek @SamplerAddressMode ((p `plusPtr` 32 :: Ptr SamplerAddressMode))
    addressModeV <- peek @SamplerAddressMode ((p `plusPtr` 36 :: Ptr SamplerAddressMode))
    addressModeW <- peek @SamplerAddressMode ((p `plusPtr` 40 :: Ptr SamplerAddressMode))
    mipLodBias <- peek @CFloat ((p `plusPtr` 44 :: Ptr CFloat))
    anisotropyEnable <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    maxAnisotropy <- peek @CFloat ((p `plusPtr` 52 :: Ptr CFloat))
    compareEnable <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    compareOp <- peek @CompareOp ((p `plusPtr` 60 :: Ptr CompareOp))
    minLod <- peek @CFloat ((p `plusPtr` 64 :: Ptr CFloat))
    maxLod <- peek @CFloat ((p `plusPtr` 68 :: Ptr CFloat))
    borderColor <- peek @BorderColor ((p `plusPtr` 72 :: Ptr BorderColor))
    unnormalizedCoordinates <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    pure $ SamplerCreateInfo
             next flags magFilter minFilter mipmapMode addressModeU addressModeV addressModeW (coerce @CFloat @Float mipLodBias) (bool32ToBool anisotropyEnable) (coerce @CFloat @Float maxAnisotropy) (bool32ToBool compareEnable) compareOp (coerce @CFloat @Float minLod) (coerce @CFloat @Float maxLod) borderColor (bool32ToBool unnormalizedCoordinates)

instance es ~ '[] => Zero (SamplerCreateInfo es) where
  zero = SamplerCreateInfo
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
           zero
           zero
           zero
           zero
           zero
           zero
           zero

