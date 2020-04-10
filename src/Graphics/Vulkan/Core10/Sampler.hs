{-# language CPP #-}
module Graphics.Vulkan.Core10.Sampler  ( createSampler
                                       , withSampler
                                       , destroySampler
                                       , SamplerCreateInfo(..)
                                       ) where

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.Enums.BorderColor (BorderColor)
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.Core10.Enums.CompareOp (CompareOp)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateSampler))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroySampler))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.Core10.Enums.Filter (Filter)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Handles (Sampler)
import Graphics.Vulkan.Core10.Handles (Sampler(..))
import Graphics.Vulkan.Core10.Enums.SamplerAddressMode (SamplerAddressMode)
import Graphics.Vulkan.Core10.Enums.SamplerCreateFlagBits (SamplerCreateFlags)
import Graphics.Vulkan.Core10.Enums.SamplerMipmapMode (SamplerMipmapMode)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax (SamplerReductionModeCreateInfo)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionInfo)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSampler
  :: FunPtr (Ptr Device_T -> Ptr (SamplerCreateInfo a) -> Ptr AllocationCallbacks -> Ptr Sampler -> IO Result) -> Ptr Device_T -> Ptr (SamplerCreateInfo a) -> Ptr AllocationCallbacks -> Ptr Sampler -> IO Result

-- | vkCreateSampler - Create a new sampler object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the sampler.
--
-- -   @pCreateInfo@ is a pointer to a 'SamplerCreateInfo' structure
--     specifying the state of the sampler object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pSampler@ is a pointer to a
--     'Graphics.Vulkan.Core10.Handles.Sampler' handle in which the
--     resulting sampler object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'SamplerCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pSampler@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core10.Handles.Sampler' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.Sampler', 'SamplerCreateInfo'
createSampler :: forall a io . (PokeChain a, MonadIO io) => Device -> SamplerCreateInfo a -> ("allocator" ::: Maybe AllocationCallbacks) -> io (Sampler)
createSampler device createInfo allocator = liftIO . evalContT $ do
  let vkCreateSampler' = mkVkCreateSampler (pVkCreateSampler (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSampler <- ContT $ bracket (callocBytes @Sampler 8) free
  r <- lift $ vkCreateSampler' (deviceHandle (device)) pCreateInfo pAllocator (pPSampler)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSampler <- lift $ peek @Sampler pPSampler
  pure $ (pSampler)

-- | A safe wrapper for 'createSampler' and 'destroySampler' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withSampler :: forall a r . PokeChain a => Device -> SamplerCreateInfo a -> Maybe AllocationCallbacks -> ((Sampler) -> IO r) -> IO r
withSampler device pCreateInfo pAllocator =
  bracket
    (createSampler device pCreateInfo pAllocator)
    (\(o0) -> destroySampler device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySampler
  :: FunPtr (Ptr Device_T -> Sampler -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Sampler -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroySampler - Destroy a sampler object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the sampler.
--
-- -   @sampler@ is the sampler to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @sampler@ /must/ have completed
--     execution
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @sampler@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @sampler@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   If @sampler@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @sampler@ /must/
--     be a valid 'Graphics.Vulkan.Core10.Handles.Sampler' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   If @sampler@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @sampler@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.Sampler'
destroySampler :: forall io . MonadIO io => Device -> Sampler -> ("allocator" ::: Maybe AllocationCallbacks) -> io ()
destroySampler device sampler allocator = liftIO . evalContT $ do
  let vkDestroySampler' = mkVkDestroySampler (pVkDestroySampler (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroySampler' (deviceHandle (device)) (sampler) pAllocator
  pure $ ()


-- | VkSamplerCreateInfo - Structure specifying parameters of a newly created
-- sampler
--
-- = Description
--
-- Mapping of OpenGL to Vulkan filter modes
--
-- @magFilter@ values of
-- 'Graphics.Vulkan.Core10.Enums.Filter.FILTER_NEAREST' and
-- 'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR' directly correspond
-- to @GL_NEAREST@ and @GL_LINEAR@ magnification filters. @minFilter@ and
-- @mipmapMode@ combine to correspond to the similarly named OpenGL
-- minification filter of @GL_minFilter_MIPMAP_mipmapMode@ (e.g.
-- @minFilter@ of 'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
-- @mipmapMode@ of
-- 'Graphics.Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_NEAREST'
-- correspond to @GL_LINEAR_MIPMAP_NEAREST@).
--
-- There are no Vulkan filter modes that directly correspond to OpenGL
-- minification filters of @GL_LINEAR@ or @GL_NEAREST@, but they /can/ be
-- emulated using
-- 'Graphics.Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_NEAREST',
-- @minLod@ = 0, and @maxLod@ = 0.25, and using @minFilter@ =
-- 'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR' or @minFilter@ =
-- 'Graphics.Vulkan.Core10.Enums.Filter.FILTER_NEAREST', respectively.
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
-- member of the
-- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'
-- structure. If @maxSamplerAllocationCount@ is exceeded, 'createSampler'
-- will return
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'.
--
-- Since 'Graphics.Vulkan.Core10.Handles.Sampler' is a non-dispatchable
-- handle type, implementations /may/ return the same handle for sampler
-- state vectors that are identical. In such cases, all such objects would
-- only count once against the @maxSamplerAllocationCount@ limit.
--
-- == Valid Usage
--
-- -   The absolute value of @mipLodBias@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxSamplerLodBias@
--
-- -   @maxLod@ /must/ be greater than or equal to @minLod@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-samplerAnisotropy anisotropic sampling>
--     feature is not enabled, @anisotropyEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   If @anisotropyEnable@ is 'Graphics.Vulkan.Core10.BaseType.TRUE',
--     @maxAnisotropy@ /must/ be between @1.0@ and
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxSamplerAnisotropy@,
--     inclusive
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
--     is enabled and
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT'
--     is not set for the format, @minFilter@ and @magFilter@ /must/ be
--     equal to the sampler Y′CBCR conversion’s @chromaFilter@
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', @minFilter@ and @magFilter@
--     /must/ be equal
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', @mipmapMode@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_NEAREST'
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', @minLod@ and @maxLod@ /must/
--     be zero
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', @addressModeU@ and
--     @addressModeV@ /must/ each be either
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--     or
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER'
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', @anisotropyEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   If @unnormalizedCoordinates@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', @compareEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   If any of @addressModeU@, @addressModeV@ or @addressModeW@ are
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER',
--     @borderColor@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.BorderColor.BorderColor' value
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
--     is enabled, @addressModeU@, @addressModeV@, and @addressModeW@
--     /must/ be
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE',
--     @anisotropyEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE', and
--     @unnormalizedCoordinates@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   The sampler reduction mode /must/ be set to
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE'
--     if
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
--     is enabled
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-samplerMirrorClampToEdge samplerMirrorClampToEdge>
--     is not enabled, and if the
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_KHR_sampler_mirror_clamp_to_edge@
--     extension is not enabled, @addressModeU@, @addressModeV@ and
--     @addressModeW@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE'
--
-- -   If @compareEnable@ is 'Graphics.Vulkan.Core10.BaseType.TRUE',
--     @compareOp@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.CompareOp.CompareOp' value
--
-- -   If either @magFilter@ or @minFilter@ is
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT',
--     @anisotropyEnable@ /must/ be 'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   If @compareEnable@ is 'Graphics.Vulkan.Core10.BaseType.TRUE', the
--     @reductionMode@ member of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'
--     /must/ be
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE'
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @minFilter@ and @magFilter@ /must/ be equal.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @mipmapMode@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_NEAREST'.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @minLod@ and @maxLod@ /must/ be zero.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @addressModeU@ and @addressModeV@ /must/ each be either
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--     or
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER'.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @anisotropyEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @compareEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'.
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT',
--     then @unnormalizedCoordinates@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'
--     or
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlagBits'
--     values
--
-- -   @magFilter@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.Filter.Filter' value
--
-- -   @minFilter@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.Filter.Filter' value
--
-- -   @mipmapMode@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.SamplerMipmapMode.SamplerMipmapMode'
--     value
--
-- -   @addressModeU@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
--     value
--
-- -   @addressModeV@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
--     value
--
-- -   @addressModeW@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.BorderColor.BorderColor',
-- 'Graphics.Vulkan.Core10.Enums.CompareOp.CompareOp',
-- 'Graphics.Vulkan.Core10.Enums.Filter.Filter',
-- 'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode',
-- 'Graphics.Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.SamplerMipmapMode.SamplerMipmapMode',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createSampler'
data SamplerCreateInfo (es :: [Type]) = SamplerCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlagBits'
    -- describing additional parameters of the sampler.
    flags :: SamplerCreateFlags
  , -- | @magFilter@ is a 'Graphics.Vulkan.Core10.Enums.Filter.Filter' value
    -- specifying the magnification filter to apply to lookups.
    magFilter :: Filter
  , -- | @minFilter@ is a 'Graphics.Vulkan.Core10.Enums.Filter.Filter' value
    -- specifying the minification filter to apply to lookups.
    minFilter :: Filter
  , -- | @mipmapMode@ is a
    -- 'Graphics.Vulkan.Core10.Enums.SamplerMipmapMode.SamplerMipmapMode' value
    -- specifying the mipmap filter to apply to lookups.
    mipmapMode :: SamplerMipmapMode
  , -- | @addressModeU@ is a
    -- 'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
    -- value specifying the addressing mode for outside [0..1] range for U
    -- coordinate.
    addressModeU :: SamplerAddressMode
  , -- | @addressModeV@ is a
    -- 'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
    -- value specifying the addressing mode for outside [0..1] range for V
    -- coordinate.
    addressModeV :: SamplerAddressMode
  , -- | @addressModeW@ is a
    -- 'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
    -- value specifying the addressing mode for outside [0..1] range for W
    -- coordinate.
    addressModeW :: SamplerAddressMode
  , -- | @mipLodBias@ is the bias to be added to mipmap LOD (level-of-detail)
    -- calculation and bias provided by image sampling functions in SPIR-V, as
    -- described in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-level-of-detail-operation Level-of-Detail Operation>
    -- section.
    mipLodBias :: Float
  , -- | @anisotropyEnable@ is 'Graphics.Vulkan.Core10.BaseType.TRUE' to enable
    -- anisotropic filtering, as described in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-texel-anisotropic-filtering Texel Anisotropic Filtering>
    -- section, or 'Graphics.Vulkan.Core10.BaseType.FALSE' otherwise.
    anisotropyEnable :: Bool
  , -- | @maxAnisotropy@ is the anisotropy value clamp used by the sampler when
    -- @anisotropyEnable@ is 'Graphics.Vulkan.Core10.BaseType.TRUE'. If
    -- @anisotropyEnable@ is 'Graphics.Vulkan.Core10.BaseType.FALSE',
    -- @maxAnisotropy@ is ignored.
    maxAnisotropy :: Float
  , -- | @compareEnable@ is 'Graphics.Vulkan.Core10.BaseType.TRUE' to enable
    -- comparison against a reference value during lookups, or
    -- 'Graphics.Vulkan.Core10.BaseType.FALSE' otherwise.
    --
    -- -   Note: Some implementations will default to shader state if this
    --     member does not match.
    compareEnable :: Bool
  , -- | @compareOp@ is a 'Graphics.Vulkan.Core10.Enums.CompareOp.CompareOp'
    -- value specifying the comparison function to apply to fetched data before
    -- filtering as described in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation Depth Compare Operation>
    -- section.
    compareOp :: CompareOp
  , -- | @minLod@ and @maxLod@ are the values used to clamp the computed LOD
    -- value, as described in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-level-of-detail-operation Level-of-Detail Operation>
    -- section.
    minLod :: Float
  , -- No documentation found for Nested "VkSamplerCreateInfo" "maxLod"
    maxLod :: Float
  , -- | @borderColor@ is a
    -- 'Graphics.Vulkan.Core10.Enums.BorderColor.BorderColor' value specifying
    -- the predefined border color to use.
    borderColor :: BorderColor
  , -- | @unnormalizedCoordinates@ controls whether to use unnormalized or
    -- normalized texel coordinates to address texels of the image. When set to
    -- 'Graphics.Vulkan.Core10.BaseType.TRUE', the range of the image
    -- coordinates used to lookup the texel is in the range of zero to the
    -- image dimensions for x, y and z. When set to
    -- 'Graphics.Vulkan.Core10.BaseType.FALSE' the range of image coordinates
    -- is zero to one.
    --
    -- When @unnormalizedCoordinates@ is
    -- 'Graphics.Vulkan.Core10.BaseType.TRUE', images the sampler is used with
    -- in the shader have the following requirements:
    --
    -- -   The @viewType@ /must/ be either
    --     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D' or
    --     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'.
    --
    -- -   The image view /must/ have a single layer and a single mip level.
    --
    -- When @unnormalizedCoordinates@ is
    -- 'Graphics.Vulkan.Core10.BaseType.TRUE', image built-in functions in the
    -- shader that use the sampler have the following requirements:
    --
    -- -   The functions /must/ not use projection.
    --
    -- -   The functions /must/ not use offsets.
    unnormalizedCoordinates :: Bool
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (SamplerCreateInfo es)

instance Extensible SamplerCreateInfo where
  extensibleType = STRUCTURE_TYPE_SAMPLER_CREATE_INFO
  setNext x next = x{next = next}
  getNext SamplerCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SamplerCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SamplerReductionModeCreateInfo = Just f
    | Just Refl <- eqT @e @SamplerYcbcrConversionInfo = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (SamplerCreateInfo es) where
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

instance PeekChain es => FromCStruct (SamplerCreateInfo es) where
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
             next flags magFilter minFilter mipmapMode addressModeU addressModeV addressModeW ((\(CFloat a) -> a) mipLodBias) (bool32ToBool anisotropyEnable) ((\(CFloat a) -> a) maxAnisotropy) (bool32ToBool compareEnable) compareOp ((\(CFloat a) -> a) minLod) ((\(CFloat a) -> a) maxLod) borderColor (bool32ToBool unnormalizedCoordinates)

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

