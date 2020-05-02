{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion  ( createSamplerYcbcrConversion
                                                                             , withSamplerYcbcrConversion
                                                                             , destroySamplerYcbcrConversion
                                                                             , SamplerYcbcrConversionInfo(..)
                                                                             , SamplerYcbcrConversionCreateInfo(..)
                                                                             , BindImagePlaneMemoryInfo(..)
                                                                             , ImagePlaneMemoryRequirementsInfo(..)
                                                                             , PhysicalDeviceSamplerYcbcrConversionFeatures(..)
                                                                             , SamplerYcbcrConversionImageFormatProperties(..)
                                                                             , SamplerYcbcrConversion(..)
                                                                             , Format(..)
                                                                             , StructureType(..)
                                                                             , ObjectType(..)
                                                                             , ImageCreateFlagBits(..)
                                                                             , ImageCreateFlags
                                                                             , FormatFeatureFlagBits(..)
                                                                             , FormatFeatureFlags
                                                                             , ImageAspectFlagBits(..)
                                                                             , ImageAspectFlags
                                                                             , SamplerYcbcrModelConversion(..)
                                                                             , SamplerYcbcrRange(..)
                                                                             , ChromaLocation(..)
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
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.Core11.Enums.ChromaLocation (ChromaLocation)
import Graphics.Vulkan.Core10.ImageView (ComponentMapping)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateSamplerYcbcrConversion))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroySamplerYcbcrConversion))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ExternalFormatANDROID)
import Graphics.Vulkan.Core10.Enums.Filter (Filter)
import Graphics.Vulkan.Core10.Enums.Format (Format)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits)
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core11.Handles (SamplerYcbcrConversion)
import Graphics.Vulkan.Core11.Handles (SamplerYcbcrConversion(..))
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion)
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Core11.Enums.ChromaLocation (ChromaLocation(..))
import Graphics.Vulkan.Core10.Enums.Format (Format(..))
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(..))
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(..))
import Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Graphics.Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Graphics.Vulkan.Core11.Handles (SamplerYcbcrConversion(..))
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion(..))
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSamplerYcbcrConversion
  :: FunPtr (Ptr Device_T -> Ptr (SamplerYcbcrConversionCreateInfo a) -> Ptr AllocationCallbacks -> Ptr SamplerYcbcrConversion -> IO Result) -> Ptr Device_T -> Ptr (SamplerYcbcrConversionCreateInfo a) -> Ptr AllocationCallbacks -> Ptr SamplerYcbcrConversion -> IO Result

-- | vkCreateSamplerYcbcrConversion - Create a new Y′CBCR conversion
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the sampler Y′CBCR
--     conversion.
--
-- -   @pCreateInfo@ is a pointer to a 'SamplerYcbcrConversionCreateInfo'
--     structure specifying the requested sampler Y′CBCR conversion.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pYcbcrConversion@ is a pointer to a
--     'Graphics.Vulkan.Core11.Handles.SamplerYcbcrConversion' handle in
--     which the resulting sampler Y′CBCR conversion is returned.
--
-- = Description
--
-- The interpretation of the configured sampler Y′CBCR conversion is
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion the description of sampler Y′CBCR conversion>
-- in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures Image Operations>
-- chapter.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sampler-YCbCr-conversion sampler Y′CBCR conversion feature>
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'SamplerYcbcrConversionCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pYcbcrConversion@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core11.Handles.SamplerYcbcrConversion' handle
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
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core11.Handles.SamplerYcbcrConversion',
-- 'SamplerYcbcrConversionCreateInfo'
createSamplerYcbcrConversion :: forall a io . (PokeChain a, MonadIO io) => Device -> SamplerYcbcrConversionCreateInfo a -> ("allocator" ::: Maybe AllocationCallbacks) -> io (SamplerYcbcrConversion)
createSamplerYcbcrConversion device createInfo allocator = liftIO . evalContT $ do
  let vkCreateSamplerYcbcrConversion' = mkVkCreateSamplerYcbcrConversion (pVkCreateSamplerYcbcrConversion (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPYcbcrConversion <- ContT $ bracket (callocBytes @SamplerYcbcrConversion 8) free
  r <- lift $ vkCreateSamplerYcbcrConversion' (deviceHandle (device)) pCreateInfo pAllocator (pPYcbcrConversion)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pYcbcrConversion <- lift $ peek @SamplerYcbcrConversion pPYcbcrConversion
  pure $ (pYcbcrConversion)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSamplerYcbcrConversion' and 'destroySamplerYcbcrConversion'
--
-- To ensure that 'destroySamplerYcbcrConversion' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withSamplerYcbcrConversion :: forall a io r . (PokeChain a, MonadIO io) => (io (SamplerYcbcrConversion) -> ((SamplerYcbcrConversion) -> io ()) -> r) -> Device -> SamplerYcbcrConversionCreateInfo a -> Maybe AllocationCallbacks -> r
withSamplerYcbcrConversion b device pCreateInfo pAllocator =
  b (createSamplerYcbcrConversion device pCreateInfo pAllocator)
    (\(o0) -> destroySamplerYcbcrConversion device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySamplerYcbcrConversion
  :: FunPtr (Ptr Device_T -> SamplerYcbcrConversion -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> SamplerYcbcrConversion -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroySamplerYcbcrConversion - Destroy a created Y′CBCR conversion
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the Y′CBCR conversion.
--
-- -   @ycbcrConversion@ is the conversion to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   If @ycbcrConversion@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @ycbcrConversion@
--     /must/ be a valid
--     'Graphics.Vulkan.Core11.Handles.SamplerYcbcrConversion' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   If @ycbcrConversion@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @ycbcrConversion@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core11.Handles.SamplerYcbcrConversion'
destroySamplerYcbcrConversion :: forall io . MonadIO io => Device -> SamplerYcbcrConversion -> ("allocator" ::: Maybe AllocationCallbacks) -> io ()
destroySamplerYcbcrConversion device ycbcrConversion allocator = liftIO . evalContT $ do
  let vkDestroySamplerYcbcrConversion' = mkVkDestroySamplerYcbcrConversion (pVkDestroySamplerYcbcrConversion (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroySamplerYcbcrConversion' (deviceHandle (device)) (ycbcrConversion) pAllocator
  pure $ ()


-- | VkSamplerYcbcrConversionInfo - Structure specifying Y′CBCR conversion to
-- a sampler or image view
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Handles.SamplerYcbcrConversion',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data SamplerYcbcrConversionInfo = SamplerYcbcrConversionInfo
  { -- | @conversion@ /must/ be a valid
    -- 'Graphics.Vulkan.Core11.Handles.SamplerYcbcrConversion' handle
    conversion :: SamplerYcbcrConversion }
  deriving (Typeable)
deriving instance Show SamplerYcbcrConversionInfo

instance ToCStruct SamplerYcbcrConversionInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SamplerYcbcrConversionInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SamplerYcbcrConversion)) (conversion)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SamplerYcbcrConversion)) (zero)
    f

instance FromCStruct SamplerYcbcrConversionInfo where
  peekCStruct p = do
    conversion <- peek @SamplerYcbcrConversion ((p `plusPtr` 16 :: Ptr SamplerYcbcrConversion))
    pure $ SamplerYcbcrConversionInfo
             conversion

instance Storable SamplerYcbcrConversionInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SamplerYcbcrConversionInfo where
  zero = SamplerYcbcrConversionInfo
           zero


-- | VkSamplerYcbcrConversionCreateInfo - Structure specifying the parameters
-- of the newly created conversion
--
-- = Description
--
-- Note
--
-- Setting @forceExplicitReconstruction@ to
-- 'Graphics.Vulkan.Core10.BaseType.TRUE' /may/ have a performance penalty
-- on implementations where explicit reconstruction is not the default mode
-- of operation.
--
-- If @format@ supports
-- 'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT'
-- the @forceExplicitReconstruction@ value behaves as if it was set to
-- 'Graphics.Vulkan.Core10.BaseType.TRUE'.
--
-- If the @pNext@ chain includes a
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
-- structure with non-zero @externalFormat@ member, the sampler Y′CBCR
-- conversion object represents an /external format conversion/, and
-- @format@ /must/ be
-- 'Graphics.Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'. Such conversions
-- /must/ only be used to sample image views with a matching
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats external format>.
-- When creating an external format conversion, the value of @components@
-- is ignored.
--
-- == Valid Usage
--
-- -   If an external format conversion is being created, @format@ /must/
--     be 'Graphics.Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', otherwise
--     it /must/ not be
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-sampler-ycbcr-conversion-format-features sampler Y′CBCR conversion’s features>
--     /must/ support
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-sampler-ycbcr-conversion-format-features sampler Y′CBCR conversion’s features>
--     do not support
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT',
--     @xChromaOffset@ and @yChromaOffset@ /must/ not be
--     'Graphics.Vulkan.Core11.Enums.ChromaLocation.CHROMA_LOCATION_COSITED_EVEN'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-sampler-ycbcr-conversion-format-features sampler Y′CBCR conversion’s features>
--     do not support
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT',
--     @xChromaOffset@ and @yChromaOffset@ /must/ not be
--     'Graphics.Vulkan.Core11.Enums.ChromaLocation.CHROMA_LOCATION_MIDPOINT'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.g@
--     /must/ be
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_IDENTITY'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.a@
--     /must/ be
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_IDENTITY',
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ONE',
--     or
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ZERO'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.r@
--     /must/ be
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_IDENTITY'
--     or
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_B'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.b@
--     /must/ be
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_IDENTITY'
--     or
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_R'
--
-- -   If the format has a @_422@ or @_420@ suffix, and if either
--     @components.r@ or @components.b@ is
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_IDENTITY',
--     both values /must/ be
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_IDENTITY'
--
-- -   If @ycbcrModel@ is not
--     'Graphics.Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY',
--     then @components.r@, @components.g@, and @components.b@ /must/
--     correspond to channels of the @format@; that is, @components.r@,
--     @components.g@, and @components.b@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ZERO'
--     or
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ONE',
--     and /must/ not correspond to a channel which contains zero or one as
--     a consequence of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-conversion-to-rgba conversion to RGBA>
--
-- -   If @ycbcrRange@ is
--     'Graphics.Vulkan.Core11.Enums.SamplerYcbcrRange.SAMPLER_YCBCR_RANGE_ITU_NARROW'
--     then the R, G and B channels obtained by applying the @component@
--     swizzle to @format@ /must/ each have a bit-depth greater than or
--     equal to 8
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-sampler-ycbcr-conversion-format-features sampler Y′CBCR conversion’s features>
--     do not support
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT'
--     @forceExplicitReconstruction@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-sampler-ycbcr-conversion-format-features sampler Y′CBCR conversion’s features>
--     do not support
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT',
--     @chromaFilter@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @format@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.Format.Format' value
--
-- -   @ycbcrModel@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SamplerYcbcrModelConversion'
--     value
--
-- -   @ycbcrRange@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Enums.SamplerYcbcrRange.SamplerYcbcrRange'
--     value
--
-- -   @components@ /must/ be a valid
--     'Graphics.Vulkan.Core10.ImageView.ComponentMapping' structure
--
-- -   @xChromaOffset@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Enums.ChromaLocation.ChromaLocation' value
--
-- -   @yChromaOffset@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Enums.ChromaLocation.ChromaLocation' value
--
-- -   @chromaFilter@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.Filter.Filter' value
--
-- If @chromaFilter@ is
-- 'Graphics.Vulkan.Core10.Enums.Filter.FILTER_NEAREST', chroma samples are
-- reconstructed to luma channel resolution using nearest-neighbour
-- sampling. Otherwise, chroma samples are reconstructed using
-- interpolation. More details can be found in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion the description of sampler Y′CBCR conversion>
-- in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures Image Operations>
-- chapter.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core11.Enums.ChromaLocation.ChromaLocation',
-- 'Graphics.Vulkan.Core10.ImageView.ComponentMapping',
-- 'Graphics.Vulkan.Core10.Enums.Filter.Filter',
-- 'Graphics.Vulkan.Core10.Enums.Format.Format',
-- 'Graphics.Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SamplerYcbcrModelConversion',
-- 'Graphics.Vulkan.Core11.Enums.SamplerYcbcrRange.SamplerYcbcrRange',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createSamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversionKHR'
data SamplerYcbcrConversionCreateInfo (es :: [Type]) = SamplerYcbcrConversionCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @format@ is the format of the image from which color information will be
    -- retrieved.
    format :: Format
  , -- | @ycbcrModel@ describes the color matrix for conversion between color
    -- models.
    ycbcrModel :: SamplerYcbcrModelConversion
  , -- | @ycbcrRange@ describes whether the encoded values have headroom and foot
    -- room, or whether the encoding uses the full numerical range.
    ycbcrRange :: SamplerYcbcrRange
  , -- | @components@ applies a /swizzle/ based on
    -- 'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle' enums
    -- prior to range expansion and color model conversion.
    components :: ComponentMapping
  , -- | @xChromaOffset@ describes the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-chroma-reconstruction sample location>
    -- associated with downsampled chroma channels in the x dimension.
    -- @xChromaOffset@ has no effect for formats in which chroma channels are
    -- the same resolution as the luma channel.
    xChromaOffset :: ChromaLocation
  , -- | @yChromaOffset@ describes the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-chroma-reconstruction sample location>
    -- associated with downsampled chroma channels in the y dimension.
    -- @yChromaOffset@ has no effect for formats in which the chroma channels
    -- are not downsampled vertically.
    yChromaOffset :: ChromaLocation
  , -- | @chromaFilter@ is the filter for chroma reconstruction.
    chromaFilter :: Filter
  , -- | @forceExplicitReconstruction@ /can/ be used to ensure that
    -- reconstruction is done explicitly, if supported.
    forceExplicitReconstruction :: Bool
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (SamplerYcbcrConversionCreateInfo es)

instance Extensible SamplerYcbcrConversionCreateInfo where
  extensibleType = STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
  setNext x next = x{next = next}
  getNext SamplerYcbcrConversionCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SamplerYcbcrConversionCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ExternalFormatANDROID = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (SamplerYcbcrConversionCreateInfo es) where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SamplerYcbcrConversionCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (format)
    lift $ poke ((p `plusPtr` 20 :: Ptr SamplerYcbcrModelConversion)) (ycbcrModel)
    lift $ poke ((p `plusPtr` 24 :: Ptr SamplerYcbcrRange)) (ycbcrRange)
    ContT $ pokeCStruct ((p `plusPtr` 28 :: Ptr ComponentMapping)) (components) . ($ ())
    lift $ poke ((p `plusPtr` 44 :: Ptr ChromaLocation)) (xChromaOffset)
    lift $ poke ((p `plusPtr` 48 :: Ptr ChromaLocation)) (yChromaOffset)
    lift $ poke ((p `plusPtr` 52 :: Ptr Filter)) (chromaFilter)
    lift $ poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (forceExplicitReconstruction))
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr SamplerYcbcrModelConversion)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr SamplerYcbcrRange)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 28 :: Ptr ComponentMapping)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 44 :: Ptr ChromaLocation)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr ChromaLocation)) (zero)
    lift $ poke ((p `plusPtr` 52 :: Ptr Filter)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance PeekChain es => FromCStruct (SamplerYcbcrConversionCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    format <- peek @Format ((p `plusPtr` 16 :: Ptr Format))
    ycbcrModel <- peek @SamplerYcbcrModelConversion ((p `plusPtr` 20 :: Ptr SamplerYcbcrModelConversion))
    ycbcrRange <- peek @SamplerYcbcrRange ((p `plusPtr` 24 :: Ptr SamplerYcbcrRange))
    components <- peekCStruct @ComponentMapping ((p `plusPtr` 28 :: Ptr ComponentMapping))
    xChromaOffset <- peek @ChromaLocation ((p `plusPtr` 44 :: Ptr ChromaLocation))
    yChromaOffset <- peek @ChromaLocation ((p `plusPtr` 48 :: Ptr ChromaLocation))
    chromaFilter <- peek @Filter ((p `plusPtr` 52 :: Ptr Filter))
    forceExplicitReconstruction <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    pure $ SamplerYcbcrConversionCreateInfo
             next format ycbcrModel ycbcrRange components xChromaOffset yChromaOffset chromaFilter (bool32ToBool forceExplicitReconstruction)

instance es ~ '[] => Zero (SamplerYcbcrConversionCreateInfo es) where
  zero = SamplerYcbcrConversionCreateInfo
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkBindImagePlaneMemoryInfo - Structure specifying how to bind an image
-- plane to memory
--
-- == Valid Usage
--
-- -   If the image’s @tiling@ is
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' or
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL',
--     then @planeAspect@ /must/ be a single valid /format plane/ for the
--     image (that is, for a two-plane image @planeAspect@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     and for a three-plane image @planeAspect@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT')
--
-- -   If the image’s @tiling@ is
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then @planeAspect@ /must/ be a single valid /memory plane/ for the
--     image (that is, @aspectMask@ /must/ specify a plane index that is
--     less than the
--     'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
--     associated with the image’s @format@ and
--     'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierPropertiesEXT'::@drmFormatModifier@)
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO'
--
-- -   @planeAspect@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data BindImagePlaneMemoryInfo = BindImagePlaneMemoryInfo
  { -- | @planeAspect@ is the aspect of the disjoint image plane to bind.
    planeAspect :: ImageAspectFlagBits }
  deriving (Typeable)
deriving instance Show BindImagePlaneMemoryInfo

instance ToCStruct BindImagePlaneMemoryInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindImagePlaneMemoryInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageAspectFlagBits)) (planeAspect)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageAspectFlagBits)) (zero)
    f

instance FromCStruct BindImagePlaneMemoryInfo where
  peekCStruct p = do
    planeAspect <- peek @ImageAspectFlagBits ((p `plusPtr` 16 :: Ptr ImageAspectFlagBits))
    pure $ BindImagePlaneMemoryInfo
             planeAspect

instance Storable BindImagePlaneMemoryInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindImagePlaneMemoryInfo where
  zero = BindImagePlaneMemoryInfo
           zero


-- | VkImagePlaneMemoryRequirementsInfo - Structure specifying image plane
-- for memory requirements
--
-- == Valid Usage
--
-- -   If the image’s @tiling@ is
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' or
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL',
--     then @planeAspect@ /must/ be a single valid /format plane/ for the
--     image (that is, for a two-plane image @planeAspect@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     and for a three-plane image @planeAspect@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT')
--
-- -   If the image’s @tiling@ is
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then @planeAspect@ /must/ be a single valid /memory plane/ for the
--     image (that is, @aspectMask@ /must/ specify a plane index that is
--     less than the
--     'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
--     associated with the image’s @format@ and
--     'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierPropertiesEXT'::@drmFormatModifier@)
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO'
--
-- -   @planeAspect@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data ImagePlaneMemoryRequirementsInfo = ImagePlaneMemoryRequirementsInfo
  { -- | @planeAspect@ is the aspect corresponding to the image plane to query.
    planeAspect :: ImageAspectFlagBits }
  deriving (Typeable)
deriving instance Show ImagePlaneMemoryRequirementsInfo

instance ToCStruct ImagePlaneMemoryRequirementsInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImagePlaneMemoryRequirementsInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageAspectFlagBits)) (planeAspect)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageAspectFlagBits)) (zero)
    f

instance FromCStruct ImagePlaneMemoryRequirementsInfo where
  peekCStruct p = do
    planeAspect <- peek @ImageAspectFlagBits ((p `plusPtr` 16 :: Ptr ImageAspectFlagBits))
    pure $ ImagePlaneMemoryRequirementsInfo
             planeAspect

instance Storable ImagePlaneMemoryRequirementsInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImagePlaneMemoryRequirementsInfo where
  zero = ImagePlaneMemoryRequirementsInfo
           zero


-- | VkPhysicalDeviceSamplerYcbcrConversionFeatures - Structure describing
-- Y’CbCr conversion features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceSamplerYcbcrConversionFeatures'
-- structure describe the following feature:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSamplerYcbcrConversionFeatures = PhysicalDeviceSamplerYcbcrConversionFeatures
  { -- | @samplerYcbcrConversion@ specifies whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>.
    -- If @samplerYcbcrConversion@ is 'Graphics.Vulkan.Core10.BaseType.FALSE',
    -- sampler Y′CBCR conversion is not supported, and samplers using sampler
    -- Y′CBCR conversion /must/ not be used.
    samplerYcbcrConversion :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceSamplerYcbcrConversionFeatures

instance ToCStruct PhysicalDeviceSamplerYcbcrConversionFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSamplerYcbcrConversionFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (samplerYcbcrConversion))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSamplerYcbcrConversionFeatures where
  peekCStruct p = do
    samplerYcbcrConversion <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceSamplerYcbcrConversionFeatures
             (bool32ToBool samplerYcbcrConversion)

instance Storable PhysicalDeviceSamplerYcbcrConversionFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSamplerYcbcrConversionFeatures where
  zero = PhysicalDeviceSamplerYcbcrConversionFeatures
           zero


-- | VkSamplerYcbcrConversionImageFormatProperties - Structure specifying
-- combined image sampler descriptor count for multi-planar images
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data SamplerYcbcrConversionImageFormatProperties = SamplerYcbcrConversionImageFormatProperties
  { -- | @combinedImageSamplerDescriptorCount@ is the number of combined image
    -- sampler descriptors that the implementation uses to access the format.
    combinedImageSamplerDescriptorCount :: Word32 }
  deriving (Typeable)
deriving instance Show SamplerYcbcrConversionImageFormatProperties

instance ToCStruct SamplerYcbcrConversionImageFormatProperties where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SamplerYcbcrConversionImageFormatProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (combinedImageSamplerDescriptorCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct SamplerYcbcrConversionImageFormatProperties where
  peekCStruct p = do
    combinedImageSamplerDescriptorCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ SamplerYcbcrConversionImageFormatProperties
             combinedImageSamplerDescriptorCount

instance Storable SamplerYcbcrConversionImageFormatProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SamplerYcbcrConversionImageFormatProperties where
  zero = SamplerYcbcrConversionImageFormatProperties
           zero

