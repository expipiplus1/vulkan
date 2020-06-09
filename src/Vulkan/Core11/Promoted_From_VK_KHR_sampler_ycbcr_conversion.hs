{-# language CPP #-}
module Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion  ( createSamplerYcbcrConversion
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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core11.Enums.ChromaLocation (ChromaLocation)
import Vulkan.Core10.ImageView (ComponentMapping)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateSamplerYcbcrConversion))
import Vulkan.Dynamic (DeviceCmds(pVkDestroySamplerYcbcrConversion))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ExternalFormatANDROID)
import Vulkan.Core10.Enums.Filter (Filter)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core11.Handles (SamplerYcbcrConversion)
import Vulkan.Core11.Handles (SamplerYcbcrConversion(..))
import Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion)
import Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core11.Enums.ChromaLocation (ChromaLocation(..))
import Vulkan.Core10.Enums.Format (Format(..))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(..))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(..))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Vulkan.Core11.Handles (SamplerYcbcrConversion(..))
import Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion(..))
import Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSamplerYcbcrConversion
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct SamplerYcbcrConversionCreateInfo) -> Ptr AllocationCallbacks -> Ptr SamplerYcbcrConversion -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct SamplerYcbcrConversionCreateInfo) -> Ptr AllocationCallbacks -> Ptr SamplerYcbcrConversion -> IO Result

-- | vkCreateSamplerYcbcrConversion - Create a new Y′CBCR conversion
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
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'SamplerYcbcrConversionCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pYcbcrConversion@ /must/ be a valid pointer to a
--     'Vulkan.Core11.Handles.SamplerYcbcrConversion' handle
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
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core11.Handles.SamplerYcbcrConversion',
-- 'SamplerYcbcrConversionCreateInfo'
createSamplerYcbcrConversion :: forall a io
                              . (Extendss SamplerYcbcrConversionCreateInfo a, PokeChain a, MonadIO io)
                             => -- | @device@ is the logical device that creates the sampler Y′CBCR
                                -- conversion.
                                Device
                             -> -- | @pCreateInfo@ is a pointer to a 'SamplerYcbcrConversionCreateInfo'
                                -- structure specifying the requested sampler Y′CBCR conversion.
                                (SamplerYcbcrConversionCreateInfo a)
                             -> -- | @pAllocator@ controls host memory allocation as described in the
                                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                -- chapter.
                                ("allocator" ::: Maybe AllocationCallbacks)
                             -> io (SamplerYcbcrConversion)
createSamplerYcbcrConversion device createInfo allocator = liftIO . evalContT $ do
  let vkCreateSamplerYcbcrConversionPtr = pVkCreateSamplerYcbcrConversion (deviceCmds (device :: Device))
  lift $ unless (vkCreateSamplerYcbcrConversionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateSamplerYcbcrConversion is null" Nothing Nothing
  let vkCreateSamplerYcbcrConversion' = mkVkCreateSamplerYcbcrConversion vkCreateSamplerYcbcrConversionPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPYcbcrConversion <- ContT $ bracket (callocBytes @SamplerYcbcrConversion 8) free
  r <- lift $ vkCreateSamplerYcbcrConversion' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPYcbcrConversion)
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
withSamplerYcbcrConversion :: forall a io r . (Extendss SamplerYcbcrConversionCreateInfo a, PokeChain a, MonadIO io) => Device -> SamplerYcbcrConversionCreateInfo a -> Maybe AllocationCallbacks -> (io (SamplerYcbcrConversion) -> ((SamplerYcbcrConversion) -> io ()) -> r) -> r
withSamplerYcbcrConversion device pCreateInfo pAllocator b =
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
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @ycbcrConversion@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @ycbcrConversion@ /must/
--     be a valid 'Vulkan.Core11.Handles.SamplerYcbcrConversion' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
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
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core11.Handles.SamplerYcbcrConversion'
destroySamplerYcbcrConversion :: forall io
                               . (MonadIO io)
                              => -- | @device@ is the logical device that destroys the Y′CBCR conversion.
                                 Device
                              -> -- | @ycbcrConversion@ is the conversion to destroy.
                                 SamplerYcbcrConversion
                              -> -- | @pAllocator@ controls host memory allocation as described in the
                                 -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                 -- chapter.
                                 ("allocator" ::: Maybe AllocationCallbacks)
                              -> io ()
destroySamplerYcbcrConversion device ycbcrConversion allocator = liftIO . evalContT $ do
  let vkDestroySamplerYcbcrConversionPtr = pVkDestroySamplerYcbcrConversion (deviceCmds (device :: Device))
  lift $ unless (vkDestroySamplerYcbcrConversionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroySamplerYcbcrConversion is null" Nothing Nothing
  let vkDestroySamplerYcbcrConversion' = mkVkDestroySamplerYcbcrConversion vkDestroySamplerYcbcrConversionPtr
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
-- 'Vulkan.Core11.Handles.SamplerYcbcrConversion',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SamplerYcbcrConversionInfo = SamplerYcbcrConversionInfo
  { -- | @conversion@ is a 'Vulkan.Core11.Handles.SamplerYcbcrConversion' handle
    -- created with 'createSamplerYcbcrConversion'.
    --
    -- @conversion@ /must/ be a valid
    -- 'Vulkan.Core11.Handles.SamplerYcbcrConversion' handle
    conversion :: SamplerYcbcrConversion }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerYcbcrConversionInfo)
#endif
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
-- 'Vulkan.Core10.FundamentalTypes.TRUE' /may/ have a performance penalty
-- on implementations where explicit reconstruction is not the default mode
-- of operation.
--
-- If @format@ supports
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT'
-- the @forceExplicitReconstruction@ value behaves as if it was set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE'.
--
-- If the @pNext@ chain includes a
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
-- structure with non-zero @externalFormat@ member, the sampler Y′CBCR
-- conversion object represents an /external format conversion/, and
-- @format@ /must/ be 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'. Such
-- conversions /must/ only be used to sample image views with a matching
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats external format>.
-- When creating an external format conversion, the value of @components@
-- is ignored.
--
-- == Valid Usage
--
-- -   If an external format conversion is being created, @format@ /must/
--     be 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   If an external format conversion is not being created, @format@
--     /must/ represent unsigned normalized values (i.e. the format must be
--     a @UNORM@ format)
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-sampler-ycbcr-conversion-format-features sampler Y′CBCR conversion’s features>
--     /must/ support
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT'
--     or
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-sampler-ycbcr-conversion-format-features sampler Y′CBCR conversion’s features>
--     do not support
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT',
--     @xChromaOffset@ and @yChromaOffset@ /must/ not be
--     'Vulkan.Core11.Enums.ChromaLocation.CHROMA_LOCATION_COSITED_EVEN'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-sampler-ycbcr-conversion-format-features sampler Y′CBCR conversion’s features>
--     do not support
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT',
--     @xChromaOffset@ and @yChromaOffset@ /must/ not be
--     'Vulkan.Core11.Enums.ChromaLocation.CHROMA_LOCATION_MIDPOINT'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.g@
--     /must/ be the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views-identity-mappings identity swizzle>
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.a@
--     /must/ be the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views-identity-mappings identity swizzle>,
--     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ONE', or
--     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ZERO'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.r@
--     /must/ be the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views-identity-mappings identity swizzle>
--     or 'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_B'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.b@
--     /must/ be the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views-identity-mappings identity swizzle>
--     or 'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_R'
--
-- -   If the format has a @_422@ or @_420@ suffix, and if either
--     @components.r@ or @components.b@ is the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views-identity-mappings identity swizzle>,
--     both values /must/ be the identity swizzle
--
-- -   If @ycbcrModel@ is not
--     'Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY',
--     then @components.r@, @components.g@, and @components.b@ /must/
--     correspond to channels of the @format@; that is, @components.r@,
--     @components.g@, and @components.b@ /must/ not be
--     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ZERO' or
--     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ONE', and
--     /must/ not correspond to a channel which contains zero or one as a
--     consequence of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-conversion-to-rgba conversion to RGBA>
--
-- -   If @ycbcrRange@ is
--     'Vulkan.Core11.Enums.SamplerYcbcrRange.SAMPLER_YCBCR_RANGE_ITU_NARROW'
--     then the R, G and B channels obtained by applying the @component@
--     swizzle to @format@ /must/ each have a bit-depth greater than or
--     equal to 8
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-sampler-ycbcr-conversion-format-features sampler Y′CBCR conversion’s features>
--     do not support
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT'
--     @forceExplicitReconstruction@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-sampler-ycbcr-conversion-format-features sampler Y′CBCR conversion’s features>
--     do not support
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT',
--     @chromaFilter@ /must/ not be
--     'Vulkan.Core10.Enums.Filter.FILTER_LINEAR'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @format@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- -   @ycbcrModel@ /must/ be a valid
--     'Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SamplerYcbcrModelConversion'
--     value
--
-- -   @ycbcrRange@ /must/ be a valid
--     'Vulkan.Core11.Enums.SamplerYcbcrRange.SamplerYcbcrRange' value
--
-- -   @components@ /must/ be a valid
--     'Vulkan.Core10.ImageView.ComponentMapping' structure
--
-- -   @xChromaOffset@ /must/ be a valid
--     'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation' value
--
-- -   @yChromaOffset@ /must/ be a valid
--     'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation' value
--
-- -   @chromaFilter@ /must/ be a valid 'Vulkan.Core10.Enums.Filter.Filter'
--     value
--
-- If @chromaFilter@ is 'Vulkan.Core10.Enums.Filter.FILTER_NEAREST', chroma
-- samples are reconstructed to luma channel resolution using
-- nearest-neighbour sampling. Otherwise, chroma samples are reconstructed
-- using interpolation. More details can be found in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion the description of sampler Y′CBCR conversion>
-- in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures Image Operations>
-- chapter.
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation',
-- 'Vulkan.Core10.ImageView.ComponentMapping',
-- 'Vulkan.Core10.Enums.Filter.Filter',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SamplerYcbcrModelConversion',
-- 'Vulkan.Core11.Enums.SamplerYcbcrRange.SamplerYcbcrRange',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createSamplerYcbcrConversion',
-- 'Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversionKHR'
data SamplerYcbcrConversionCreateInfo (es :: [Type]) = SamplerYcbcrConversionCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
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
    -- 'Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle' enums prior to
    -- range expansion and color model conversion.
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
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerYcbcrConversionCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SamplerYcbcrConversionCreateInfo es)

instance Extensible SamplerYcbcrConversionCreateInfo where
  extensibleType = STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
  setNext x next = x{next = next}
  getNext SamplerYcbcrConversionCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SamplerYcbcrConversionCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ExternalFormatANDROID = Just f
    | otherwise = Nothing

instance (Extendss SamplerYcbcrConversionCreateInfo es, PokeChain es) => ToCStruct (SamplerYcbcrConversionCreateInfo es) where
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

instance (Extendss SamplerYcbcrConversionCreateInfo es, PeekChain es) => FromCStruct (SamplerYcbcrConversionCreateInfo es) where
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
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' or
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', then
--     @planeAspect@ /must/ be a single valid /format plane/ for the image
--     (that is, for a two-plane image @planeAspect@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     and for a three-plane image @planeAspect@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT')
--
-- -   If the image’s @tiling@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then @planeAspect@ /must/ be a single valid /memory plane/ for the
--     image (that is, @aspectMask@ /must/ specify a plane index that is
--     less than the
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
--     associated with the image’s @format@ and
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierPropertiesEXT'::@drmFormatModifier@)
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO'
--
-- -   @planeAspect@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BindImagePlaneMemoryInfo = BindImagePlaneMemoryInfo
  { -- | @planeAspect@ is the aspect of the disjoint image plane to bind.
    planeAspect :: ImageAspectFlagBits }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindImagePlaneMemoryInfo)
#endif
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
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' or
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', then
--     @planeAspect@ /must/ be a single valid /format plane/ for the image
--     (that is, for a two-plane image @planeAspect@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     and for a three-plane image @planeAspect@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT')
--
-- -   If the image’s @tiling@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then @planeAspect@ /must/ be a single valid /memory plane/ for the
--     image (that is, @aspectMask@ /must/ specify a plane index that is
--     less than the
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
--     associated with the image’s @format@ and
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierPropertiesEXT'::@drmFormatModifier@)
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO'
--
-- -   @planeAspect@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImagePlaneMemoryRequirementsInfo = ImagePlaneMemoryRequirementsInfo
  { -- | @planeAspect@ is the aspect corresponding to the image plane to query.
    planeAspect :: ImageAspectFlagBits }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImagePlaneMemoryRequirementsInfo)
#endif
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
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSamplerYcbcrConversionFeatures = PhysicalDeviceSamplerYcbcrConversionFeatures
  { -- | @samplerYcbcrConversion@ specifies whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>.
    -- If @samplerYcbcrConversion@ is 'Vulkan.Core10.FundamentalTypes.FALSE',
    -- sampler Y′CBCR conversion is not supported, and samplers using sampler
    -- Y′CBCR conversion /must/ not be used.
    samplerYcbcrConversion :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSamplerYcbcrConversionFeatures)
#endif
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
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SamplerYcbcrConversionImageFormatProperties = SamplerYcbcrConversionImageFormatProperties
  { -- | @combinedImageSamplerDescriptorCount@ is the number of combined image
    -- sampler descriptors that the implementation uses to access the format.
    combinedImageSamplerDescriptorCount :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerYcbcrConversionImageFormatProperties)
#endif
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

