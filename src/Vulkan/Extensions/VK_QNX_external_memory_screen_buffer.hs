{-# language CPP #-}
-- | = Name
--
-- VK_QNX_external_memory_screen_buffer - device extension
--
-- == VK_QNX_external_memory_screen_buffer
--
-- [__Name String__]
--     @VK_QNX_external_memory_screen_buffer@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     530
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_sampler_ycbcr_conversion VK_KHR_sampler_ycbcr_conversion>
--              and
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory VK_KHR_external_memory>
--              and
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dedicated_allocation VK_KHR_dedicated_allocation>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_queue_family_foreign VK_EXT_queue_family_foreign>
--
-- [__Contact__]
--
--     -   Mike Gorchak
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QNX_external_memory_screen_buffer] @mgorchak-blackberry%0A*Here describe the issue or question you have about the VK_QNX_external_memory_screen_buffer extension* >
--
--     -   Aaron Ruby
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QNX_external_memory_screen_buffer] @aruby-blackberry%0A*Here describe the issue or question you have about the VK_QNX_external_memory_screen_buffer extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-05-17
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Gorchak, QNX \/ Blackberry Limited
--
--     -   Aaron Ruby, QNX \/ Blackberry Limited
--
-- == Description
--
-- This extension enables an application to import QNX Screen
-- 'Screen_buffer' objects created outside of the Vulkan device into Vulkan
-- memory objects, where they can be bound to images and buffers.
--
-- Some 'Screen_buffer' images have implementation-defined /external
-- formats/ that /may/ not correspond to Vulkan formats. Sampler Y′CBCR
-- conversion /can/ be used to sample from these images and convert them to
-- a known color space.
--
-- 'Screen_buffer' is strongly typed, so naming the handle type is
-- redundant. The internal layout and therefore size of a 'Screen_buffer'
-- image may depend on native usage flags that do not have corresponding
-- Vulkan counterparts.
--
-- == New Commands
--
-- -   'getScreenBufferPropertiesQNX'
--
-- == New Structures
--
-- -   'ScreenBufferPropertiesQNX'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo':
--
--     -   'ExternalFormatQNX'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportScreenBufferInfoQNX'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX'
--
-- -   Extending 'ScreenBufferPropertiesQNX':
--
--     -   'ScreenBufferFormatPropertiesQNX'
--
-- == New Enum Constants
--
-- -   'QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_EXTENSION_NAME'
--
-- -   'QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_SCREEN_BUFFER_BIT_QNX'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_FORMAT_QNX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_SCREEN_BUFFER_INFO_QNX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_SCREEN_BUFFER_FEATURES_QNX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SCREEN_BUFFER_FORMAT_PROPERTIES_QNX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SCREEN_BUFFER_PROPERTIES_QNX'
--
-- == Version History
--
-- -   Revision 1, 2023-05-17 (Mike Gorchak)
--
--     -   Initial version
--
-- == See Also
--
-- 'ExternalFormatQNX', 'ImportScreenBufferInfoQNX',
-- 'PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX',
-- 'ScreenBufferFormatPropertiesQNX', 'ScreenBufferPropertiesQNX',
-- 'getScreenBufferPropertiesQNX'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QNX_external_memory_screen_buffer Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QNX_external_memory_screen_buffer  ( getScreenBufferPropertiesQNX
                                                               , ImportScreenBufferInfoQNX(..)
                                                               , ScreenBufferPropertiesQNX(..)
                                                               , ScreenBufferFormatPropertiesQNX(..)
                                                               , ExternalFormatQNX(..)
                                                               , PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX(..)
                                                               , QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_SPEC_VERSION
                                                               , pattern QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_SPEC_VERSION
                                                               , QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_EXTENSION_NAME
                                                               , pattern QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_EXTENSION_NAME
                                                               , Screen_buffer
                                                               ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
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
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core11.Enums.ChromaLocation (ChromaLocation)
import Vulkan.Core10.ImageView (ComponentMapping)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetScreenBufferPropertiesQNX))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion)
import Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_FORMAT_QNX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_SCREEN_BUFFER_INFO_QNX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_SCREEN_BUFFER_FEATURES_QNX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SCREEN_BUFFER_FORMAT_PROPERTIES_QNX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SCREEN_BUFFER_PROPERTIES_QNX))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetScreenBufferPropertiesQNX
  :: FunPtr (Ptr Device_T -> Ptr Screen_buffer -> Ptr (SomeStruct ScreenBufferPropertiesQNX) -> IO Result) -> Ptr Device_T -> Ptr Screen_buffer -> Ptr (SomeStruct ScreenBufferPropertiesQNX) -> IO Result

-- | vkGetScreenBufferPropertiesQNX - Get Properties of External Memory QNX
-- Screen Buffers
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
--     -   'Vulkan.Extensions.VK_KHR_external_memory.ERROR_INVALID_EXTERNAL_HANDLE_KHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QNX_external_memory_screen_buffer VK_QNX_external_memory_screen_buffer>,
-- 'Vulkan.Core10.Handles.Device', 'ScreenBufferPropertiesQNX'
getScreenBufferPropertiesQNX :: forall a io
                              . ( Extendss ScreenBufferPropertiesQNX a
                                , PokeChain a
                                , PeekChain a
                                , MonadIO io )
                             => -- | @device@ is the logical device that will be importing @buffer@.
                                --
                                -- #VUID-vkGetScreenBufferPropertiesQNX-device-parameter# @device@ /must/
                                -- be a valid 'Vulkan.Core10.Handles.Device' handle
                                Device
                             -> -- | @buffer@ is the QNX Screen buffer which will be imported.
                                --
                                -- #VUID-vkGetScreenBufferPropertiesQNX-buffer-08968# @buffer@ /must/ be a
                                -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-external-screen-buffer-validity valid QNX Screen buffer>
                                --
                                -- #VUID-vkGetScreenBufferPropertiesQNX-buffer-parameter# @buffer@ /must/
                                -- be a valid pointer to a valid 'Screen_buffer' value
                                (Ptr Screen_buffer)
                             -> io (ScreenBufferPropertiesQNX a)
getScreenBufferPropertiesQNX device buffer = liftIO . evalContT $ do
  let vkGetScreenBufferPropertiesQNXPtr = pVkGetScreenBufferPropertiesQNX (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetScreenBufferPropertiesQNXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetScreenBufferPropertiesQNX is null" Nothing Nothing
  let vkGetScreenBufferPropertiesQNX' = mkVkGetScreenBufferPropertiesQNX vkGetScreenBufferPropertiesQNXPtr
  pPProperties <- ContT (withZeroCStruct @(ScreenBufferPropertiesQNX _))
  r <- lift $ traceAroundEvent "vkGetScreenBufferPropertiesQNX" (vkGetScreenBufferPropertiesQNX'
                                                                   (deviceHandle (device))
                                                                   (buffer)
                                                                   (forgetExtensions (pPProperties)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pProperties <- lift $ peekCStruct @(ScreenBufferPropertiesQNX _) pPProperties
  pure $ (pProperties)


-- | VkImportScreenBufferInfoQNX - Import memory from a QNX Screen buffer
--
-- = Description
--
-- The implementation /may/ not acquire a reference to the imported Screen
-- buffer. Therefore, the application /must/ ensure that the object
-- referred to by @buffer@ stays valid as long as the device memory to
-- which it is imported is being used.
--
-- == Valid Usage
--
-- -   #VUID-VkImportScreenBufferInfoQNX-buffer-08966# If @buffer@ is not
--     @NULL@, QNX Screen Buffers /must/ be supported for import, as
--     reported by
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalImageFormatProperties'
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalBufferProperties'
--
-- -   #VUID-VkImportScreenBufferInfoQNX-buffer-08967# @buffer@ is not
--     @NULL@, it /must/ be a pointer to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-external-screen-buffer-validity valid QNX Screen buffer>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImportScreenBufferInfoQNX-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_SCREEN_BUFFER_INFO_QNX'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QNX_external_memory_screen_buffer VK_QNX_external_memory_screen_buffer>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportScreenBufferInfoQNX = ImportScreenBufferInfoQNX
  { -- | @buffer@ is a pointer to a @struct@ 'Screen_buffer', the QNX Screen
    -- buffer to import
    buffer :: Ptr Screen_buffer }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportScreenBufferInfoQNX)
#endif
deriving instance Show ImportScreenBufferInfoQNX

instance ToCStruct ImportScreenBufferInfoQNX where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportScreenBufferInfoQNX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_SCREEN_BUFFER_INFO_QNX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Screen_buffer))) (buffer)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_SCREEN_BUFFER_INFO_QNX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Screen_buffer))) (zero)
    f

instance FromCStruct ImportScreenBufferInfoQNX where
  peekCStruct p = do
    buffer <- peek @(Ptr Screen_buffer) ((p `plusPtr` 16 :: Ptr (Ptr Screen_buffer)))
    pure $ ImportScreenBufferInfoQNX
             buffer

instance Storable ImportScreenBufferInfoQNX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportScreenBufferInfoQNX where
  zero = ImportScreenBufferInfoQNX
           zero


-- | VkScreenBufferPropertiesQNX - Properties of External Memory QNX Screen
-- Buffers
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkScreenBufferPropertiesQNX-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SCREEN_BUFFER_PROPERTIES_QNX'
--
-- -   #VUID-VkScreenBufferPropertiesQNX-pNext-pNext# @pNext@ /must/ be
--     @NULL@ or a pointer to a valid instance of
--     'ScreenBufferFormatPropertiesQNX'
--
-- -   #VUID-VkScreenBufferPropertiesQNX-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QNX_external_memory_screen_buffer VK_QNX_external_memory_screen_buffer>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getScreenBufferPropertiesQNX'
data ScreenBufferPropertiesQNX (es :: [Type]) = ScreenBufferPropertiesQNX
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @allocationSize@ is the size of the external memory.
    allocationSize :: DeviceSize
  , -- | @memoryTypeBits@ is a bitmask containing one bit set for every memory
    -- type which the specified Screen buffer /can/ be imported as.
    memoryTypeBits :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ScreenBufferPropertiesQNX (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ScreenBufferPropertiesQNX es)

instance Extensible ScreenBufferPropertiesQNX where
  extensibleTypeName = "ScreenBufferPropertiesQNX"
  setNext ScreenBufferPropertiesQNX{..} next' = ScreenBufferPropertiesQNX{next = next', ..}
  getNext ScreenBufferPropertiesQNX{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ScreenBufferPropertiesQNX e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ScreenBufferFormatPropertiesQNX = Just f
    | otherwise = Nothing

instance ( Extendss ScreenBufferPropertiesQNX es
         , PokeChain es ) => ToCStruct (ScreenBufferPropertiesQNX es) where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ScreenBufferPropertiesQNX{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SCREEN_BUFFER_PROPERTIES_QNX)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (allocationSize)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (memoryTypeBits)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SCREEN_BUFFER_PROPERTIES_QNX)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ f

instance ( Extendss ScreenBufferPropertiesQNX es
         , PeekChain es ) => FromCStruct (ScreenBufferPropertiesQNX es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    allocationSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ ScreenBufferPropertiesQNX
             next allocationSize memoryTypeBits

instance es ~ '[] => Zero (ScreenBufferPropertiesQNX es) where
  zero = ScreenBufferPropertiesQNX
           ()
           zero
           zero


-- | VkScreenBufferFormatPropertiesQNX - Structure describing the image
-- format properties of a QNX Screen buffer
--
-- = Description
--
-- If the QNX Screen buffer has one of the formats listed in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-external-qnx-screen-buffer-formats QNX Screen Format Equivalence table>,
-- then @format@ /must/ have the equivalent Vulkan format listed in the
-- table. Otherwise, @format@ /may/ be
-- 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', indicating the QNX Screen
-- buffer /can/ only be used with an external format. The @formatFeatures@
-- member /must/ include
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
-- and /should/ include
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
-- and
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QNX_external_memory_screen_buffer VK_QNX_external_memory_screen_buffer>,
-- 'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation',
-- 'Vulkan.Core10.ImageView.ComponentMapping',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlags',
-- 'Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SamplerYcbcrModelConversion',
-- 'Vulkan.Core11.Enums.SamplerYcbcrRange.SamplerYcbcrRange',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ScreenBufferFormatPropertiesQNX = ScreenBufferFormatPropertiesQNX
  { -- | @format@ is the Vulkan format corresponding to the Screen buffer’s
    -- format or 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED' if there is not
    -- an equivalent Vulkan format.
    format :: Format
  , -- | @externalFormat@ is an implementation-defined external format identifier
    -- for use with 'ExternalFormatQNX'. It /must/ not be zero.
    externalFormat :: Word64
  , -- | @screenUsage@ is an implementation-defined external usage identifier for
    -- the QNX Screen buffer.
    screenUsage :: Word64
  , -- | @formatFeatures@ describes the capabilities of this external format when
    -- used with an image bound to memory imported from @buffer@.
    formatFeatures :: FormatFeatureFlags
  , -- | @samplerYcbcrConversionComponents@ is the component swizzle that
    -- /should/ be used in
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'.
    samplerYcbcrConversionComponents :: ComponentMapping
  , -- | @suggestedYcbcrModel@ is a suggested color model to use in the
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'.
    suggestedYcbcrModel :: SamplerYcbcrModelConversion
  , -- | @suggestedYcbcrRange@ is a suggested numerical value range to use in
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'.
    suggestedYcbcrRange :: SamplerYcbcrRange
  , -- | @suggestedXChromaOffset@ is a suggested X chroma offset to use in
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'.
    suggestedXChromaOffset :: ChromaLocation
  , -- | @suggestedYChromaOffset@ is a suggested Y chroma offset to use in
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'.
    suggestedYChromaOffset :: ChromaLocation
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ScreenBufferFormatPropertiesQNX)
#endif
deriving instance Show ScreenBufferFormatPropertiesQNX

instance ToCStruct ScreenBufferFormatPropertiesQNX where
  withCStruct x f = allocaBytes 80 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ScreenBufferFormatPropertiesQNX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SCREEN_BUFFER_FORMAT_PROPERTIES_QNX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (format)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (externalFormat)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (screenUsage)
    poke ((p `plusPtr` 40 :: Ptr FormatFeatureFlags)) (formatFeatures)
    poke ((p `plusPtr` 44 :: Ptr ComponentMapping)) (samplerYcbcrConversionComponents)
    poke ((p `plusPtr` 60 :: Ptr SamplerYcbcrModelConversion)) (suggestedYcbcrModel)
    poke ((p `plusPtr` 64 :: Ptr SamplerYcbcrRange)) (suggestedYcbcrRange)
    poke ((p `plusPtr` 68 :: Ptr ChromaLocation)) (suggestedXChromaOffset)
    poke ((p `plusPtr` 72 :: Ptr ChromaLocation)) (suggestedYChromaOffset)
    f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SCREEN_BUFFER_FORMAT_PROPERTIES_QNX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr FormatFeatureFlags)) (zero)
    poke ((p `plusPtr` 44 :: Ptr ComponentMapping)) (zero)
    poke ((p `plusPtr` 60 :: Ptr SamplerYcbcrModelConversion)) (zero)
    poke ((p `plusPtr` 64 :: Ptr SamplerYcbcrRange)) (zero)
    poke ((p `plusPtr` 68 :: Ptr ChromaLocation)) (zero)
    poke ((p `plusPtr` 72 :: Ptr ChromaLocation)) (zero)
    f

instance FromCStruct ScreenBufferFormatPropertiesQNX where
  peekCStruct p = do
    format <- peek @Format ((p `plusPtr` 16 :: Ptr Format))
    externalFormat <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    screenUsage <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    formatFeatures <- peek @FormatFeatureFlags ((p `plusPtr` 40 :: Ptr FormatFeatureFlags))
    samplerYcbcrConversionComponents <- peekCStruct @ComponentMapping ((p `plusPtr` 44 :: Ptr ComponentMapping))
    suggestedYcbcrModel <- peek @SamplerYcbcrModelConversion ((p `plusPtr` 60 :: Ptr SamplerYcbcrModelConversion))
    suggestedYcbcrRange <- peek @SamplerYcbcrRange ((p `plusPtr` 64 :: Ptr SamplerYcbcrRange))
    suggestedXChromaOffset <- peek @ChromaLocation ((p `plusPtr` 68 :: Ptr ChromaLocation))
    suggestedYChromaOffset <- peek @ChromaLocation ((p `plusPtr` 72 :: Ptr ChromaLocation))
    pure $ ScreenBufferFormatPropertiesQNX
             format
             externalFormat
             screenUsage
             formatFeatures
             samplerYcbcrConversionComponents
             suggestedYcbcrModel
             suggestedYcbcrRange
             suggestedXChromaOffset
             suggestedYChromaOffset

instance Storable ScreenBufferFormatPropertiesQNX where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ScreenBufferFormatPropertiesQNX where
  zero = ScreenBufferFormatPropertiesQNX
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkExternalFormatQNX - Structure containing a QNX Screen buffer external
-- format
--
-- = Description
--
-- If @externalFormat@ is zero, the effect is as if the 'ExternalFormatQNX'
-- structure was not present. Otherwise, the @image@ will have the
-- specified external format.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QNX_external_memory_screen_buffer VK_QNX_external_memory_screen_buffer>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExternalFormatQNX = ExternalFormatQNX
  { -- | @externalFormat@ is an implementation-defined identifier for the
    -- external format
    --
    -- #VUID-VkExternalFormatQNX-externalFormat-08956# @externalFormat@ /must/
    -- be @0@ or a value returned in the @externalFormat@ member of
    -- 'ScreenBufferFormatPropertiesQNX' by an earlier call to
    -- 'getScreenBufferPropertiesQNX'
    externalFormat :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalFormatQNX)
#endif
deriving instance Show ExternalFormatQNX

instance ToCStruct ExternalFormatQNX where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalFormatQNX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_FORMAT_QNX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (externalFormat)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_FORMAT_QNX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    f

instance FromCStruct ExternalFormatQNX where
  peekCStruct p = do
    externalFormat <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    pure $ ExternalFormatQNX
             externalFormat

instance Storable ExternalFormatQNX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalFormatQNX where
  zero = ExternalFormatQNX
           zero


-- | VkPhysicalDeviceExternalMemoryScreenBufferFeaturesQNX - Structure
-- describing QNX Screen Buffer features that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX'
-- structure describe the following features:
--
-- = Description
--
-- \'
--
-- +-----------------------------------+------------------------------------+
-- | Features                          | Functionality                      |
-- +-----------------------------------+------------------------------------+
-- | @screenBufferImport@              | 'ImportScreenBufferInfoQNX'        |
-- +-----------------------------------+------------------------------------+
-- | Always supported1                 | 'getScreenBufferPropertiesQNX',    |
-- |                                   | 'ScreenBufferPropertiesQNX',       |
-- |                                   | 'ScreenBufferFormatPropertiesQNX', |
-- |                                   | 'ExternalFormatQNX'                |
-- +-----------------------------------+------------------------------------+
--
-- Functionality supported for QNX Screen Buffer features
--
-- [1]
--     Functionality in this row is always available.
--
-- The
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-externalscreenbuffer-table Functionality supported for QNX Screen buffer features>
-- table summarizes the functionality enabled by the
-- 'PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX' structure. Each
-- entry in the body of the table summarizes the functionality that /can/
-- be used when the given features are supported and enabled. This
-- summarizes Valid Usage statements that are added elsewhere in this
-- specification.
--
-- If the 'PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QNX_external_memory_screen_buffer VK_QNX_external_memory_screen_buffer>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX = PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX
  { -- | #features-screenBufferImport# @screenBufferImport@ indicates whether QNX
    -- Screen buffer import functionality is supported. If @screenBufferImport@
    -- is set to 'Vulkan.Core10.FundamentalTypes.TRUE',
    -- 'Vulkan.Core10.Handles.DeviceMemory' supports importing 'Screen_buffer'
    -- from applications. In this case, the application is responsible for the
    -- resource management of the 'Screen_buffer'.
    screenBufferImport :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX)
#endif
deriving instance Show PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX

instance ToCStruct PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_SCREEN_BUFFER_FEATURES_QNX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (screenBufferImport))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_SCREEN_BUFFER_FEATURES_QNX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX where
  peekCStruct p = do
    screenBufferImport <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX
             (bool32ToBool screenBufferImport)

instance Storable PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX where
  zero = PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX
           zero


type QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_SPEC_VERSION"
pattern QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_SPEC_VERSION :: forall a . Integral a => a
pattern QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_SPEC_VERSION = 1


type QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_EXTENSION_NAME = "VK_QNX_external_memory_screen_buffer"

-- No documentation found for TopLevel "VK_QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_EXTENSION_NAME"
pattern QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QNX_EXTERNAL_MEMORY_SCREEN_BUFFER_EXTENSION_NAME = "VK_QNX_external_memory_screen_buffer"


data Screen_buffer

