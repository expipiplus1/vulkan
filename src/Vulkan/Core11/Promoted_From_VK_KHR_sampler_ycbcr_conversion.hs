{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_sampler_ycbcr_conversion"
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

-- No documentation found for TopLevel "vkCreateSamplerYcbcrConversion"
createSamplerYcbcrConversion :: forall a io
                              . (Extendss SamplerYcbcrConversionCreateInfo a, PokeChain a, MonadIO io)
                             => -- No documentation found for Nested "vkCreateSamplerYcbcrConversion" "device"
                                Device
                             -> -- No documentation found for Nested "vkCreateSamplerYcbcrConversion" "pCreateInfo"
                                (SamplerYcbcrConversionCreateInfo a)
                             -> -- No documentation found for Nested "vkCreateSamplerYcbcrConversion" "pAllocator"
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
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSamplerYcbcrConversion :: forall a io r . (Extendss SamplerYcbcrConversionCreateInfo a, PokeChain a, MonadIO io) => Device -> SamplerYcbcrConversionCreateInfo a -> Maybe AllocationCallbacks -> (io SamplerYcbcrConversion -> (SamplerYcbcrConversion -> io ()) -> r) -> r
withSamplerYcbcrConversion device pCreateInfo pAllocator b =
  b (createSamplerYcbcrConversion device pCreateInfo pAllocator)
    (\(o0) -> destroySamplerYcbcrConversion device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySamplerYcbcrConversion
  :: FunPtr (Ptr Device_T -> SamplerYcbcrConversion -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> SamplerYcbcrConversion -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroySamplerYcbcrConversion"
destroySamplerYcbcrConversion :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkDestroySamplerYcbcrConversion" "device"
                                 Device
                              -> -- No documentation found for Nested "vkDestroySamplerYcbcrConversion" "ycbcrConversion"
                                 SamplerYcbcrConversion
                              -> -- No documentation found for Nested "vkDestroySamplerYcbcrConversion" "pAllocator"
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



-- No documentation found for TopLevel "VkSamplerYcbcrConversionInfo"
data SamplerYcbcrConversionInfo = SamplerYcbcrConversionInfo
  { -- No documentation found for Nested "VkSamplerYcbcrConversionInfo" "conversion"
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



-- No documentation found for TopLevel "VkSamplerYcbcrConversionCreateInfo"
data SamplerYcbcrConversionCreateInfo (es :: [Type]) = SamplerYcbcrConversionCreateInfo
  { -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "format"
    format :: Format
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "ycbcrModel"
    ycbcrModel :: SamplerYcbcrModelConversion
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "ycbcrRange"
    ycbcrRange :: SamplerYcbcrRange
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "components"
    components :: ComponentMapping
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "xChromaOffset"
    xChromaOffset :: ChromaLocation
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "yChromaOffset"
    yChromaOffset :: ChromaLocation
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "chromaFilter"
    chromaFilter :: Filter
  , -- No documentation found for Nested "VkSamplerYcbcrConversionCreateInfo" "forceExplicitReconstruction"
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
    lift $ poke ((p `plusPtr` 28 :: Ptr ComponentMapping)) (components)
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
    lift $ poke ((p `plusPtr` 28 :: Ptr ComponentMapping)) (zero)
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



-- No documentation found for TopLevel "VkBindImagePlaneMemoryInfo"
data BindImagePlaneMemoryInfo = BindImagePlaneMemoryInfo
  { -- No documentation found for Nested "VkBindImagePlaneMemoryInfo" "planeAspect"
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



-- No documentation found for TopLevel "VkImagePlaneMemoryRequirementsInfo"
data ImagePlaneMemoryRequirementsInfo = ImagePlaneMemoryRequirementsInfo
  { -- No documentation found for Nested "VkImagePlaneMemoryRequirementsInfo" "planeAspect"
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



-- No documentation found for TopLevel "VkPhysicalDeviceSamplerYcbcrConversionFeatures"
data PhysicalDeviceSamplerYcbcrConversionFeatures = PhysicalDeviceSamplerYcbcrConversionFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceSamplerYcbcrConversionFeatures" "samplerYcbcrConversion"
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



-- No documentation found for TopLevel "VkSamplerYcbcrConversionImageFormatProperties"
data SamplerYcbcrConversionImageFormatProperties = SamplerYcbcrConversionImageFormatProperties
  { -- No documentation found for Nested "VkSamplerYcbcrConversionImageFormatProperties" "combinedImageSamplerDescriptorCount"
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

