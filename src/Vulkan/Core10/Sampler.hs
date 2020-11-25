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
import Foreign.C.Types (CFloat)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
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
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
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

-- No documentation found for TopLevel "vkCreateSampler"
createSampler :: forall a io
               . (Extendss SamplerCreateInfo a, PokeChain a, MonadIO io)
              => -- No documentation found for Nested "vkCreateSampler" "device"
                 Device
              -> -- No documentation found for Nested "vkCreateSampler" "pCreateInfo"
                 (SamplerCreateInfo a)
              -> -- No documentation found for Nested "vkCreateSampler" "pAllocator"
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
  r <- lift $ vkCreateSampler' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPSampler)
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

-- No documentation found for TopLevel "vkDestroySampler"
destroySampler :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vkDestroySampler" "device"
                  Device
               -> -- No documentation found for Nested "vkDestroySampler" "sampler"
                  Sampler
               -> -- No documentation found for Nested "vkDestroySampler" "pAllocator"
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
  lift $ vkDestroySampler' (deviceHandle (device)) (sampler) pAllocator
  pure $ ()



-- No documentation found for TopLevel "VkSamplerCreateInfo"
data SamplerCreateInfo (es :: [Type]) = SamplerCreateInfo
  { -- No documentation found for Nested "VkSamplerCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkSamplerCreateInfo" "flags"
    flags :: SamplerCreateFlags
  , -- No documentation found for Nested "VkSamplerCreateInfo" "magFilter"
    magFilter :: Filter
  , -- No documentation found for Nested "VkSamplerCreateInfo" "minFilter"
    minFilter :: Filter
  , -- No documentation found for Nested "VkSamplerCreateInfo" "mipmapMode"
    mipmapMode :: SamplerMipmapMode
  , -- No documentation found for Nested "VkSamplerCreateInfo" "addressModeU"
    addressModeU :: SamplerAddressMode
  , -- No documentation found for Nested "VkSamplerCreateInfo" "addressModeV"
    addressModeV :: SamplerAddressMode
  , -- No documentation found for Nested "VkSamplerCreateInfo" "addressModeW"
    addressModeW :: SamplerAddressMode
  , -- No documentation found for Nested "VkSamplerCreateInfo" "mipLodBias"
    mipLodBias :: Float
  , -- No documentation found for Nested "VkSamplerCreateInfo" "anisotropyEnable"
    anisotropyEnable :: Bool
  , -- No documentation found for Nested "VkSamplerCreateInfo" "maxAnisotropy"
    maxAnisotropy :: Float
  , -- No documentation found for Nested "VkSamplerCreateInfo" "compareEnable"
    compareEnable :: Bool
  , -- No documentation found for Nested "VkSamplerCreateInfo" "compareOp"
    compareOp :: CompareOp
  , -- No documentation found for Nested "VkSamplerCreateInfo" "minLod"
    minLod :: Float
  , -- No documentation found for Nested "VkSamplerCreateInfo" "maxLod"
    maxLod :: Float
  , -- No documentation found for Nested "VkSamplerCreateInfo" "borderColor"
    borderColor :: BorderColor
  , -- No documentation found for Nested "VkSamplerCreateInfo" "unnormalizedCoordinates"
    unnormalizedCoordinates :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SamplerCreateInfo es)

instance Extensible SamplerCreateInfo where
  extensibleType = STRUCTURE_TYPE_SAMPLER_CREATE_INFO
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

