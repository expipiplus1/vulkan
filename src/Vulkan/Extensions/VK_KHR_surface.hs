{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_surface"
module Vulkan.Extensions.VK_KHR_surface  ( destroySurfaceKHR
                                         , getPhysicalDeviceSurfaceSupportKHR
                                         , getPhysicalDeviceSurfaceCapabilitiesKHR
                                         , getPhysicalDeviceSurfaceFormatsKHR
                                         , getPhysicalDeviceSurfacePresentModesKHR
                                         , pattern COLORSPACE_SRGB_NONLINEAR_KHR
                                         , SurfaceCapabilitiesKHR(..)
                                         , SurfaceFormatKHR(..)
                                         , PresentModeKHR( PRESENT_MODE_IMMEDIATE_KHR
                                                         , PRESENT_MODE_MAILBOX_KHR
                                                         , PRESENT_MODE_FIFO_KHR
                                                         , PRESENT_MODE_FIFO_RELAXED_KHR
                                                         , PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
                                                         , PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
                                                         , ..
                                                         )
                                         , ColorSpaceKHR( COLOR_SPACE_SRGB_NONLINEAR_KHR
                                                        , COLOR_SPACE_DISPLAY_NATIVE_AMD
                                                        , COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT
                                                        , COLOR_SPACE_PASS_THROUGH_EXT
                                                        , COLOR_SPACE_ADOBERGB_NONLINEAR_EXT
                                                        , COLOR_SPACE_ADOBERGB_LINEAR_EXT
                                                        , COLOR_SPACE_HDR10_HLG_EXT
                                                        , COLOR_SPACE_DOLBYVISION_EXT
                                                        , COLOR_SPACE_HDR10_ST2084_EXT
                                                        , COLOR_SPACE_BT2020_LINEAR_EXT
                                                        , COLOR_SPACE_BT709_NONLINEAR_EXT
                                                        , COLOR_SPACE_BT709_LINEAR_EXT
                                                        , COLOR_SPACE_DCI_P3_NONLINEAR_EXT
                                                        , COLOR_SPACE_DISPLAY_P3_LINEAR_EXT
                                                        , COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT
                                                        , COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT
                                                        , ..
                                                        )
                                         , CompositeAlphaFlagsKHR
                                         , CompositeAlphaFlagBitsKHR( COMPOSITE_ALPHA_OPAQUE_BIT_KHR
                                                                    , COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
                                                                    , COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
                                                                    , COMPOSITE_ALPHA_INHERIT_BIT_KHR
                                                                    , ..
                                                                    )
                                         , SurfaceTransformFlagsKHR
                                         , SurfaceTransformFlagBitsKHR( SURFACE_TRANSFORM_IDENTITY_BIT_KHR
                                                                      , SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
                                                                      , SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
                                                                      , SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
                                                                      , SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
                                                                      , SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
                                                                      , SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
                                                                      , SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
                                                                      , SURFACE_TRANSFORM_INHERIT_BIT_KHR
                                                                      , ..
                                                                      )
                                         , KHR_SURFACE_SPEC_VERSION
                                         , pattern KHR_SURFACE_SPEC_VERSION
                                         , KHR_SURFACE_EXTENSION_NAME
                                         , pattern KHR_SURFACE_EXTENSION_NAME
                                         , SurfaceKHR(..)
                                         ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkDestroySurfaceKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceCapabilitiesKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceFormatsKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfacePresentModesKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceSupportKHR))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySurfaceKHR
  :: FunPtr (Ptr Instance_T -> SurfaceKHR -> Ptr AllocationCallbacks -> IO ()) -> Ptr Instance_T -> SurfaceKHR -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroySurfaceKHR"
destroySurfaceKHR :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkDestroySurfaceKHR" "instance"
                     Instance
                  -> -- No documentation found for Nested "vkDestroySurfaceKHR" "surface"
                     SurfaceKHR
                  -> -- No documentation found for Nested "vkDestroySurfaceKHR" "pAllocator"
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io ()
destroySurfaceKHR instance' surface allocator = liftIO . evalContT $ do
  let vkDestroySurfaceKHRPtr = pVkDestroySurfaceKHR (instanceCmds (instance' :: Instance))
  lift $ unless (vkDestroySurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroySurfaceKHR is null" Nothing Nothing
  let vkDestroySurfaceKHR' = mkVkDestroySurfaceKHR vkDestroySurfaceKHRPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroySurfaceKHR' (instanceHandle (instance')) (surface) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceSupportKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> SurfaceKHR -> Ptr Bool32 -> IO Result) -> Ptr PhysicalDevice_T -> Word32 -> SurfaceKHR -> Ptr Bool32 -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceSupportKHR"
getPhysicalDeviceSurfaceSupportKHR :: forall io
                                    . (MonadIO io)
                                   => -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceSupportKHR" "physicalDevice"
                                      PhysicalDevice
                                   -> -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceSupportKHR" "queueFamilyIndex"
                                      ("queueFamilyIndex" ::: Word32)
                                   -> -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceSupportKHR" "surface"
                                      SurfaceKHR
                                   -> io (("supported" ::: Bool))
getPhysicalDeviceSurfaceSupportKHR physicalDevice queueFamilyIndex surface = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceSupportKHRPtr = pVkGetPhysicalDeviceSurfaceSupportKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfaceSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfaceSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfaceSupportKHR' = mkVkGetPhysicalDeviceSurfaceSupportKHR vkGetPhysicalDeviceSurfaceSupportKHRPtr
  pPSupported <- ContT $ bracket (callocBytes @Bool32 4) free
  r <- lift $ vkGetPhysicalDeviceSurfaceSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (surface) (pPSupported)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSupported <- lift $ peek @Bool32 pPSupported
  pure $ ((bool32ToBool pSupported))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceCapabilitiesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr SurfaceCapabilitiesKHR -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr SurfaceCapabilitiesKHR -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
getPhysicalDeviceSurfaceCapabilitiesKHR :: forall io
                                         . (MonadIO io)
                                        => -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceCapabilitiesKHR" "physicalDevice"
                                           PhysicalDevice
                                        -> -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceCapabilitiesKHR" "surface"
                                           SurfaceKHR
                                        -> io (SurfaceCapabilitiesKHR)
getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceCapabilitiesKHRPtr = pVkGetPhysicalDeviceSurfaceCapabilitiesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfaceCapabilitiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfaceCapabilitiesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfaceCapabilitiesKHR' = mkVkGetPhysicalDeviceSurfaceCapabilitiesKHR vkGetPhysicalDeviceSurfaceCapabilitiesKHRPtr
  pPSurfaceCapabilities <- ContT (withZeroCStruct @SurfaceCapabilitiesKHR)
  r <- lift $ vkGetPhysicalDeviceSurfaceCapabilitiesKHR' (physicalDeviceHandle (physicalDevice)) (surface) (pPSurfaceCapabilities)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceCapabilities <- lift $ peekCStruct @SurfaceCapabilitiesKHR pPSurfaceCapabilities
  pure $ (pSurfaceCapabilities)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceFormatsKHR
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr SurfaceFormatKHR -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr SurfaceFormatKHR -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceFormatsKHR"
getPhysicalDeviceSurfaceFormatsKHR :: forall io
                                    . (MonadIO io)
                                   => -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceFormatsKHR" "physicalDevice"
                                      PhysicalDevice
                                   -> -- No documentation found for Nested "vkGetPhysicalDeviceSurfaceFormatsKHR" "surface"
                                      SurfaceKHR
                                   -> io (Result, ("surfaceFormats" ::: Vector SurfaceFormatKHR))
getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceFormatsKHRPtr = pVkGetPhysicalDeviceSurfaceFormatsKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfaceFormatsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfaceFormatsKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfaceFormatsKHR' = mkVkGetPhysicalDeviceSurfaceFormatsKHR vkGetPhysicalDeviceSurfaceFormatsKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPSurfaceFormatCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceSurfaceFormatsKHR' physicalDevice' (surface) (pPSurfaceFormatCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceFormatCount <- lift $ peek @Word32 pPSurfaceFormatCount
  pPSurfaceFormats <- ContT $ bracket (callocBytes @SurfaceFormatKHR ((fromIntegral (pSurfaceFormatCount)) * 8)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPSurfaceFormats `advancePtrBytes` (i * 8) :: Ptr SurfaceFormatKHR) . ($ ())) [0..(fromIntegral (pSurfaceFormatCount)) - 1]
  r' <- lift $ vkGetPhysicalDeviceSurfaceFormatsKHR' physicalDevice' (surface) (pPSurfaceFormatCount) ((pPSurfaceFormats))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pSurfaceFormatCount' <- lift $ peek @Word32 pPSurfaceFormatCount
  pSurfaceFormats' <- lift $ generateM (fromIntegral (pSurfaceFormatCount')) (\i -> peekCStruct @SurfaceFormatKHR (((pPSurfaceFormats) `advancePtrBytes` (8 * (i)) :: Ptr SurfaceFormatKHR)))
  pure $ ((r'), pSurfaceFormats')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfacePresentModesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr PresentModeKHR -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr PresentModeKHR -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfacePresentModesKHR"
getPhysicalDeviceSurfacePresentModesKHR :: forall io
                                         . (MonadIO io)
                                        => -- No documentation found for Nested "vkGetPhysicalDeviceSurfacePresentModesKHR" "physicalDevice"
                                           PhysicalDevice
                                        -> -- No documentation found for Nested "vkGetPhysicalDeviceSurfacePresentModesKHR" "surface"
                                           SurfaceKHR
                                        -> io (Result, ("presentModes" ::: Vector PresentModeKHR))
getPhysicalDeviceSurfacePresentModesKHR physicalDevice surface = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfacePresentModesKHRPtr = pVkGetPhysicalDeviceSurfacePresentModesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfacePresentModesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfacePresentModesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfacePresentModesKHR' = mkVkGetPhysicalDeviceSurfacePresentModesKHR vkGetPhysicalDeviceSurfacePresentModesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPresentModeCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceSurfacePresentModesKHR' physicalDevice' (surface) (pPPresentModeCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPresentModeCount <- lift $ peek @Word32 pPPresentModeCount
  pPPresentModes <- ContT $ bracket (callocBytes @PresentModeKHR ((fromIntegral (pPresentModeCount)) * 4)) free
  r' <- lift $ vkGetPhysicalDeviceSurfacePresentModesKHR' physicalDevice' (surface) (pPPresentModeCount) (pPPresentModes)
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPresentModeCount' <- lift $ peek @Word32 pPPresentModeCount
  pPresentModes' <- lift $ generateM (fromIntegral (pPresentModeCount')) (\i -> peek @PresentModeKHR ((pPPresentModes `advancePtrBytes` (4 * (i)) :: Ptr PresentModeKHR)))
  pure $ ((r'), pPresentModes')


-- No documentation found for TopLevel "VK_COLORSPACE_SRGB_NONLINEAR_KHR"
pattern COLORSPACE_SRGB_NONLINEAR_KHR = COLOR_SPACE_SRGB_NONLINEAR_KHR



-- No documentation found for TopLevel "VkSurfaceCapabilitiesKHR"
data SurfaceCapabilitiesKHR = SurfaceCapabilitiesKHR
  { -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "minImageCount"
    minImageCount :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "maxImageCount"
    maxImageCount :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "currentExtent"
    currentExtent :: Extent2D
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "minImageExtent"
    minImageExtent :: Extent2D
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "maxImageExtent"
    maxImageExtent :: Extent2D
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "maxImageArrayLayers"
    maxImageArrayLayers :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "supportedTransforms"
    supportedTransforms :: SurfaceTransformFlagsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "currentTransform"
    currentTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "supportedCompositeAlpha"
    supportedCompositeAlpha :: CompositeAlphaFlagsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "supportedUsageFlags"
    supportedUsageFlags :: ImageUsageFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceCapabilitiesKHR)
#endif
deriving instance Show SurfaceCapabilitiesKHR

instance ToCStruct SurfaceCapabilitiesKHR where
  withCStruct x f = allocaBytesAligned 52 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilitiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (minImageCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (maxImageCount)
    poke ((p `plusPtr` 8 :: Ptr Extent2D)) (currentExtent)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (minImageExtent)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (maxImageExtent)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxImageArrayLayers)
    poke ((p `plusPtr` 36 :: Ptr SurfaceTransformFlagsKHR)) (supportedTransforms)
    poke ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR)) (currentTransform)
    poke ((p `plusPtr` 44 :: Ptr CompositeAlphaFlagsKHR)) (supportedCompositeAlpha)
    poke ((p `plusPtr` 48 :: Ptr ImageUsageFlags)) (supportedUsageFlags)
    f
  cStructSize = 52
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    f

instance FromCStruct SurfaceCapabilitiesKHR where
  peekCStruct p = do
    minImageCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    maxImageCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    currentExtent <- peekCStruct @Extent2D ((p `plusPtr` 8 :: Ptr Extent2D))
    minImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    maxImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    maxImageArrayLayers <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    supportedTransforms <- peek @SurfaceTransformFlagsKHR ((p `plusPtr` 36 :: Ptr SurfaceTransformFlagsKHR))
    currentTransform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR))
    supportedCompositeAlpha <- peek @CompositeAlphaFlagsKHR ((p `plusPtr` 44 :: Ptr CompositeAlphaFlagsKHR))
    supportedUsageFlags <- peek @ImageUsageFlags ((p `plusPtr` 48 :: Ptr ImageUsageFlags))
    pure $ SurfaceCapabilitiesKHR
             minImageCount maxImageCount currentExtent minImageExtent maxImageExtent maxImageArrayLayers supportedTransforms currentTransform supportedCompositeAlpha supportedUsageFlags


instance Storable SurfaceCapabilitiesKHR where
  sizeOf ~_ = 52
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceCapabilitiesKHR where
  zero = SurfaceCapabilitiesKHR
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



-- No documentation found for TopLevel "VkSurfaceFormatKHR"
data SurfaceFormatKHR = SurfaceFormatKHR
  { -- No documentation found for Nested "VkSurfaceFormatKHR" "format"
    format :: Format
  , -- No documentation found for Nested "VkSurfaceFormatKHR" "colorSpace"
    colorSpace :: ColorSpaceKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceFormatKHR)
#endif
deriving instance Show SurfaceFormatKHR

instance ToCStruct SurfaceFormatKHR where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceFormatKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Format)) (format)
    poke ((p `plusPtr` 4 :: Ptr ColorSpaceKHR)) (colorSpace)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ColorSpaceKHR)) (zero)
    f

instance FromCStruct SurfaceFormatKHR where
  peekCStruct p = do
    format <- peek @Format ((p `plusPtr` 0 :: Ptr Format))
    colorSpace <- peek @ColorSpaceKHR ((p `plusPtr` 4 :: Ptr ColorSpaceKHR))
    pure $ SurfaceFormatKHR
             format colorSpace


instance Storable SurfaceFormatKHR where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceFormatKHR where
  zero = SurfaceFormatKHR
           zero
           zero


-- No documentation found for TopLevel "VkPresentModeKHR"
newtype PresentModeKHR = PresentModeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_IMMEDIATE_KHR"
pattern PRESENT_MODE_IMMEDIATE_KHR                 = PresentModeKHR 0
-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_MAILBOX_KHR"
pattern PRESENT_MODE_MAILBOX_KHR                   = PresentModeKHR 1
-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_FIFO_KHR"
pattern PRESENT_MODE_FIFO_KHR                      = PresentModeKHR 2
-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_FIFO_RELAXED_KHR"
pattern PRESENT_MODE_FIFO_RELAXED_KHR              = PresentModeKHR 3
-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR"
pattern PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR = PresentModeKHR 1000111001
-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR"
pattern PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR     = PresentModeKHR 1000111000
{-# complete PRESENT_MODE_IMMEDIATE_KHR,
             PRESENT_MODE_MAILBOX_KHR,
             PRESENT_MODE_FIFO_KHR,
             PRESENT_MODE_FIFO_RELAXED_KHR,
             PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR,
             PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR :: PresentModeKHR #-}

conNamePresentModeKHR :: String
conNamePresentModeKHR = "PresentModeKHR"

enumPrefixPresentModeKHR :: String
enumPrefixPresentModeKHR = "PRESENT_MODE_"

showTablePresentModeKHR :: [(PresentModeKHR, String)]
showTablePresentModeKHR =
  [ (PRESENT_MODE_IMMEDIATE_KHR                , "IMMEDIATE_KHR")
  , (PRESENT_MODE_MAILBOX_KHR                  , "MAILBOX_KHR")
  , (PRESENT_MODE_FIFO_KHR                     , "FIFO_KHR")
  , (PRESENT_MODE_FIFO_RELAXED_KHR             , "FIFO_RELAXED_KHR")
  , (PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR, "SHARED_CONTINUOUS_REFRESH_KHR")
  , (PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR    , "SHARED_DEMAND_REFRESH_KHR")
  ]


instance Show PresentModeKHR where
showsPrec = enumShowsPrec enumPrefixPresentModeKHR
                          showTablePresentModeKHR
                          conNamePresentModeKHR
                          (\(PresentModeKHR x) -> x)
                          (showsPrec 11)


instance Read PresentModeKHR where
  readPrec = enumReadPrec enumPrefixPresentModeKHR showTablePresentModeKHR conNamePresentModeKHR PresentModeKHR


-- No documentation found for TopLevel "VkColorSpaceKHR"
newtype ColorSpaceKHR = ColorSpaceKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_SRGB_NONLINEAR_KHR"
pattern COLOR_SPACE_SRGB_NONLINEAR_KHR          = ColorSpaceKHR 0
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_DISPLAY_NATIVE_AMD"
pattern COLOR_SPACE_DISPLAY_NATIVE_AMD          = ColorSpaceKHR 1000213000
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT"
pattern COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT = ColorSpaceKHR 1000104014
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_PASS_THROUGH_EXT"
pattern COLOR_SPACE_PASS_THROUGH_EXT            = ColorSpaceKHR 1000104013
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT"
pattern COLOR_SPACE_ADOBERGB_NONLINEAR_EXT      = ColorSpaceKHR 1000104012
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT"
pattern COLOR_SPACE_ADOBERGB_LINEAR_EXT         = ColorSpaceKHR 1000104011
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_HDR10_HLG_EXT"
pattern COLOR_SPACE_HDR10_HLG_EXT               = ColorSpaceKHR 1000104010
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_DOLBYVISION_EXT"
pattern COLOR_SPACE_DOLBYVISION_EXT             = ColorSpaceKHR 1000104009
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_HDR10_ST2084_EXT"
pattern COLOR_SPACE_HDR10_ST2084_EXT            = ColorSpaceKHR 1000104008
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_BT2020_LINEAR_EXT"
pattern COLOR_SPACE_BT2020_LINEAR_EXT           = ColorSpaceKHR 1000104007
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_BT709_NONLINEAR_EXT"
pattern COLOR_SPACE_BT709_NONLINEAR_EXT         = ColorSpaceKHR 1000104006
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_BT709_LINEAR_EXT"
pattern COLOR_SPACE_BT709_LINEAR_EXT            = ColorSpaceKHR 1000104005
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT"
pattern COLOR_SPACE_DCI_P3_NONLINEAR_EXT        = ColorSpaceKHR 1000104004
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_DISPLAY_P3_LINEAR_EXT"
pattern COLOR_SPACE_DISPLAY_P3_LINEAR_EXT       = ColorSpaceKHR 1000104003
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT"
pattern COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT    = ColorSpaceKHR 1000104002
-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT"
pattern COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT    = ColorSpaceKHR 1000104001
{-# complete COLOR_SPACE_SRGB_NONLINEAR_KHR,
             COLOR_SPACE_DISPLAY_NATIVE_AMD,
             COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT,
             COLOR_SPACE_PASS_THROUGH_EXT,
             COLOR_SPACE_ADOBERGB_NONLINEAR_EXT,
             COLOR_SPACE_ADOBERGB_LINEAR_EXT,
             COLOR_SPACE_HDR10_HLG_EXT,
             COLOR_SPACE_DOLBYVISION_EXT,
             COLOR_SPACE_HDR10_ST2084_EXT,
             COLOR_SPACE_BT2020_LINEAR_EXT,
             COLOR_SPACE_BT709_NONLINEAR_EXT,
             COLOR_SPACE_BT709_LINEAR_EXT,
             COLOR_SPACE_DCI_P3_NONLINEAR_EXT,
             COLOR_SPACE_DISPLAY_P3_LINEAR_EXT,
             COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT,
             COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT :: ColorSpaceKHR #-}

conNameColorSpaceKHR :: String
conNameColorSpaceKHR = "ColorSpaceKHR"

enumPrefixColorSpaceKHR :: String
enumPrefixColorSpaceKHR = "COLOR_SPACE_"

showTableColorSpaceKHR :: [(ColorSpaceKHR, String)]
showTableColorSpaceKHR =
  [ (COLOR_SPACE_SRGB_NONLINEAR_KHR         , "SRGB_NONLINEAR_KHR")
  , (COLOR_SPACE_DISPLAY_NATIVE_AMD         , "DISPLAY_NATIVE_AMD")
  , (COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT, "EXTENDED_SRGB_NONLINEAR_EXT")
  , (COLOR_SPACE_PASS_THROUGH_EXT           , "PASS_THROUGH_EXT")
  , (COLOR_SPACE_ADOBERGB_NONLINEAR_EXT     , "ADOBERGB_NONLINEAR_EXT")
  , (COLOR_SPACE_ADOBERGB_LINEAR_EXT        , "ADOBERGB_LINEAR_EXT")
  , (COLOR_SPACE_HDR10_HLG_EXT              , "HDR10_HLG_EXT")
  , (COLOR_SPACE_DOLBYVISION_EXT            , "DOLBYVISION_EXT")
  , (COLOR_SPACE_HDR10_ST2084_EXT           , "HDR10_ST2084_EXT")
  , (COLOR_SPACE_BT2020_LINEAR_EXT          , "BT2020_LINEAR_EXT")
  , (COLOR_SPACE_BT709_NONLINEAR_EXT        , "BT709_NONLINEAR_EXT")
  , (COLOR_SPACE_BT709_LINEAR_EXT           , "BT709_LINEAR_EXT")
  , (COLOR_SPACE_DCI_P3_NONLINEAR_EXT       , "DCI_P3_NONLINEAR_EXT")
  , (COLOR_SPACE_DISPLAY_P3_LINEAR_EXT      , "DISPLAY_P3_LINEAR_EXT")
  , (COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT   , "EXTENDED_SRGB_LINEAR_EXT")
  , (COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT   , "DISPLAY_P3_NONLINEAR_EXT")
  ]


instance Show ColorSpaceKHR where
showsPrec = enumShowsPrec enumPrefixColorSpaceKHR
                          showTableColorSpaceKHR
                          conNameColorSpaceKHR
                          (\(ColorSpaceKHR x) -> x)
                          (showsPrec 11)


instance Read ColorSpaceKHR where
  readPrec = enumReadPrec enumPrefixColorSpaceKHR showTableColorSpaceKHR conNameColorSpaceKHR ColorSpaceKHR


type CompositeAlphaFlagsKHR = CompositeAlphaFlagBitsKHR

-- No documentation found for TopLevel "VkCompositeAlphaFlagBitsKHR"
newtype CompositeAlphaFlagBitsKHR = CompositeAlphaFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkCompositeAlphaFlagBitsKHR" "VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR"
pattern COMPOSITE_ALPHA_OPAQUE_BIT_KHR          = CompositeAlphaFlagBitsKHR 0x00000001
-- No documentation found for Nested "VkCompositeAlphaFlagBitsKHR" "VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR"
pattern COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR  = CompositeAlphaFlagBitsKHR 0x00000002
-- No documentation found for Nested "VkCompositeAlphaFlagBitsKHR" "VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR"
pattern COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = CompositeAlphaFlagBitsKHR 0x00000004
-- No documentation found for Nested "VkCompositeAlphaFlagBitsKHR" "VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR"
pattern COMPOSITE_ALPHA_INHERIT_BIT_KHR         = CompositeAlphaFlagBitsKHR 0x00000008

conNameCompositeAlphaFlagBitsKHR :: String
conNameCompositeAlphaFlagBitsKHR = "CompositeAlphaFlagBitsKHR"

enumPrefixCompositeAlphaFlagBitsKHR :: String
enumPrefixCompositeAlphaFlagBitsKHR = "COMPOSITE_ALPHA_"

showTableCompositeAlphaFlagBitsKHR :: [(CompositeAlphaFlagBitsKHR, String)]
showTableCompositeAlphaFlagBitsKHR =
  [ (COMPOSITE_ALPHA_OPAQUE_BIT_KHR         , "OPAQUE_BIT_KHR")
  , (COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR , "PRE_MULTIPLIED_BIT_KHR")
  , (COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR, "POST_MULTIPLIED_BIT_KHR")
  , (COMPOSITE_ALPHA_INHERIT_BIT_KHR        , "INHERIT_BIT_KHR")
  ]


instance Show CompositeAlphaFlagBitsKHR where
showsPrec = enumShowsPrec enumPrefixCompositeAlphaFlagBitsKHR
                          showTableCompositeAlphaFlagBitsKHR
                          conNameCompositeAlphaFlagBitsKHR
                          (\(CompositeAlphaFlagBitsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read CompositeAlphaFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixCompositeAlphaFlagBitsKHR
                          showTableCompositeAlphaFlagBitsKHR
                          conNameCompositeAlphaFlagBitsKHR
                          CompositeAlphaFlagBitsKHR


type SurfaceTransformFlagsKHR = SurfaceTransformFlagBitsKHR

-- No documentation found for TopLevel "VkSurfaceTransformFlagBitsKHR"
newtype SurfaceTransformFlagBitsKHR = SurfaceTransformFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR"
pattern SURFACE_TRANSFORM_IDENTITY_BIT_KHR                     = SurfaceTransformFlagBitsKHR 0x00000001
-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR"
pattern SURFACE_TRANSFORM_ROTATE_90_BIT_KHR                    = SurfaceTransformFlagBitsKHR 0x00000002
-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR"
pattern SURFACE_TRANSFORM_ROTATE_180_BIT_KHR                   = SurfaceTransformFlagBitsKHR 0x00000004
-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR"
pattern SURFACE_TRANSFORM_ROTATE_270_BIT_KHR                   = SurfaceTransformFlagBitsKHR 0x00000008
-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR"
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR            = SurfaceTransformFlagBitsKHR 0x00000010
-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR"
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR  = SurfaceTransformFlagBitsKHR 0x00000020
-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR"
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = SurfaceTransformFlagBitsKHR 0x00000040
-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR"
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = SurfaceTransformFlagBitsKHR 0x00000080
-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR"
pattern SURFACE_TRANSFORM_INHERIT_BIT_KHR                      = SurfaceTransformFlagBitsKHR 0x00000100

conNameSurfaceTransformFlagBitsKHR :: String
conNameSurfaceTransformFlagBitsKHR = "SurfaceTransformFlagBitsKHR"

enumPrefixSurfaceTransformFlagBitsKHR :: String
enumPrefixSurfaceTransformFlagBitsKHR = "SURFACE_TRANSFORM_"

showTableSurfaceTransformFlagBitsKHR :: [(SurfaceTransformFlagBitsKHR, String)]
showTableSurfaceTransformFlagBitsKHR =
  [ (SURFACE_TRANSFORM_IDENTITY_BIT_KHR                    , "IDENTITY_BIT_KHR")
  , (SURFACE_TRANSFORM_ROTATE_90_BIT_KHR                   , "ROTATE_90_BIT_KHR")
  , (SURFACE_TRANSFORM_ROTATE_180_BIT_KHR                  , "ROTATE_180_BIT_KHR")
  , (SURFACE_TRANSFORM_ROTATE_270_BIT_KHR                  , "ROTATE_270_BIT_KHR")
  , (SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR           , "HORIZONTAL_MIRROR_BIT_KHR")
  , (SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR , "HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR")
  , (SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR, "HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR")
  , (SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR, "HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR")
  , (SURFACE_TRANSFORM_INHERIT_BIT_KHR                     , "INHERIT_BIT_KHR")
  ]


instance Show SurfaceTransformFlagBitsKHR where
showsPrec = enumShowsPrec enumPrefixSurfaceTransformFlagBitsKHR
                          showTableSurfaceTransformFlagBitsKHR
                          conNameSurfaceTransformFlagBitsKHR
                          (\(SurfaceTransformFlagBitsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read SurfaceTransformFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixSurfaceTransformFlagBitsKHR
                          showTableSurfaceTransformFlagBitsKHR
                          conNameSurfaceTransformFlagBitsKHR
                          SurfaceTransformFlagBitsKHR


type KHR_SURFACE_SPEC_VERSION = 25

-- No documentation found for TopLevel "VK_KHR_SURFACE_SPEC_VERSION"
pattern KHR_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SURFACE_SPEC_VERSION = 25


type KHR_SURFACE_EXTENSION_NAME = "VK_KHR_surface"

-- No documentation found for TopLevel "VK_KHR_SURFACE_EXTENSION_NAME"
pattern KHR_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SURFACE_EXTENSION_NAME = "VK_KHR_surface"

