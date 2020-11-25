{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_display"
module Vulkan.Extensions.VK_KHR_display  ( getPhysicalDeviceDisplayPropertiesKHR
                                         , getPhysicalDeviceDisplayPlanePropertiesKHR
                                         , getDisplayPlaneSupportedDisplaysKHR
                                         , getDisplayModePropertiesKHR
                                         , createDisplayModeKHR
                                         , getDisplayPlaneCapabilitiesKHR
                                         , createDisplayPlaneSurfaceKHR
                                         , DisplayPropertiesKHR(..)
                                         , DisplayPlanePropertiesKHR(..)
                                         , DisplayModeParametersKHR(..)
                                         , DisplayModePropertiesKHR(..)
                                         , DisplayModeCreateInfoKHR(..)
                                         , DisplayPlaneCapabilitiesKHR(..)
                                         , DisplaySurfaceCreateInfoKHR(..)
                                         , DisplayModeCreateFlagsKHR(..)
                                         , DisplaySurfaceCreateFlagsKHR(..)
                                         , DisplayPlaneAlphaFlagsKHR
                                         , DisplayPlaneAlphaFlagBitsKHR( DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
                                                                       , DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
                                                                       , DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
                                                                       , DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
                                                                       , ..
                                                                       )
                                         , KHR_DISPLAY_SPEC_VERSION
                                         , pattern KHR_DISPLAY_SPEC_VERSION
                                         , KHR_DISPLAY_EXTENSION_NAME
                                         , pattern KHR_DISPLAY_EXTENSION_NAME
                                         , DisplayKHR(..)
                                         , DisplayModeKHR(..)
                                         , SurfaceKHR(..)
                                         , SurfaceTransformFlagBitsKHR(..)
                                         , SurfaceTransformFlagsKHR
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
import Numeric (showHex)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Extensions.Handles (DisplayKHR)
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Extensions.Handles (DisplayModeKHR)
import Vulkan.Extensions.Handles (DisplayModeKHR(..))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateDisplayModeKHR))
import Vulkan.Dynamic (InstanceCmds(pVkCreateDisplayPlaneSurfaceKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetDisplayModePropertiesKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetDisplayPlaneCapabilitiesKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetDisplayPlaneSupportedDisplaysKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceDisplayPlanePropertiesKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceDisplayPropertiesKHR))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.FundamentalTypes (Offset2D)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Extensions.Handles (DisplayModeKHR(..))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPropertiesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayPropertiesKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayPropertiesKHR -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayPropertiesKHR"
getPhysicalDeviceDisplayPropertiesKHR :: forall io
                                       . (MonadIO io)
                                      => -- No documentation found for Nested "vkGetPhysicalDeviceDisplayPropertiesKHR" "physicalDevice"
                                         PhysicalDevice
                                      -> io (Result, ("properties" ::: Vector DisplayPropertiesKHR))
getPhysicalDeviceDisplayPropertiesKHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceDisplayPropertiesKHRPtr = pVkGetPhysicalDeviceDisplayPropertiesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceDisplayPropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceDisplayPropertiesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceDisplayPropertiesKHR' = mkVkGetPhysicalDeviceDisplayPropertiesKHR vkGetPhysicalDeviceDisplayPropertiesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceDisplayPropertiesKHR' physicalDevice' (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @DisplayPropertiesKHR ((fromIntegral (pPropertyCount)) * 48)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 48) :: Ptr DisplayPropertiesKHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkGetPhysicalDeviceDisplayPropertiesKHR' physicalDevice' (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @DisplayPropertiesKHR (((pPProperties) `advancePtrBytes` (48 * (i)) :: Ptr DisplayPropertiesKHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPlanePropertiesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayPlanePropertiesKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayPlanePropertiesKHR -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"
getPhysicalDeviceDisplayPlanePropertiesKHR :: forall io
                                            . (MonadIO io)
                                           => -- No documentation found for Nested "vkGetPhysicalDeviceDisplayPlanePropertiesKHR" "physicalDevice"
                                              PhysicalDevice
                                           -> io (Result, ("properties" ::: Vector DisplayPlanePropertiesKHR))
getPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceDisplayPlanePropertiesKHRPtr = pVkGetPhysicalDeviceDisplayPlanePropertiesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceDisplayPlanePropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceDisplayPlanePropertiesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceDisplayPlanePropertiesKHR' = mkVkGetPhysicalDeviceDisplayPlanePropertiesKHR vkGetPhysicalDeviceDisplayPlanePropertiesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceDisplayPlanePropertiesKHR' physicalDevice' (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @DisplayPlanePropertiesKHR ((fromIntegral (pPropertyCount)) * 16)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 16) :: Ptr DisplayPlanePropertiesKHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkGetPhysicalDeviceDisplayPlanePropertiesKHR' physicalDevice' (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @DisplayPlanePropertiesKHR (((pPProperties) `advancePtrBytes` (16 * (i)) :: Ptr DisplayPlanePropertiesKHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneSupportedDisplaysKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr Word32 -> Ptr DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> Word32 -> Ptr Word32 -> Ptr DisplayKHR -> IO Result

-- No documentation found for TopLevel "vkGetDisplayPlaneSupportedDisplaysKHR"
getDisplayPlaneSupportedDisplaysKHR :: forall io
                                     . (MonadIO io)
                                    => -- No documentation found for Nested "vkGetDisplayPlaneSupportedDisplaysKHR" "physicalDevice"
                                       PhysicalDevice
                                    -> -- No documentation found for Nested "vkGetDisplayPlaneSupportedDisplaysKHR" "planeIndex"
                                       ("planeIndex" ::: Word32)
                                    -> io (Result, ("displays" ::: Vector DisplayKHR))
getDisplayPlaneSupportedDisplaysKHR physicalDevice planeIndex = liftIO . evalContT $ do
  let vkGetDisplayPlaneSupportedDisplaysKHRPtr = pVkGetDisplayPlaneSupportedDisplaysKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetDisplayPlaneSupportedDisplaysKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDisplayPlaneSupportedDisplaysKHR is null" Nothing Nothing
  let vkGetDisplayPlaneSupportedDisplaysKHR' = mkVkGetDisplayPlaneSupportedDisplaysKHR vkGetDisplayPlaneSupportedDisplaysKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPDisplayCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetDisplayPlaneSupportedDisplaysKHR' physicalDevice' (planeIndex) (pPDisplayCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDisplayCount <- lift $ peek @Word32 pPDisplayCount
  pPDisplays <- ContT $ bracket (callocBytes @DisplayKHR ((fromIntegral (pDisplayCount)) * 8)) free
  r' <- lift $ vkGetDisplayPlaneSupportedDisplaysKHR' physicalDevice' (planeIndex) (pPDisplayCount) (pPDisplays)
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pDisplayCount' <- lift $ peek @Word32 pPDisplayCount
  pDisplays' <- lift $ generateM (fromIntegral (pDisplayCount')) (\i -> peek @DisplayKHR ((pPDisplays `advancePtrBytes` (8 * (i)) :: Ptr DisplayKHR)))
  pure $ ((r'), pDisplays')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayModePropertiesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> Ptr Word32 -> Ptr DisplayModePropertiesKHR -> IO Result) -> Ptr PhysicalDevice_T -> DisplayKHR -> Ptr Word32 -> Ptr DisplayModePropertiesKHR -> IO Result

-- No documentation found for TopLevel "vkGetDisplayModePropertiesKHR"
getDisplayModePropertiesKHR :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "vkGetDisplayModePropertiesKHR" "physicalDevice"
                               PhysicalDevice
                            -> -- No documentation found for Nested "vkGetDisplayModePropertiesKHR" "display"
                               DisplayKHR
                            -> io (Result, ("properties" ::: Vector DisplayModePropertiesKHR))
getDisplayModePropertiesKHR physicalDevice display = liftIO . evalContT $ do
  let vkGetDisplayModePropertiesKHRPtr = pVkGetDisplayModePropertiesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetDisplayModePropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDisplayModePropertiesKHR is null" Nothing Nothing
  let vkGetDisplayModePropertiesKHR' = mkVkGetDisplayModePropertiesKHR vkGetDisplayModePropertiesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetDisplayModePropertiesKHR' physicalDevice' (display) (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @DisplayModePropertiesKHR ((fromIntegral (pPropertyCount)) * 24)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 24) :: Ptr DisplayModePropertiesKHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkGetDisplayModePropertiesKHR' physicalDevice' (display) (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @DisplayModePropertiesKHR (((pPProperties) `advancePtrBytes` (24 * (i)) :: Ptr DisplayModePropertiesKHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDisplayModeKHR
  :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> Ptr DisplayModeCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr DisplayModeKHR -> IO Result) -> Ptr PhysicalDevice_T -> DisplayKHR -> Ptr DisplayModeCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr DisplayModeKHR -> IO Result

-- No documentation found for TopLevel "vkCreateDisplayModeKHR"
createDisplayModeKHR :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkCreateDisplayModeKHR" "physicalDevice"
                        PhysicalDevice
                     -> -- No documentation found for Nested "vkCreateDisplayModeKHR" "display"
                        DisplayKHR
                     -> -- No documentation found for Nested "vkCreateDisplayModeKHR" "pCreateInfo"
                        DisplayModeCreateInfoKHR
                     -> -- No documentation found for Nested "vkCreateDisplayModeKHR" "pAllocator"
                        ("allocator" ::: Maybe AllocationCallbacks)
                     -> io (DisplayModeKHR)
createDisplayModeKHR physicalDevice display createInfo allocator = liftIO . evalContT $ do
  let vkCreateDisplayModeKHRPtr = pVkCreateDisplayModeKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkCreateDisplayModeKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDisplayModeKHR is null" Nothing Nothing
  let vkCreateDisplayModeKHR' = mkVkCreateDisplayModeKHR vkCreateDisplayModeKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPMode <- ContT $ bracket (callocBytes @DisplayModeKHR 8) free
  r <- lift $ vkCreateDisplayModeKHR' (physicalDeviceHandle (physicalDevice)) (display) pCreateInfo pAllocator (pPMode)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMode <- lift $ peek @DisplayModeKHR pPMode
  pure $ (pMode)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneCapabilitiesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> DisplayModeKHR -> Word32 -> Ptr DisplayPlaneCapabilitiesKHR -> IO Result) -> Ptr PhysicalDevice_T -> DisplayModeKHR -> Word32 -> Ptr DisplayPlaneCapabilitiesKHR -> IO Result

-- No documentation found for TopLevel "vkGetDisplayPlaneCapabilitiesKHR"
getDisplayPlaneCapabilitiesKHR :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "vkGetDisplayPlaneCapabilitiesKHR" "physicalDevice"
                                  PhysicalDevice
                               -> -- No documentation found for Nested "vkGetDisplayPlaneCapabilitiesKHR" "mode"
                                  DisplayModeKHR
                               -> -- No documentation found for Nested "vkGetDisplayPlaneCapabilitiesKHR" "planeIndex"
                                  ("planeIndex" ::: Word32)
                               -> io (DisplayPlaneCapabilitiesKHR)
getDisplayPlaneCapabilitiesKHR physicalDevice mode planeIndex = liftIO . evalContT $ do
  let vkGetDisplayPlaneCapabilitiesKHRPtr = pVkGetDisplayPlaneCapabilitiesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetDisplayPlaneCapabilitiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDisplayPlaneCapabilitiesKHR is null" Nothing Nothing
  let vkGetDisplayPlaneCapabilitiesKHR' = mkVkGetDisplayPlaneCapabilitiesKHR vkGetDisplayPlaneCapabilitiesKHRPtr
  pPCapabilities <- ContT (withZeroCStruct @DisplayPlaneCapabilitiesKHR)
  r <- lift $ vkGetDisplayPlaneCapabilitiesKHR' (physicalDeviceHandle (physicalDevice)) (mode) (planeIndex) (pPCapabilities)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCapabilities <- lift $ peekCStruct @DisplayPlaneCapabilitiesKHR pPCapabilities
  pure $ (pCapabilities)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDisplayPlaneSurfaceKHR
  :: FunPtr (Ptr Instance_T -> Ptr DisplaySurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result) -> Ptr Instance_T -> Ptr DisplaySurfaceCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO Result

-- No documentation found for TopLevel "vkCreateDisplayPlaneSurfaceKHR"
createDisplayPlaneSurfaceKHR :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkCreateDisplayPlaneSurfaceKHR" "instance"
                                Instance
                             -> -- No documentation found for Nested "vkCreateDisplayPlaneSurfaceKHR" "pCreateInfo"
                                DisplaySurfaceCreateInfoKHR
                             -> -- No documentation found for Nested "vkCreateDisplayPlaneSurfaceKHR" "pAllocator"
                                ("allocator" ::: Maybe AllocationCallbacks)
                             -> io (SurfaceKHR)
createDisplayPlaneSurfaceKHR instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateDisplayPlaneSurfaceKHRPtr = pVkCreateDisplayPlaneSurfaceKHR (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateDisplayPlaneSurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDisplayPlaneSurfaceKHR is null" Nothing Nothing
  let vkCreateDisplayPlaneSurfaceKHR' = mkVkCreateDisplayPlaneSurfaceKHR vkCreateDisplayPlaneSurfaceKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSurface <- ContT $ bracket (callocBytes @SurfaceKHR 8) free
  r <- lift $ vkCreateDisplayPlaneSurfaceKHR' (instanceHandle (instance')) pCreateInfo pAllocator (pPSurface)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurface <- lift $ peek @SurfaceKHR pPSurface
  pure $ (pSurface)



-- No documentation found for TopLevel "VkDisplayPropertiesKHR"
data DisplayPropertiesKHR = DisplayPropertiesKHR
  { -- No documentation found for Nested "VkDisplayPropertiesKHR" "display"
    display :: DisplayKHR
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "displayName"
    displayName :: ByteString
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "physicalDimensions"
    physicalDimensions :: Extent2D
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "physicalResolution"
    physicalResolution :: Extent2D
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "supportedTransforms"
    supportedTransforms :: SurfaceTransformFlagsKHR
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "planeReorderPossible"
    planeReorderPossible :: Bool
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "persistentContent"
    persistentContent :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPropertiesKHR)
#endif
deriving instance Show DisplayPropertiesKHR

instance ToCStruct DisplayPropertiesKHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPropertiesKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr DisplayKHR)) (display)
    displayName'' <- ContT $ useAsCString (displayName)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr CChar))) displayName''
    lift $ poke ((p `plusPtr` 16 :: Ptr Extent2D)) (physicalDimensions)
    lift $ poke ((p `plusPtr` 24 :: Ptr Extent2D)) (physicalResolution)
    lift $ poke ((p `plusPtr` 32 :: Ptr SurfaceTransformFlagsKHR)) (supportedTransforms)
    lift $ poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (planeReorderPossible))
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (persistentContent))
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr DisplayKHR)) (zero)
    displayName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr CChar))) displayName''
    lift $ poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance FromCStruct DisplayPropertiesKHR where
  peekCStruct p = do
    display <- peek @DisplayKHR ((p `plusPtr` 0 :: Ptr DisplayKHR))
    displayName <- packCString =<< peek ((p `plusPtr` 8 :: Ptr (Ptr CChar)))
    physicalDimensions <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    physicalResolution <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    supportedTransforms <- peek @SurfaceTransformFlagsKHR ((p `plusPtr` 32 :: Ptr SurfaceTransformFlagsKHR))
    planeReorderPossible <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    persistentContent <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    pure $ DisplayPropertiesKHR
             display displayName physicalDimensions physicalResolution supportedTransforms (bool32ToBool planeReorderPossible) (bool32ToBool persistentContent)

instance Zero DisplayPropertiesKHR where
  zero = DisplayPropertiesKHR
           zero
           mempty
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkDisplayPlanePropertiesKHR"
data DisplayPlanePropertiesKHR = DisplayPlanePropertiesKHR
  { -- No documentation found for Nested "VkDisplayPlanePropertiesKHR" "currentDisplay"
    currentDisplay :: DisplayKHR
  , -- No documentation found for Nested "VkDisplayPlanePropertiesKHR" "currentStackIndex"
    currentStackIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPlanePropertiesKHR)
#endif
deriving instance Show DisplayPlanePropertiesKHR

instance ToCStruct DisplayPlanePropertiesKHR where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPlanePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DisplayKHR)) (currentDisplay)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (currentStackIndex)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DisplayKHR)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct DisplayPlanePropertiesKHR where
  peekCStruct p = do
    currentDisplay <- peek @DisplayKHR ((p `plusPtr` 0 :: Ptr DisplayKHR))
    currentStackIndex <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ DisplayPlanePropertiesKHR
             currentDisplay currentStackIndex


instance Storable DisplayPlanePropertiesKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayPlanePropertiesKHR where
  zero = DisplayPlanePropertiesKHR
           zero
           zero



-- No documentation found for TopLevel "VkDisplayModeParametersKHR"
data DisplayModeParametersKHR = DisplayModeParametersKHR
  { -- No documentation found for Nested "VkDisplayModeParametersKHR" "visibleRegion"
    visibleRegion :: Extent2D
  , -- No documentation found for Nested "VkDisplayModeParametersKHR" "refreshRate"
    refreshRate :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayModeParametersKHR)
#endif
deriving instance Show DisplayModeParametersKHR

instance ToCStruct DisplayModeParametersKHR where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayModeParametersKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Extent2D)) (visibleRegion)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (refreshRate)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct DisplayModeParametersKHR where
  peekCStruct p = do
    visibleRegion <- peekCStruct @Extent2D ((p `plusPtr` 0 :: Ptr Extent2D))
    refreshRate <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ DisplayModeParametersKHR
             visibleRegion refreshRate


instance Storable DisplayModeParametersKHR where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayModeParametersKHR where
  zero = DisplayModeParametersKHR
           zero
           zero



-- No documentation found for TopLevel "VkDisplayModePropertiesKHR"
data DisplayModePropertiesKHR = DisplayModePropertiesKHR
  { -- No documentation found for Nested "VkDisplayModePropertiesKHR" "displayMode"
    displayMode :: DisplayModeKHR
  , -- No documentation found for Nested "VkDisplayModePropertiesKHR" "parameters"
    parameters :: DisplayModeParametersKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayModePropertiesKHR)
#endif
deriving instance Show DisplayModePropertiesKHR

instance ToCStruct DisplayModePropertiesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayModePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DisplayModeKHR)) (displayMode)
    poke ((p `plusPtr` 8 :: Ptr DisplayModeParametersKHR)) (parameters)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DisplayModeKHR)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DisplayModeParametersKHR)) (zero)
    f

instance FromCStruct DisplayModePropertiesKHR where
  peekCStruct p = do
    displayMode <- peek @DisplayModeKHR ((p `plusPtr` 0 :: Ptr DisplayModeKHR))
    parameters <- peekCStruct @DisplayModeParametersKHR ((p `plusPtr` 8 :: Ptr DisplayModeParametersKHR))
    pure $ DisplayModePropertiesKHR
             displayMode parameters


instance Storable DisplayModePropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayModePropertiesKHR where
  zero = DisplayModePropertiesKHR
           zero
           zero



-- No documentation found for TopLevel "VkDisplayModeCreateInfoKHR"
data DisplayModeCreateInfoKHR = DisplayModeCreateInfoKHR
  { -- No documentation found for Nested "VkDisplayModeCreateInfoKHR" "flags"
    flags :: DisplayModeCreateFlagsKHR
  , -- No documentation found for Nested "VkDisplayModeCreateInfoKHR" "parameters"
    parameters :: DisplayModeParametersKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayModeCreateInfoKHR)
#endif
deriving instance Show DisplayModeCreateInfoKHR

instance ToCStruct DisplayModeCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayModeCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayModeCreateFlagsKHR)) (flags)
    poke ((p `plusPtr` 20 :: Ptr DisplayModeParametersKHR)) (parameters)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr DisplayModeParametersKHR)) (zero)
    f

instance FromCStruct DisplayModeCreateInfoKHR where
  peekCStruct p = do
    flags <- peek @DisplayModeCreateFlagsKHR ((p `plusPtr` 16 :: Ptr DisplayModeCreateFlagsKHR))
    parameters <- peekCStruct @DisplayModeParametersKHR ((p `plusPtr` 20 :: Ptr DisplayModeParametersKHR))
    pure $ DisplayModeCreateInfoKHR
             flags parameters


instance Storable DisplayModeCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayModeCreateInfoKHR where
  zero = DisplayModeCreateInfoKHR
           zero
           zero



-- No documentation found for TopLevel "VkDisplayPlaneCapabilitiesKHR"
data DisplayPlaneCapabilitiesKHR = DisplayPlaneCapabilitiesKHR
  { -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "supportedAlpha"
    supportedAlpha :: DisplayPlaneAlphaFlagsKHR
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "minSrcPosition"
    minSrcPosition :: Offset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "maxSrcPosition"
    maxSrcPosition :: Offset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "minSrcExtent"
    minSrcExtent :: Extent2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "maxSrcExtent"
    maxSrcExtent :: Extent2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "minDstPosition"
    minDstPosition :: Offset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "maxDstPosition"
    maxDstPosition :: Offset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "minDstExtent"
    minDstExtent :: Extent2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "maxDstExtent"
    maxDstExtent :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPlaneCapabilitiesKHR)
#endif
deriving instance Show DisplayPlaneCapabilitiesKHR

instance ToCStruct DisplayPlaneCapabilitiesKHR where
  withCStruct x f = allocaBytesAligned 68 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPlaneCapabilitiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DisplayPlaneAlphaFlagsKHR)) (supportedAlpha)
    poke ((p `plusPtr` 4 :: Ptr Offset2D)) (minSrcPosition)
    poke ((p `plusPtr` 12 :: Ptr Offset2D)) (maxSrcPosition)
    poke ((p `plusPtr` 20 :: Ptr Extent2D)) (minSrcExtent)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (maxSrcExtent)
    poke ((p `plusPtr` 36 :: Ptr Offset2D)) (minDstPosition)
    poke ((p `plusPtr` 44 :: Ptr Offset2D)) (maxDstPosition)
    poke ((p `plusPtr` 52 :: Ptr Extent2D)) (minDstExtent)
    poke ((p `plusPtr` 60 :: Ptr Extent2D)) (maxDstExtent)
    f
  cStructSize = 68
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 4 :: Ptr Offset2D)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Offset2D)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Offset2D)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Offset2D)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct DisplayPlaneCapabilitiesKHR where
  peekCStruct p = do
    supportedAlpha <- peek @DisplayPlaneAlphaFlagsKHR ((p `plusPtr` 0 :: Ptr DisplayPlaneAlphaFlagsKHR))
    minSrcPosition <- peekCStruct @Offset2D ((p `plusPtr` 4 :: Ptr Offset2D))
    maxSrcPosition <- peekCStruct @Offset2D ((p `plusPtr` 12 :: Ptr Offset2D))
    minSrcExtent <- peekCStruct @Extent2D ((p `plusPtr` 20 :: Ptr Extent2D))
    maxSrcExtent <- peekCStruct @Extent2D ((p `plusPtr` 28 :: Ptr Extent2D))
    minDstPosition <- peekCStruct @Offset2D ((p `plusPtr` 36 :: Ptr Offset2D))
    maxDstPosition <- peekCStruct @Offset2D ((p `plusPtr` 44 :: Ptr Offset2D))
    minDstExtent <- peekCStruct @Extent2D ((p `plusPtr` 52 :: Ptr Extent2D))
    maxDstExtent <- peekCStruct @Extent2D ((p `plusPtr` 60 :: Ptr Extent2D))
    pure $ DisplayPlaneCapabilitiesKHR
             supportedAlpha minSrcPosition maxSrcPosition minSrcExtent maxSrcExtent minDstPosition maxDstPosition minDstExtent maxDstExtent


instance Storable DisplayPlaneCapabilitiesKHR where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayPlaneCapabilitiesKHR where
  zero = DisplayPlaneCapabilitiesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkDisplaySurfaceCreateInfoKHR"
data DisplaySurfaceCreateInfoKHR = DisplaySurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "flags"
    flags :: DisplaySurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "displayMode"
    displayMode :: DisplayModeKHR
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "planeIndex"
    planeIndex :: Word32
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "planeStackIndex"
    planeStackIndex :: Word32
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "transform"
    transform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "globalAlpha"
    globalAlpha :: Float
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "alphaMode"
    alphaMode :: DisplayPlaneAlphaFlagBitsKHR
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "imageExtent"
    imageExtent :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplaySurfaceCreateInfoKHR)
#endif
deriving instance Show DisplaySurfaceCreateInfoKHR

instance ToCStruct DisplaySurfaceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplaySurfaceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplaySurfaceCreateFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr DisplayModeKHR)) (displayMode)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (planeIndex)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (planeStackIndex)
    poke ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR)) (transform)
    poke ((p `plusPtr` 44 :: Ptr CFloat)) (CFloat (globalAlpha))
    poke ((p `plusPtr` 48 :: Ptr DisplayPlaneAlphaFlagBitsKHR)) (alphaMode)
    poke ((p `plusPtr` 52 :: Ptr Extent2D)) (imageExtent)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr DisplayModeKHR)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    poke ((p `plusPtr` 44 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 48 :: Ptr DisplayPlaneAlphaFlagBitsKHR)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct DisplaySurfaceCreateInfoKHR where
  peekCStruct p = do
    flags <- peek @DisplaySurfaceCreateFlagsKHR ((p `plusPtr` 16 :: Ptr DisplaySurfaceCreateFlagsKHR))
    displayMode <- peek @DisplayModeKHR ((p `plusPtr` 24 :: Ptr DisplayModeKHR))
    planeIndex <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    planeStackIndex <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    transform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR))
    globalAlpha <- peek @CFloat ((p `plusPtr` 44 :: Ptr CFloat))
    alphaMode <- peek @DisplayPlaneAlphaFlagBitsKHR ((p `plusPtr` 48 :: Ptr DisplayPlaneAlphaFlagBitsKHR))
    imageExtent <- peekCStruct @Extent2D ((p `plusPtr` 52 :: Ptr Extent2D))
    pure $ DisplaySurfaceCreateInfoKHR
             flags displayMode planeIndex planeStackIndex transform ((\(CFloat a) -> a) globalAlpha) alphaMode imageExtent


instance Storable DisplaySurfaceCreateInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplaySurfaceCreateInfoKHR where
  zero = DisplaySurfaceCreateInfoKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- No documentation found for TopLevel "VkDisplayModeCreateFlagsKHR"
newtype DisplayModeCreateFlagsKHR = DisplayModeCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDisplayModeCreateFlagsKHR :: String
conNameDisplayModeCreateFlagsKHR = "DisplayModeCreateFlagsKHR"

enumPrefixDisplayModeCreateFlagsKHR :: String
enumPrefixDisplayModeCreateFlagsKHR = ""

showTableDisplayModeCreateFlagsKHR :: [(DisplayModeCreateFlagsKHR, String)]
showTableDisplayModeCreateFlagsKHR = []


instance Show DisplayModeCreateFlagsKHR where
showsPrec = enumShowsPrec enumPrefixDisplayModeCreateFlagsKHR
                          showTableDisplayModeCreateFlagsKHR
                          conNameDisplayModeCreateFlagsKHR
                          (\(DisplayModeCreateFlagsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DisplayModeCreateFlagsKHR where
  readPrec = enumReadPrec enumPrefixDisplayModeCreateFlagsKHR
                          showTableDisplayModeCreateFlagsKHR
                          conNameDisplayModeCreateFlagsKHR
                          DisplayModeCreateFlagsKHR


-- No documentation found for TopLevel "VkDisplaySurfaceCreateFlagsKHR"
newtype DisplaySurfaceCreateFlagsKHR = DisplaySurfaceCreateFlagsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDisplaySurfaceCreateFlagsKHR :: String
conNameDisplaySurfaceCreateFlagsKHR = "DisplaySurfaceCreateFlagsKHR"

enumPrefixDisplaySurfaceCreateFlagsKHR :: String
enumPrefixDisplaySurfaceCreateFlagsKHR = ""

showTableDisplaySurfaceCreateFlagsKHR :: [(DisplaySurfaceCreateFlagsKHR, String)]
showTableDisplaySurfaceCreateFlagsKHR = []


instance Show DisplaySurfaceCreateFlagsKHR where
showsPrec = enumShowsPrec enumPrefixDisplaySurfaceCreateFlagsKHR
                          showTableDisplaySurfaceCreateFlagsKHR
                          conNameDisplaySurfaceCreateFlagsKHR
                          (\(DisplaySurfaceCreateFlagsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DisplaySurfaceCreateFlagsKHR where
  readPrec = enumReadPrec enumPrefixDisplaySurfaceCreateFlagsKHR
                          showTableDisplaySurfaceCreateFlagsKHR
                          conNameDisplaySurfaceCreateFlagsKHR
                          DisplaySurfaceCreateFlagsKHR


type DisplayPlaneAlphaFlagsKHR = DisplayPlaneAlphaFlagBitsKHR

-- No documentation found for TopLevel "VkDisplayPlaneAlphaFlagBitsKHR"
newtype DisplayPlaneAlphaFlagBitsKHR = DisplayPlaneAlphaFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDisplayPlaneAlphaFlagBitsKHR" "VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR"
pattern DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR                  = DisplayPlaneAlphaFlagBitsKHR 0x00000001
-- No documentation found for Nested "VkDisplayPlaneAlphaFlagBitsKHR" "VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR"
pattern DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR                  = DisplayPlaneAlphaFlagBitsKHR 0x00000002
-- No documentation found for Nested "VkDisplayPlaneAlphaFlagBitsKHR" "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR"
pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR               = DisplayPlaneAlphaFlagBitsKHR 0x00000004
-- No documentation found for Nested "VkDisplayPlaneAlphaFlagBitsKHR" "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR"
pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = DisplayPlaneAlphaFlagBitsKHR 0x00000008

conNameDisplayPlaneAlphaFlagBitsKHR :: String
conNameDisplayPlaneAlphaFlagBitsKHR = "DisplayPlaneAlphaFlagBitsKHR"

enumPrefixDisplayPlaneAlphaFlagBitsKHR :: String
enumPrefixDisplayPlaneAlphaFlagBitsKHR = "DISPLAY_PLANE_ALPHA_"

showTableDisplayPlaneAlphaFlagBitsKHR :: [(DisplayPlaneAlphaFlagBitsKHR, String)]
showTableDisplayPlaneAlphaFlagBitsKHR =
  [ (DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR                 , "OPAQUE_BIT_KHR")
  , (DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR                 , "GLOBAL_BIT_KHR")
  , (DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR              , "PER_PIXEL_BIT_KHR")
  , (DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR, "PER_PIXEL_PREMULTIPLIED_BIT_KHR")
  ]


instance Show DisplayPlaneAlphaFlagBitsKHR where
showsPrec = enumShowsPrec enumPrefixDisplayPlaneAlphaFlagBitsKHR
                          showTableDisplayPlaneAlphaFlagBitsKHR
                          conNameDisplayPlaneAlphaFlagBitsKHR
                          (\(DisplayPlaneAlphaFlagBitsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DisplayPlaneAlphaFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixDisplayPlaneAlphaFlagBitsKHR
                          showTableDisplayPlaneAlphaFlagBitsKHR
                          conNameDisplayPlaneAlphaFlagBitsKHR
                          DisplayPlaneAlphaFlagBitsKHR


type KHR_DISPLAY_SPEC_VERSION = 23

-- No documentation found for TopLevel "VK_KHR_DISPLAY_SPEC_VERSION"
pattern KHR_DISPLAY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DISPLAY_SPEC_VERSION = 23


type KHR_DISPLAY_EXTENSION_NAME = "VK_KHR_display"

-- No documentation found for TopLevel "VK_KHR_DISPLAY_EXTENSION_NAME"
pattern KHR_DISPLAY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DISPLAY_EXTENSION_NAME = "VK_KHR_display"

