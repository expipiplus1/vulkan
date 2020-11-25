{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_get_display_properties2"
module Vulkan.Extensions.VK_KHR_get_display_properties2  ( getPhysicalDeviceDisplayProperties2KHR
                                                         , getPhysicalDeviceDisplayPlaneProperties2KHR
                                                         , getDisplayModeProperties2KHR
                                                         , getDisplayPlaneCapabilities2KHR
                                                         , DisplayProperties2KHR(..)
                                                         , DisplayPlaneProperties2KHR(..)
                                                         , DisplayModeProperties2KHR(..)
                                                         , DisplayPlaneInfo2KHR(..)
                                                         , DisplayPlaneCapabilities2KHR(..)
                                                         , KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
                                                         , pattern KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
                                                         , KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
                                                         , pattern KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
                                                         , DisplayKHR(..)
                                                         , DisplayModeKHR(..)
                                                         , DisplayPropertiesKHR(..)
                                                         , DisplayPlanePropertiesKHR(..)
                                                         , DisplayModeParametersKHR(..)
                                                         , DisplayModePropertiesKHR(..)
                                                         , DisplayPlaneCapabilitiesKHR(..)
                                                         , DisplayPlaneAlphaFlagBitsKHR(..)
                                                         , DisplayPlaneAlphaFlagsKHR
                                                         , SurfaceTransformFlagBitsKHR(..)
                                                         , SurfaceTransformFlagsKHR
                                                         ) where

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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.Handles (DisplayKHR)
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Extensions.Handles (DisplayModeKHR)
import Vulkan.Extensions.VK_KHR_display (DisplayModePropertiesKHR)
import Vulkan.Extensions.VK_KHR_display (DisplayPlaneCapabilitiesKHR)
import Vulkan.Extensions.VK_KHR_display (DisplayPlanePropertiesKHR)
import Vulkan.Extensions.VK_KHR_display (DisplayPropertiesKHR)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetDisplayModeProperties2KHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetDisplayPlaneCapabilities2KHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceDisplayPlaneProperties2KHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceDisplayProperties2KHR))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Extensions.Handles (DisplayModeKHR(..))
import Vulkan.Extensions.VK_KHR_display (DisplayModeParametersKHR(..))
import Vulkan.Extensions.VK_KHR_display (DisplayModePropertiesKHR(..))
import Vulkan.Extensions.VK_KHR_display (DisplayPlaneAlphaFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_display (DisplayPlaneAlphaFlagsKHR)
import Vulkan.Extensions.VK_KHR_display (DisplayPlaneCapabilitiesKHR(..))
import Vulkan.Extensions.VK_KHR_display (DisplayPlanePropertiesKHR(..))
import Vulkan.Extensions.VK_KHR_display (DisplayPropertiesKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayProperties2KHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayProperties2KHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayProperties2KHR -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayProperties2KHR"
getPhysicalDeviceDisplayProperties2KHR :: forall io
                                        . (MonadIO io)
                                       => -- No documentation found for Nested "vkGetPhysicalDeviceDisplayProperties2KHR" "physicalDevice"
                                          PhysicalDevice
                                       -> io (Result, ("properties" ::: Vector DisplayProperties2KHR))
getPhysicalDeviceDisplayProperties2KHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceDisplayProperties2KHRPtr = pVkGetPhysicalDeviceDisplayProperties2KHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceDisplayProperties2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceDisplayProperties2KHR is null" Nothing Nothing
  let vkGetPhysicalDeviceDisplayProperties2KHR' = mkVkGetPhysicalDeviceDisplayProperties2KHR vkGetPhysicalDeviceDisplayProperties2KHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceDisplayProperties2KHR' physicalDevice' (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @DisplayProperties2KHR ((fromIntegral (pPropertyCount)) * 64)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 64) :: Ptr DisplayProperties2KHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkGetPhysicalDeviceDisplayProperties2KHR' physicalDevice' (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @DisplayProperties2KHR (((pPProperties) `advancePtrBytes` (64 * (i)) :: Ptr DisplayProperties2KHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPlaneProperties2KHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayPlaneProperties2KHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayPlaneProperties2KHR -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayPlaneProperties2KHR"
getPhysicalDeviceDisplayPlaneProperties2KHR :: forall io
                                             . (MonadIO io)
                                            => -- No documentation found for Nested "vkGetPhysicalDeviceDisplayPlaneProperties2KHR" "physicalDevice"
                                               PhysicalDevice
                                            -> io (Result, ("properties" ::: Vector DisplayPlaneProperties2KHR))
getPhysicalDeviceDisplayPlaneProperties2KHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceDisplayPlaneProperties2KHRPtr = pVkGetPhysicalDeviceDisplayPlaneProperties2KHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceDisplayPlaneProperties2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceDisplayPlaneProperties2KHR is null" Nothing Nothing
  let vkGetPhysicalDeviceDisplayPlaneProperties2KHR' = mkVkGetPhysicalDeviceDisplayPlaneProperties2KHR vkGetPhysicalDeviceDisplayPlaneProperties2KHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceDisplayPlaneProperties2KHR' physicalDevice' (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @DisplayPlaneProperties2KHR ((fromIntegral (pPropertyCount)) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 32) :: Ptr DisplayPlaneProperties2KHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkGetPhysicalDeviceDisplayPlaneProperties2KHR' physicalDevice' (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @DisplayPlaneProperties2KHR (((pPProperties) `advancePtrBytes` (32 * (i)) :: Ptr DisplayPlaneProperties2KHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayModeProperties2KHR
  :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> Ptr Word32 -> Ptr DisplayModeProperties2KHR -> IO Result) -> Ptr PhysicalDevice_T -> DisplayKHR -> Ptr Word32 -> Ptr DisplayModeProperties2KHR -> IO Result

-- No documentation found for TopLevel "vkGetDisplayModeProperties2KHR"
getDisplayModeProperties2KHR :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkGetDisplayModeProperties2KHR" "physicalDevice"
                                PhysicalDevice
                             -> -- No documentation found for Nested "vkGetDisplayModeProperties2KHR" "display"
                                DisplayKHR
                             -> io (Result, ("properties" ::: Vector DisplayModeProperties2KHR))
getDisplayModeProperties2KHR physicalDevice display = liftIO . evalContT $ do
  let vkGetDisplayModeProperties2KHRPtr = pVkGetDisplayModeProperties2KHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetDisplayModeProperties2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDisplayModeProperties2KHR is null" Nothing Nothing
  let vkGetDisplayModeProperties2KHR' = mkVkGetDisplayModeProperties2KHR vkGetDisplayModeProperties2KHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetDisplayModeProperties2KHR' physicalDevice' (display) (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @DisplayModeProperties2KHR ((fromIntegral (pPropertyCount)) * 40)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 40) :: Ptr DisplayModeProperties2KHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkGetDisplayModeProperties2KHR' physicalDevice' (display) (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @DisplayModeProperties2KHR (((pPProperties) `advancePtrBytes` (40 * (i)) :: Ptr DisplayModeProperties2KHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneCapabilities2KHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr DisplayPlaneInfo2KHR -> Ptr DisplayPlaneCapabilities2KHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr DisplayPlaneInfo2KHR -> Ptr DisplayPlaneCapabilities2KHR -> IO Result

-- No documentation found for TopLevel "vkGetDisplayPlaneCapabilities2KHR"
getDisplayPlaneCapabilities2KHR :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkGetDisplayPlaneCapabilities2KHR" "physicalDevice"
                                   PhysicalDevice
                                -> -- No documentation found for Nested "vkGetDisplayPlaneCapabilities2KHR" "pDisplayPlaneInfo"
                                   DisplayPlaneInfo2KHR
                                -> io (DisplayPlaneCapabilities2KHR)
getDisplayPlaneCapabilities2KHR physicalDevice displayPlaneInfo = liftIO . evalContT $ do
  let vkGetDisplayPlaneCapabilities2KHRPtr = pVkGetDisplayPlaneCapabilities2KHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetDisplayPlaneCapabilities2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDisplayPlaneCapabilities2KHR is null" Nothing Nothing
  let vkGetDisplayPlaneCapabilities2KHR' = mkVkGetDisplayPlaneCapabilities2KHR vkGetDisplayPlaneCapabilities2KHRPtr
  pDisplayPlaneInfo <- ContT $ withCStruct (displayPlaneInfo)
  pPCapabilities <- ContT (withZeroCStruct @DisplayPlaneCapabilities2KHR)
  r <- lift $ vkGetDisplayPlaneCapabilities2KHR' (physicalDeviceHandle (physicalDevice)) pDisplayPlaneInfo (pPCapabilities)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCapabilities <- lift $ peekCStruct @DisplayPlaneCapabilities2KHR pPCapabilities
  pure $ (pCapabilities)



-- No documentation found for TopLevel "VkDisplayProperties2KHR"
data DisplayProperties2KHR = DisplayProperties2KHR
  { -- No documentation found for Nested "VkDisplayProperties2KHR" "displayProperties"
    displayProperties :: DisplayPropertiesKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayProperties2KHR)
#endif
deriving instance Show DisplayProperties2KHR

instance ToCStruct DisplayProperties2KHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayProperties2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DisplayPropertiesKHR)) (displayProperties) . ($ ())
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DisplayPropertiesKHR)) (zero) . ($ ())
    lift $ f

instance FromCStruct DisplayProperties2KHR where
  peekCStruct p = do
    displayProperties <- peekCStruct @DisplayPropertiesKHR ((p `plusPtr` 16 :: Ptr DisplayPropertiesKHR))
    pure $ DisplayProperties2KHR
             displayProperties

instance Zero DisplayProperties2KHR where
  zero = DisplayProperties2KHR
           zero



-- No documentation found for TopLevel "VkDisplayPlaneProperties2KHR"
data DisplayPlaneProperties2KHR = DisplayPlaneProperties2KHR
  { -- No documentation found for Nested "VkDisplayPlaneProperties2KHR" "displayPlaneProperties"
    displayPlaneProperties :: DisplayPlanePropertiesKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPlaneProperties2KHR)
#endif
deriving instance Show DisplayPlaneProperties2KHR

instance ToCStruct DisplayPlaneProperties2KHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPlaneProperties2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayPlanePropertiesKHR)) (displayPlaneProperties)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayPlanePropertiesKHR)) (zero)
    f

instance FromCStruct DisplayPlaneProperties2KHR where
  peekCStruct p = do
    displayPlaneProperties <- peekCStruct @DisplayPlanePropertiesKHR ((p `plusPtr` 16 :: Ptr DisplayPlanePropertiesKHR))
    pure $ DisplayPlaneProperties2KHR
             displayPlaneProperties


instance Storable DisplayPlaneProperties2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayPlaneProperties2KHR where
  zero = DisplayPlaneProperties2KHR
           zero



-- No documentation found for TopLevel "VkDisplayModeProperties2KHR"
data DisplayModeProperties2KHR = DisplayModeProperties2KHR
  { -- No documentation found for Nested "VkDisplayModeProperties2KHR" "displayModeProperties"
    displayModeProperties :: DisplayModePropertiesKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayModeProperties2KHR)
#endif
deriving instance Show DisplayModeProperties2KHR

instance ToCStruct DisplayModeProperties2KHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayModeProperties2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayModePropertiesKHR)) (displayModeProperties)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayModePropertiesKHR)) (zero)
    f

instance FromCStruct DisplayModeProperties2KHR where
  peekCStruct p = do
    displayModeProperties <- peekCStruct @DisplayModePropertiesKHR ((p `plusPtr` 16 :: Ptr DisplayModePropertiesKHR))
    pure $ DisplayModeProperties2KHR
             displayModeProperties


instance Storable DisplayModeProperties2KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayModeProperties2KHR where
  zero = DisplayModeProperties2KHR
           zero



-- No documentation found for TopLevel "VkDisplayPlaneInfo2KHR"
data DisplayPlaneInfo2KHR = DisplayPlaneInfo2KHR
  { -- No documentation found for Nested "VkDisplayPlaneInfo2KHR" "mode"
    mode :: DisplayModeKHR
  , -- No documentation found for Nested "VkDisplayPlaneInfo2KHR" "planeIndex"
    planeIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPlaneInfo2KHR)
#endif
deriving instance Show DisplayPlaneInfo2KHR

instance ToCStruct DisplayPlaneInfo2KHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPlaneInfo2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayModeKHR)) (mode)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (planeIndex)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayModeKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct DisplayPlaneInfo2KHR where
  peekCStruct p = do
    mode <- peek @DisplayModeKHR ((p `plusPtr` 16 :: Ptr DisplayModeKHR))
    planeIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ DisplayPlaneInfo2KHR
             mode planeIndex


instance Storable DisplayPlaneInfo2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayPlaneInfo2KHR where
  zero = DisplayPlaneInfo2KHR
           zero
           zero



-- No documentation found for TopLevel "VkDisplayPlaneCapabilities2KHR"
data DisplayPlaneCapabilities2KHR = DisplayPlaneCapabilities2KHR
  { -- No documentation found for Nested "VkDisplayPlaneCapabilities2KHR" "capabilities"
    capabilities :: DisplayPlaneCapabilitiesKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPlaneCapabilities2KHR)
#endif
deriving instance Show DisplayPlaneCapabilities2KHR

instance ToCStruct DisplayPlaneCapabilities2KHR where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPlaneCapabilities2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayPlaneCapabilitiesKHR)) (capabilities)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayPlaneCapabilitiesKHR)) (zero)
    f

instance FromCStruct DisplayPlaneCapabilities2KHR where
  peekCStruct p = do
    capabilities <- peekCStruct @DisplayPlaneCapabilitiesKHR ((p `plusPtr` 16 :: Ptr DisplayPlaneCapabilitiesKHR))
    pure $ DisplayPlaneCapabilities2KHR
             capabilities


instance Storable DisplayPlaneCapabilities2KHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayPlaneCapabilities2KHR where
  zero = DisplayPlaneCapabilities2KHR
           zero


type KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION"
pattern KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION = 1


type KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_display_properties2"

-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME"
pattern KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_display_properties2"

