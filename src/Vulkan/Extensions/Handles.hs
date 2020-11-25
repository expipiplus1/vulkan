{-# language CPP #-}
-- No documentation found for Chapter "Handles"
module Vulkan.Extensions.Handles  ( IndirectCommandsLayoutNV(..)
                                  , ValidationCacheEXT(..)
                                  , AccelerationStructureKHR(..)
                                  , AccelerationStructureNV(..)
                                  , PerformanceConfigurationINTEL(..)
                                  , DeferredOperationKHR(..)
                                  , PrivateDataSlotEXT(..)
                                  , DisplayKHR(..)
                                  , DisplayModeKHR(..)
                                  , SurfaceKHR(..)
                                  , SwapchainKHR(..)
                                  , DebugReportCallbackEXT(..)
                                  , DebugUtilsMessengerEXT(..)
                                  , Instance(..)
                                  , PhysicalDevice(..)
                                  , Device(..)
                                  , Queue(..)
                                  , CommandBuffer(..)
                                  , DeviceMemory(..)
                                  , CommandPool(..)
                                  , Buffer(..)
                                  , BufferView(..)
                                  , Image(..)
                                  , ImageView(..)
                                  , ShaderModule(..)
                                  , Pipeline(..)
                                  , PipelineLayout(..)
                                  , Sampler(..)
                                  , DescriptorSet(..)
                                  , DescriptorSetLayout(..)
                                  , Fence(..)
                                  , Semaphore(..)
                                  , QueryPool(..)
                                  , Framebuffer(..)
                                  , RenderPass(..)
                                  , PipelineCache(..)
                                  , DescriptorUpdateTemplate(..)
                                  , SamplerYcbcrConversion(..)
                                  ) where

import GHC.Show (showParen)
import Numeric (showHex)
import Foreign.Storable (Storable)
import Data.Word (Word64)
import Vulkan.Core10.APIConstants (HasObjectType(..))
import Vulkan.Core10.APIConstants (IsHandle)
import Vulkan.Zero (Zero)
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_ACCELERATION_STRUCTURE_NV))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DEFERRED_OPERATION_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DISPLAY_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DISPLAY_MODE_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SURFACE_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SWAPCHAIN_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_VALIDATION_CACHE_EXT))
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (BufferView(..))
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandPool(..))
import Vulkan.Core10.Handles (DescriptorSet(..))
import Vulkan.Core10.Handles (DescriptorSetLayout(..))
import Vulkan.Core11.Handles (DescriptorUpdateTemplate(..))
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (DeviceMemory(..))
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.Core10.Handles (Framebuffer(..))
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Handles (ImageView(..))
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Handles (PipelineCache(..))
import Vulkan.Core10.Handles (PipelineLayout(..))
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (RenderPass(..))
import Vulkan.Core10.Handles (Sampler(..))
import Vulkan.Core11.Handles (SamplerYcbcrConversion(..))
import Vulkan.Core10.Handles (Semaphore(..))
import Vulkan.Core10.Handles (ShaderModule(..))
-- No documentation found for TopLevel "VkIndirectCommandsLayoutNV"
newtype IndirectCommandsLayoutNV = IndirectCommandsLayoutNV Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType IndirectCommandsLayoutNV where
  objectTypeAndHandle (IndirectCommandsLayoutNV h) = (OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV, h)
instance Show IndirectCommandsLayoutNV where
  showsPrec p (IndirectCommandsLayoutNV x) = showParen (p >= 11) (showString "IndirectCommandsLayoutNV 0x" . showHex x)


-- No documentation found for TopLevel "VkValidationCacheEXT"
newtype ValidationCacheEXT = ValidationCacheEXT Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType ValidationCacheEXT where
  objectTypeAndHandle (ValidationCacheEXT h) = (OBJECT_TYPE_VALIDATION_CACHE_EXT, h)
instance Show ValidationCacheEXT where
  showsPrec p (ValidationCacheEXT x) = showParen (p >= 11) (showString "ValidationCacheEXT 0x" . showHex x)


-- No documentation found for TopLevel "VkAccelerationStructureKHR"
newtype AccelerationStructureKHR = AccelerationStructureKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType AccelerationStructureKHR where
  objectTypeAndHandle (AccelerationStructureKHR h) = (OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR, h)
instance Show AccelerationStructureKHR where
  showsPrec p (AccelerationStructureKHR x) = showParen (p >= 11) (showString "AccelerationStructureKHR 0x" . showHex x)


-- No documentation found for TopLevel "VkAccelerationStructureNV"
newtype AccelerationStructureNV = AccelerationStructureNV Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType AccelerationStructureNV where
  objectTypeAndHandle (AccelerationStructureNV h) = (OBJECT_TYPE_ACCELERATION_STRUCTURE_NV, h)
instance Show AccelerationStructureNV where
  showsPrec p (AccelerationStructureNV x) = showParen (p >= 11) (showString "AccelerationStructureNV 0x" . showHex x)


-- No documentation found for TopLevel "VkPerformanceConfigurationINTEL"
newtype PerformanceConfigurationINTEL = PerformanceConfigurationINTEL Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType PerformanceConfigurationINTEL where
  objectTypeAndHandle (PerformanceConfigurationINTEL h) = (OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL, h)
instance Show PerformanceConfigurationINTEL where
  showsPrec p (PerformanceConfigurationINTEL x) = showParen (p >= 11) (showString "PerformanceConfigurationINTEL 0x" . showHex x)


-- No documentation found for TopLevel "VkDeferredOperationKHR"
newtype DeferredOperationKHR = DeferredOperationKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DeferredOperationKHR where
  objectTypeAndHandle (DeferredOperationKHR h) = (OBJECT_TYPE_DEFERRED_OPERATION_KHR, h)
instance Show DeferredOperationKHR where
  showsPrec p (DeferredOperationKHR x) = showParen (p >= 11) (showString "DeferredOperationKHR 0x" . showHex x)


-- No documentation found for TopLevel "VkPrivateDataSlotEXT"
newtype PrivateDataSlotEXT = PrivateDataSlotEXT Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType PrivateDataSlotEXT where
  objectTypeAndHandle (PrivateDataSlotEXT h) = (OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT, h)
instance Show PrivateDataSlotEXT where
  showsPrec p (PrivateDataSlotEXT x) = showParen (p >= 11) (showString "PrivateDataSlotEXT 0x" . showHex x)


-- No documentation found for TopLevel "VkDisplayKHR"
newtype DisplayKHR = DisplayKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DisplayKHR where
  objectTypeAndHandle (DisplayKHR h) = (OBJECT_TYPE_DISPLAY_KHR, h)
instance Show DisplayKHR where
  showsPrec p (DisplayKHR x) = showParen (p >= 11) (showString "DisplayKHR 0x" . showHex x)


-- No documentation found for TopLevel "VkDisplayModeKHR"
newtype DisplayModeKHR = DisplayModeKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DisplayModeKHR where
  objectTypeAndHandle (DisplayModeKHR h) = (OBJECT_TYPE_DISPLAY_MODE_KHR, h)
instance Show DisplayModeKHR where
  showsPrec p (DisplayModeKHR x) = showParen (p >= 11) (showString "DisplayModeKHR 0x" . showHex x)


-- No documentation found for TopLevel "VkSurfaceKHR"
newtype SurfaceKHR = SurfaceKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType SurfaceKHR where
  objectTypeAndHandle (SurfaceKHR h) = (OBJECT_TYPE_SURFACE_KHR, h)
instance Show SurfaceKHR where
  showsPrec p (SurfaceKHR x) = showParen (p >= 11) (showString "SurfaceKHR 0x" . showHex x)


-- No documentation found for TopLevel "VkSwapchainKHR"
newtype SwapchainKHR = SwapchainKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType SwapchainKHR where
  objectTypeAndHandle (SwapchainKHR h) = (OBJECT_TYPE_SWAPCHAIN_KHR, h)
instance Show SwapchainKHR where
  showsPrec p (SwapchainKHR x) = showParen (p >= 11) (showString "SwapchainKHR 0x" . showHex x)


-- No documentation found for TopLevel "VkDebugReportCallbackEXT"
newtype DebugReportCallbackEXT = DebugReportCallbackEXT Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DebugReportCallbackEXT where
  objectTypeAndHandle (DebugReportCallbackEXT h) = (OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT, h)
instance Show DebugReportCallbackEXT where
  showsPrec p (DebugReportCallbackEXT x) = showParen (p >= 11) (showString "DebugReportCallbackEXT 0x" . showHex x)


-- No documentation found for TopLevel "VkDebugUtilsMessengerEXT"
newtype DebugUtilsMessengerEXT = DebugUtilsMessengerEXT Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DebugUtilsMessengerEXT where
  objectTypeAndHandle (DebugUtilsMessengerEXT h) = (OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT, h)
instance Show DebugUtilsMessengerEXT where
  showsPrec p (DebugUtilsMessengerEXT x) = showParen (p >= 11) (showString "DebugUtilsMessengerEXT 0x" . showHex x)

