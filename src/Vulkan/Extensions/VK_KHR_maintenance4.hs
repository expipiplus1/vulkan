{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance4 - device extension
--
-- == VK_KHR_maintenance4
--
-- [__Name String__]
--     @VK_KHR_maintenance4@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     414
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.1
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance4] @pdaniell-nv%0A<<Here describe the issue or question you have about the VK_KHR_maintenance4 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-10-25
--
-- [__Interactions and External Dependencies__]
--
--     -   Requires SPIR-V 1.2 for @LocalSizeId@
--
-- [__Contributors__]
--
--     -   Lionel Duc, NVIDIA
--
--     -   Jason Ekstrand, Intel
--
--     -   Spencer Fricke, Samsung
--
--     -   Tobias Hector, AMD
--
--     -   Lionel Landwerlin, Intel
--
--     -   Graeme Leese, Broadcom
--
--     -   Tom Olson, Arm
--
--     -   Stu Smith, AMD
--
--     -   Yiwei Zhang, Google
--
-- == Description
--
-- @VK_KHR_maintenance4@ adds a collection of minor features, none of which
-- would warrant an entire extension of their own.
--
-- The new features are as follows:
--
-- -   Allow the application to destroy their
--     'Vulkan.Core10.Handles.PipelineLayout' object immediately after it
--     was used to create another object. It is no longer necessary to keep
--     its handle valid while the created object is in use.
--
-- -   Add a new
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxBufferSize maxBufferSize>
--     implementation-defined limit for the maximum size
--     'Vulkan.Core10.Handles.Buffer' that /can/ be created.
--
-- -   Add support for the SPIR-V 1.2 @LocalSizeId@ execution mode, which
--     can be used as an alternative to @LocalSize@ to specify the local
--     workgroup size with specialization constants.
--
-- -   Add a guarantee that images created with identical creation
--     parameters will always have the same alignment requirements.
--
-- -   Add new 'getDeviceBufferMemoryRequirementsKHR',
--     'getDeviceImageMemoryRequirementsKHR', and
--     'getDeviceImageSparseMemoryRequirementsKHR' to allow the application
--     to query the image memory requirements without having to create an
--     image object and query it.
--
-- -   Relax the requirement that push constants must be initialized before
--     they are dynamically accessed.
--
-- -   Relax the interface matching rules to allow a larger output vector
--     to match with a smaller input vector, with additional values being
--     discarded.
--
-- -   Add a guarantee for buffer memory requirement that the size memory
--     requirement is never greater than the result of aligning create size
--     with the alignment memory requirement.
--
-- == New Commands
--
-- -   'getDeviceBufferMemoryRequirementsKHR'
--
-- -   'getDeviceImageMemoryRequirementsKHR'
--
-- -   'getDeviceImageSparseMemoryRequirementsKHR'
--
-- == New Structures
--
-- -   'DeviceBufferMemoryRequirementsKHR'
--
-- -   'DeviceImageMemoryRequirementsKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance4FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMaintenance4PropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_4_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_4_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_NONE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2021-08-18 (Piers Daniell)
--
--     -   Internal revisions
--
-- -   Revision 2, 2021-10-25 (Yiwei Zhang)
--
--     -   More guarantees on buffer memory requirements
--
-- == See Also
--
-- 'DeviceBufferMemoryRequirementsKHR', 'DeviceImageMemoryRequirementsKHR',
-- 'PhysicalDeviceMaintenance4FeaturesKHR',
-- 'PhysicalDeviceMaintenance4PropertiesKHR',
-- 'getDeviceBufferMemoryRequirementsKHR',
-- 'getDeviceImageMemoryRequirementsKHR',
-- 'getDeviceImageSparseMemoryRequirementsKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance4  ( getDeviceBufferMemoryRequirementsKHR
                                              , getDeviceImageMemoryRequirementsKHR
                                              , getDeviceImageSparseMemoryRequirementsKHR
                                              , DeviceBufferMemoryRequirementsKHR(..)
                                              , DeviceImageMemoryRequirementsKHR(..)
                                              , PhysicalDeviceMaintenance4FeaturesKHR(..)
                                              , PhysicalDeviceMaintenance4PropertiesKHR(..)
                                              , KHR_MAINTENANCE_4_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE_4_SPEC_VERSION
                                              , KHR_MAINTENANCE_4_EXTENSION_NAME
                                              , pattern KHR_MAINTENANCE_4_EXTENSION_NAME
                                              ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Buffer (BufferCreateInfo)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceBufferMemoryRequirementsKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceImageMemoryRequirementsKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceImageSparseMemoryRequirementsKHR))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits)
import Vulkan.Core10.Image (ImageCreateInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (SparseImageMemoryRequirements2)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceBufferMemoryRequirementsKHR
  :: FunPtr (Ptr Device_T -> Ptr DeviceBufferMemoryRequirementsKHR -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr DeviceBufferMemoryRequirementsKHR -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- | vkGetDeviceBufferMemoryRequirementsKHR - Returns the memory requirements
-- for specified Vulkan object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceBufferMemoryRequirementsKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
getDeviceBufferMemoryRequirementsKHR :: forall a io
                                      . (Extendss MemoryRequirements2 a, PokeChain a, PeekChain a, MonadIO io)
                                     => -- | @device@ is the logical device intended to own the buffer.
                                        --
                                        -- #VUID-vkGetDeviceBufferMemoryRequirementsKHR-device-parameter# @device@
                                        -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                        Device
                                     -> -- | @pInfo@ is a pointer to a 'DeviceBufferMemoryRequirementsKHR' structure
                                        -- containing parameters required for the memory requirements query.
                                        --
                                        -- #VUID-vkGetDeviceBufferMemoryRequirementsKHR-pInfo-parameter# @pInfo@
                                        -- /must/ be a valid pointer to a valid 'DeviceBufferMemoryRequirementsKHR'
                                        -- structure
                                        ("info" ::: DeviceBufferMemoryRequirementsKHR)
                                     -> io (MemoryRequirements2 a)
getDeviceBufferMemoryRequirementsKHR device info = liftIO . evalContT $ do
  let vkGetDeviceBufferMemoryRequirementsKHRPtr = pVkGetDeviceBufferMemoryRequirementsKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceBufferMemoryRequirementsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceBufferMemoryRequirementsKHR is null" Nothing Nothing
  let vkGetDeviceBufferMemoryRequirementsKHR' = mkVkGetDeviceBufferMemoryRequirementsKHR vkGetDeviceBufferMemoryRequirementsKHRPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ traceAroundEvent "vkGetDeviceBufferMemoryRequirementsKHR" (vkGetDeviceBufferMemoryRequirementsKHR' (deviceHandle (device)) pInfo (forgetExtensions (pPMemoryRequirements)))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceImageMemoryRequirementsKHR
  :: FunPtr (Ptr Device_T -> Ptr DeviceImageMemoryRequirementsKHR -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr DeviceImageMemoryRequirementsKHR -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- | vkGetDeviceImageMemoryRequirementsKHR - Returns the memory requirements
-- for specified Vulkan object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceImageMemoryRequirementsKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
getDeviceImageMemoryRequirementsKHR :: forall a io
                                     . (Extendss MemoryRequirements2 a, PokeChain a, PeekChain a, MonadIO io)
                                    => -- | @device@ is the logical device intended to own the image.
                                       --
                                       -- #VUID-vkGetDeviceImageMemoryRequirementsKHR-device-parameter# @device@
                                       -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                       Device
                                    -> -- | @pInfo@ is a pointer to a 'DeviceImageMemoryRequirementsKHR' structure
                                       -- containing parameters required for the memory requirements query.
                                       --
                                       -- #VUID-vkGetDeviceImageMemoryRequirementsKHR-pInfo-parameter# @pInfo@
                                       -- /must/ be a valid pointer to a valid 'DeviceImageMemoryRequirementsKHR'
                                       -- structure
                                       ("info" ::: DeviceImageMemoryRequirementsKHR)
                                    -> io (MemoryRequirements2 a)
getDeviceImageMemoryRequirementsKHR device info = liftIO . evalContT $ do
  let vkGetDeviceImageMemoryRequirementsKHRPtr = pVkGetDeviceImageMemoryRequirementsKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceImageMemoryRequirementsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceImageMemoryRequirementsKHR is null" Nothing Nothing
  let vkGetDeviceImageMemoryRequirementsKHR' = mkVkGetDeviceImageMemoryRequirementsKHR vkGetDeviceImageMemoryRequirementsKHRPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ traceAroundEvent "vkGetDeviceImageMemoryRequirementsKHR" (vkGetDeviceImageMemoryRequirementsKHR' (deviceHandle (device)) pInfo (forgetExtensions (pPMemoryRequirements)))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceImageSparseMemoryRequirementsKHR
  :: FunPtr (Ptr Device_T -> Ptr DeviceImageMemoryRequirementsKHR -> Ptr Word32 -> Ptr SparseImageMemoryRequirements2 -> IO ()) -> Ptr Device_T -> Ptr DeviceImageMemoryRequirementsKHR -> Ptr Word32 -> Ptr SparseImageMemoryRequirements2 -> IO ()

-- | vkGetDeviceImageSparseMemoryRequirementsKHR - Query the memory
-- requirements for a sparse image
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDeviceImageSparseMemoryRequirementsKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDeviceImageSparseMemoryRequirementsKHR-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'DeviceImageMemoryRequirementsKHR' structure
--
-- -   #VUID-vkGetDeviceImageSparseMemoryRequirementsKHR-pSparseMemoryRequirementCount-parameter#
--     @pSparseMemoryRequirementCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   #VUID-vkGetDeviceImageSparseMemoryRequirementsKHR-pSparseMemoryRequirements-parameter#
--     If the value referenced by @pSparseMemoryRequirementCount@ is not
--     @0@, and @pSparseMemoryRequirements@ is not @NULL@,
--     @pSparseMemoryRequirements@ /must/ be a valid pointer to an array of
--     @pSparseMemoryRequirementCount@
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.SparseImageMemoryRequirements2'
--     structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceImageMemoryRequirementsKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.SparseImageMemoryRequirements2'
getDeviceImageSparseMemoryRequirementsKHR :: forall io
                                           . (MonadIO io)
                                          => -- | @device@ is the logical device intended to own the image.
                                             Device
                                          -> -- | @pInfo@ is a pointer to a 'DeviceImageMemoryRequirementsKHR' structure
                                             -- containing parameters required for the memory requirements query.
                                             ("info" ::: DeviceImageMemoryRequirementsKHR)
                                          -> io (("sparseMemoryRequirements" ::: Vector SparseImageMemoryRequirements2))
getDeviceImageSparseMemoryRequirementsKHR device info = liftIO . evalContT $ do
  let vkGetDeviceImageSparseMemoryRequirementsKHRPtr = pVkGetDeviceImageSparseMemoryRequirementsKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceImageSparseMemoryRequirementsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceImageSparseMemoryRequirementsKHR is null" Nothing Nothing
  let vkGetDeviceImageSparseMemoryRequirementsKHR' = mkVkGetDeviceImageSparseMemoryRequirementsKHR vkGetDeviceImageSparseMemoryRequirementsKHRPtr
  let device' = deviceHandle (device)
  pInfo <- ContT $ withCStruct (info)
  pPSparseMemoryRequirementCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ traceAroundEvent "vkGetDeviceImageSparseMemoryRequirementsKHR" (vkGetDeviceImageSparseMemoryRequirementsKHR' device' pInfo (pPSparseMemoryRequirementCount) (nullPtr))
  pSparseMemoryRequirementCount <- lift $ peek @Word32 pPSparseMemoryRequirementCount
  pPSparseMemoryRequirements <- ContT $ bracket (callocBytes @SparseImageMemoryRequirements2 ((fromIntegral (pSparseMemoryRequirementCount)) * 64)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPSparseMemoryRequirements `advancePtrBytes` (i * 64) :: Ptr SparseImageMemoryRequirements2) . ($ ())) [0..(fromIntegral (pSparseMemoryRequirementCount)) - 1]
  lift $ traceAroundEvent "vkGetDeviceImageSparseMemoryRequirementsKHR" (vkGetDeviceImageSparseMemoryRequirementsKHR' device' pInfo (pPSparseMemoryRequirementCount) ((pPSparseMemoryRequirements)))
  pSparseMemoryRequirementCount' <- lift $ peek @Word32 pPSparseMemoryRequirementCount
  pSparseMemoryRequirements' <- lift $ generateM (fromIntegral (pSparseMemoryRequirementCount')) (\i -> peekCStruct @SparseImageMemoryRequirements2 (((pPSparseMemoryRequirements) `advancePtrBytes` (64 * (i)) :: Ptr SparseImageMemoryRequirements2)))
  pure $ (pSparseMemoryRequirements')


-- | VkDeviceBufferMemoryRequirementsKHR - (None)
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>,
-- 'Vulkan.Core10.Buffer.BufferCreateInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceBufferMemoryRequirementsKHR'
data DeviceBufferMemoryRequirementsKHR = DeviceBufferMemoryRequirementsKHR
  { -- | @pCreateInfo@ is a pointer to a 'Vulkan.Core10.Buffer.BufferCreateInfo'
    -- structure containing parameters affecting creation of the buffer to
    -- query.
    --
    -- #VUID-VkDeviceBufferMemoryRequirementsKHR-pCreateInfo-parameter#
    -- @pCreateInfo@ /must/ be a valid pointer to a valid
    -- 'Vulkan.Core10.Buffer.BufferCreateInfo' structure
    createInfo :: SomeStruct BufferCreateInfo }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceBufferMemoryRequirementsKHR)
#endif
deriving instance Show DeviceBufferMemoryRequirementsKHR

instance ToCStruct DeviceBufferMemoryRequirementsKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceBufferMemoryRequirementsKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (BufferCreateInfo '[])) $ \cont -> withSomeCStruct @BufferCreateInfo (createInfo) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (BufferCreateInfo _)))) pCreateInfo''
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (BufferCreateInfo '[])) $ \cont -> withSomeCStruct @BufferCreateInfo ((SomeStruct zero)) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (BufferCreateInfo _)))) pCreateInfo''
    lift $ f

instance FromCStruct DeviceBufferMemoryRequirementsKHR where
  peekCStruct p = do
    pCreateInfo <- peekSomeCStruct . forgetExtensions =<< peek ((p `plusPtr` 16 :: Ptr (Ptr (BufferCreateInfo _))))
    pure $ DeviceBufferMemoryRequirementsKHR
             pCreateInfo

instance Zero DeviceBufferMemoryRequirementsKHR where
  zero = DeviceBufferMemoryRequirementsKHR
           (SomeStruct zero)


-- | VkDeviceImageMemoryRequirementsKHR - (None)
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceImageMemoryRequirementsKHR-pCreateInfo-06416# The
--     @pCreateInfo@::@pNext@ chain /must/ not contain a
--     'Vulkan.Extensions.VK_KHR_swapchain.ImageSwapchainCreateInfoKHR'
--     structure
--
-- -   #VUID-VkDeviceImageMemoryRequirementsKHR-pCreateInfo-06417# If
--     @pCreateInfo@::@flags@ has
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--     set then @planeAspect@ /must/ not be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_NONE_KHR'
--
-- -   #VUID-VkDeviceImageMemoryRequirementsKHR-pCreateInfo-06418# If
--     @pCreateInfo@::@flags@ has
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--     set then @planeAspect@ /must/ not be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_NONE_KHR'
--
-- -   #VUID-VkDeviceImageMemoryRequirementsKHR-pCreateInfo-06419# If
--     @pCreateInfo@::@flags@ has
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--     set and if the @pCreateInfo@::@tiling@ is
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
-- -   #VUID-VkDeviceImageMemoryRequirementsKHR-pCreateInfo-06420# If
--     @pCreateInfo@::@flags@ has
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--     set and the @pCreateInfo@::@tiling@ is
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
-- -   #VUID-VkDeviceImageMemoryRequirementsKHR-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS_KHR'
--
-- -   #VUID-VkDeviceImageMemoryRequirementsKHR-pNext-pNext# @pNext@ /must/
--     be @NULL@
--
-- -   #VUID-VkDeviceImageMemoryRequirementsKHR-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.Image.ImageCreateInfo' structure
--
-- -   #VUID-VkDeviceImageMemoryRequirementsKHR-planeAspect-parameter#
--     @planeAspect@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits',
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceImageMemoryRequirementsKHR',
-- 'getDeviceImageSparseMemoryRequirementsKHR'
data DeviceImageMemoryRequirementsKHR = DeviceImageMemoryRequirementsKHR
  { -- | @pCreateInfo@ is a pointer to a 'Vulkan.Core10.Image.ImageCreateInfo'
    -- structure containing parameters affecting creation of the image to
    -- query.
    createInfo :: SomeStruct ImageCreateInfo
  , -- | @planeAspect@ is a
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' value
    -- specifying the aspect corresponding to the image plane to query. This
    -- parameter is ignored unless @pCreateInfo@::@flags@ has
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT' or
    -- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
    -- set.
    planeAspect :: ImageAspectFlagBits
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceImageMemoryRequirementsKHR)
#endif
deriving instance Show DeviceImageMemoryRequirementsKHR

instance ToCStruct DeviceImageMemoryRequirementsKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceImageMemoryRequirementsKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (ImageCreateInfo '[])) $ \cont -> withSomeCStruct @ImageCreateInfo (createInfo) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (ImageCreateInfo _)))) pCreateInfo''
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageAspectFlagBits)) (planeAspect)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (ImageCreateInfo '[])) $ \cont -> withSomeCStruct @ImageCreateInfo ((SomeStruct zero)) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (ImageCreateInfo _)))) pCreateInfo''
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageAspectFlagBits)) (zero)
    lift $ f

instance FromCStruct DeviceImageMemoryRequirementsKHR where
  peekCStruct p = do
    pCreateInfo <- peekSomeCStruct . forgetExtensions =<< peek ((p `plusPtr` 16 :: Ptr (Ptr (ImageCreateInfo _))))
    planeAspect <- peek @ImageAspectFlagBits ((p `plusPtr` 24 :: Ptr ImageAspectFlagBits))
    pure $ DeviceImageMemoryRequirementsKHR
             pCreateInfo planeAspect

instance Zero DeviceImageMemoryRequirementsKHR where
  zero = DeviceImageMemoryRequirementsKHR
           (SomeStruct zero)
           zero


-- | VkPhysicalDeviceMaintenance4FeaturesKHR - Structure describing whether
-- the implementation supports maintenance4 functionality
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance4FeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceMaintenance4FeaturesKHR' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance4FeaturesKHR = PhysicalDeviceMaintenance4FeaturesKHR
  { -- | #features-maintenance4# @maintenance4@ indicates that the implementation
    -- supports the following:
    --
    -- -   The application /may/ destroy a
    --     'Vulkan.Core10.Handles.PipelineLayout' object immediately after
    --     using it to create another object.
    --
    -- -   @LocalSizeId@ /can/ be used as an alternative to @LocalSize@ to
    --     specify the local workgroup size with specialization constants.
    --
    -- -   Images created with identical creation parameters will always have
    --     the same alignment requirements.
    --
    -- -   The size memory requirement of a buffer or image is never greater
    --     than that of another buffer or image created with a greater or equal
    --     size.
    --
    -- -   Push constants do not have to be initialized before they are
    --     dynamically accessed.
    --
    -- -   The interface matching rules allow a larger output vector to match
    --     with a smaller input vector, with additional values being discarded.
    maintenance4 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance4FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance4FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance4FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance4FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (maintenance4))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance4FeaturesKHR where
  peekCStruct p = do
    maintenance4 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance4FeaturesKHR
             (bool32ToBool maintenance4)

instance Storable PhysicalDeviceMaintenance4FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance4FeaturesKHR where
  zero = PhysicalDeviceMaintenance4FeaturesKHR
           zero


-- | VkPhysicalDeviceMaintenance4PropertiesKHR - Structure describing various
-- implementation-defined properties introduced with VK_KHR_maintenance4
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance4PropertiesKHR' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance4PropertiesKHR = PhysicalDeviceMaintenance4PropertiesKHR
  { -- | #limits-maxBufferSize# @maxBufferSize@ is the maximum size
    -- 'Vulkan.Core10.Handles.Buffer' that /can/ be created.
    maxBufferSize :: DeviceSize }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance4PropertiesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance4PropertiesKHR

instance ToCStruct PhysicalDeviceMaintenance4PropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance4PropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (maxBufferSize)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PhysicalDeviceMaintenance4PropertiesKHR where
  peekCStruct p = do
    maxBufferSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ PhysicalDeviceMaintenance4PropertiesKHR
             maxBufferSize

instance Storable PhysicalDeviceMaintenance4PropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance4PropertiesKHR where
  zero = PhysicalDeviceMaintenance4PropertiesKHR
           zero


type KHR_MAINTENANCE_4_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_4_SPEC_VERSION"
pattern KHR_MAINTENANCE_4_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE_4_SPEC_VERSION = 2


type KHR_MAINTENANCE_4_EXTENSION_NAME = "VK_KHR_maintenance4"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_4_EXTENSION_NAME"
pattern KHR_MAINTENANCE_4_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE_4_EXTENSION_NAME = "VK_KHR_maintenance4"

