{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_maintenance4"
module Vulkan.Core13.Promoted_From_VK_KHR_maintenance4  ( getDeviceBufferMemoryRequirements
                                                        , getDeviceImageMemoryRequirements
                                                        , getDeviceImageSparseMemoryRequirements
                                                        , DeviceBufferMemoryRequirements(..)
                                                        , DeviceImageMemoryRequirements(..)
                                                        , PhysicalDeviceMaintenance4Features(..)
                                                        , PhysicalDeviceMaintenance4Properties(..)
                                                        , StructureType(..)
                                                        , ImageAspectFlagBits(..)
                                                        , ImageAspectFlags
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
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceBufferMemoryRequirements))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceImageMemoryRequirements))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceImageSparseMemoryRequirements))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(..))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceBufferMemoryRequirements
  :: FunPtr (Ptr Device_T -> Ptr DeviceBufferMemoryRequirements -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr DeviceBufferMemoryRequirements -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- | vkGetDeviceBufferMemoryRequirements - Returns the memory requirements
-- for specified Vulkan object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceBufferMemoryRequirements',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
getDeviceBufferMemoryRequirements :: forall a io
                                   . (Extendss MemoryRequirements2 a, PokeChain a, PeekChain a, MonadIO io)
                                  => -- | @device@ is the logical device intended to own the buffer.
                                     --
                                     -- #VUID-vkGetDeviceBufferMemoryRequirements-device-parameter# @device@
                                     -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                     Device
                                  -> -- | @pInfo@ is a pointer to a 'DeviceBufferMemoryRequirements' structure
                                     -- containing parameters required for the memory requirements query.
                                     --
                                     -- #VUID-vkGetDeviceBufferMemoryRequirements-pInfo-parameter# @pInfo@
                                     -- /must/ be a valid pointer to a valid 'DeviceBufferMemoryRequirements'
                                     -- structure
                                     ("info" ::: DeviceBufferMemoryRequirements)
                                  -> io (MemoryRequirements2 a)
getDeviceBufferMemoryRequirements device info = liftIO . evalContT $ do
  let vkGetDeviceBufferMemoryRequirementsPtr = pVkGetDeviceBufferMemoryRequirements (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceBufferMemoryRequirementsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceBufferMemoryRequirements is null" Nothing Nothing
  let vkGetDeviceBufferMemoryRequirements' = mkVkGetDeviceBufferMemoryRequirements vkGetDeviceBufferMemoryRequirementsPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ traceAroundEvent "vkGetDeviceBufferMemoryRequirements" (vkGetDeviceBufferMemoryRequirements' (deviceHandle (device)) pInfo (forgetExtensions (pPMemoryRequirements)))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceImageMemoryRequirements
  :: FunPtr (Ptr Device_T -> Ptr DeviceImageMemoryRequirements -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr DeviceImageMemoryRequirements -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- | vkGetDeviceImageMemoryRequirements - Returns the memory requirements for
-- specified Vulkan object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceImageMemoryRequirements',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
getDeviceImageMemoryRequirements :: forall a io
                                  . (Extendss MemoryRequirements2 a, PokeChain a, PeekChain a, MonadIO io)
                                 => -- | @device@ is the logical device intended to own the image.
                                    --
                                    -- #VUID-vkGetDeviceImageMemoryRequirements-device-parameter# @device@
                                    -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                    Device
                                 -> -- | @pInfo@ is a pointer to a 'DeviceImageMemoryRequirements' structure
                                    -- containing parameters required for the memory requirements query.
                                    --
                                    -- #VUID-vkGetDeviceImageMemoryRequirements-pInfo-parameter# @pInfo@ /must/
                                    -- be a valid pointer to a valid 'DeviceImageMemoryRequirements' structure
                                    ("info" ::: DeviceImageMemoryRequirements)
                                 -> io (MemoryRequirements2 a)
getDeviceImageMemoryRequirements device info = liftIO . evalContT $ do
  let vkGetDeviceImageMemoryRequirementsPtr = pVkGetDeviceImageMemoryRequirements (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceImageMemoryRequirementsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceImageMemoryRequirements is null" Nothing Nothing
  let vkGetDeviceImageMemoryRequirements' = mkVkGetDeviceImageMemoryRequirements vkGetDeviceImageMemoryRequirementsPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ traceAroundEvent "vkGetDeviceImageMemoryRequirements" (vkGetDeviceImageMemoryRequirements' (deviceHandle (device)) pInfo (forgetExtensions (pPMemoryRequirements)))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceImageSparseMemoryRequirements
  :: FunPtr (Ptr Device_T -> Ptr DeviceImageMemoryRequirements -> Ptr Word32 -> Ptr SparseImageMemoryRequirements2 -> IO ()) -> Ptr Device_T -> Ptr DeviceImageMemoryRequirements -> Ptr Word32 -> Ptr SparseImageMemoryRequirements2 -> IO ()

-- | vkGetDeviceImageSparseMemoryRequirements - Query the memory requirements
-- for a sparse image
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDeviceImageSparseMemoryRequirements-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDeviceImageSparseMemoryRequirements-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'DeviceImageMemoryRequirements' structure
--
-- -   #VUID-vkGetDeviceImageSparseMemoryRequirements-pSparseMemoryRequirementCount-parameter#
--     @pSparseMemoryRequirementCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   #VUID-vkGetDeviceImageSparseMemoryRequirements-pSparseMemoryRequirements-parameter#
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceImageMemoryRequirements',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.SparseImageMemoryRequirements2'
getDeviceImageSparseMemoryRequirements :: forall io
                                        . (MonadIO io)
                                       => -- | @device@ is the logical device intended to own the image.
                                          Device
                                       -> -- | @pInfo@ is a pointer to a 'DeviceImageMemoryRequirements' structure
                                          -- containing parameters required for the memory requirements query.
                                          ("info" ::: DeviceImageMemoryRequirements)
                                       -> io (("sparseMemoryRequirements" ::: Vector SparseImageMemoryRequirements2))
getDeviceImageSparseMemoryRequirements device info = liftIO . evalContT $ do
  let vkGetDeviceImageSparseMemoryRequirementsPtr = pVkGetDeviceImageSparseMemoryRequirements (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceImageSparseMemoryRequirementsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceImageSparseMemoryRequirements is null" Nothing Nothing
  let vkGetDeviceImageSparseMemoryRequirements' = mkVkGetDeviceImageSparseMemoryRequirements vkGetDeviceImageSparseMemoryRequirementsPtr
  let device' = deviceHandle (device)
  pInfo <- ContT $ withCStruct (info)
  pPSparseMemoryRequirementCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ traceAroundEvent "vkGetDeviceImageSparseMemoryRequirements" (vkGetDeviceImageSparseMemoryRequirements' device' pInfo (pPSparseMemoryRequirementCount) (nullPtr))
  pSparseMemoryRequirementCount <- lift $ peek @Word32 pPSparseMemoryRequirementCount
  pPSparseMemoryRequirements <- ContT $ bracket (callocBytes @SparseImageMemoryRequirements2 ((fromIntegral (pSparseMemoryRequirementCount)) * 64)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPSparseMemoryRequirements `advancePtrBytes` (i * 64) :: Ptr SparseImageMemoryRequirements2) . ($ ())) [0..(fromIntegral (pSparseMemoryRequirementCount)) - 1]
  lift $ traceAroundEvent "vkGetDeviceImageSparseMemoryRequirements" (vkGetDeviceImageSparseMemoryRequirements' device' pInfo (pPSparseMemoryRequirementCount) ((pPSparseMemoryRequirements)))
  pSparseMemoryRequirementCount' <- lift $ peek @Word32 pPSparseMemoryRequirementCount
  pSparseMemoryRequirements' <- lift $ generateM (fromIntegral (pSparseMemoryRequirementCount')) (\i -> peekCStruct @SparseImageMemoryRequirements2 (((pPSparseMemoryRequirements) `advancePtrBytes` (64 * (i)) :: Ptr SparseImageMemoryRequirements2)))
  pure $ (pSparseMemoryRequirements')


-- | VkDeviceBufferMemoryRequirements - (None)
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Buffer.BufferCreateInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceBufferMemoryRequirements',
-- 'Vulkan.Extensions.VK_KHR_maintenance4.getDeviceBufferMemoryRequirementsKHR'
data DeviceBufferMemoryRequirements = DeviceBufferMemoryRequirements
  { -- | @pCreateInfo@ is a pointer to a 'Vulkan.Core10.Buffer.BufferCreateInfo'
    -- structure containing parameters affecting creation of the buffer to
    -- query.
    --
    -- #VUID-VkDeviceBufferMemoryRequirements-pCreateInfo-parameter#
    -- @pCreateInfo@ /must/ be a valid pointer to a valid
    -- 'Vulkan.Core10.Buffer.BufferCreateInfo' structure
    createInfo :: SomeStruct BufferCreateInfo }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceBufferMemoryRequirements)
#endif
deriving instance Show DeviceBufferMemoryRequirements

instance ToCStruct DeviceBufferMemoryRequirements where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceBufferMemoryRequirements{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (BufferCreateInfo '[])) $ \cont -> withSomeCStruct @BufferCreateInfo (createInfo) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (BufferCreateInfo _)))) pCreateInfo''
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (BufferCreateInfo '[])) $ \cont -> withSomeCStruct @BufferCreateInfo ((SomeStruct zero)) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (BufferCreateInfo _)))) pCreateInfo''
    lift $ f

instance FromCStruct DeviceBufferMemoryRequirements where
  peekCStruct p = do
    pCreateInfo <- peekSomeCStruct . forgetExtensions =<< peek ((p `plusPtr` 16 :: Ptr (Ptr (BufferCreateInfo _))))
    pure $ DeviceBufferMemoryRequirements
             pCreateInfo

instance Zero DeviceBufferMemoryRequirements where
  zero = DeviceBufferMemoryRequirements
           (SomeStruct zero)


-- | VkDeviceImageMemoryRequirements - (None)
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceImageMemoryRequirementsKHR-pCreateInfo-06416# The
--     @pCreateInfo@::@pNext@ chain /must/ not contain a
--     'Vulkan.Extensions.VK_KHR_swapchain.ImageSwapchainCreateInfoKHR'
--     structure
--
-- -   #VUID-VkDeviceImageMemoryRequirements-pCreateInfo-06776# The
--     @pCreateInfo@::@pNext@ chain /must/ not contain a
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT'
--     structure.
--
-- -   #VUID-VkDeviceImageMemoryRequirementsKHR-pCreateInfo-06417# If
--     @pCreateInfo@::@format@ specifies a /multi-planar/ format and
--     @pCreateInfo@::@flags@ has
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--     set then @planeAspect@ /must/ not be
--     'Vulkan.Extensions.VK_KHR_maintenance4.IMAGE_ASPECT_NONE_KHR'
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
--     @pCreateInfo@::@tiling@ is
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
-- -   #VUID-VkDeviceImageMemoryRequirements-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS'
--
-- -   #VUID-VkDeviceImageMemoryRequirements-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkDeviceImageMemoryRequirements-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.Image.ImageCreateInfo' structure
--
-- -   #VUID-VkDeviceImageMemoryRequirements-planeAspect-parameter# If
--     @planeAspect@ is not @0@, @planeAspect@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits',
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceImageMemoryRequirements',
-- 'Vulkan.Extensions.VK_KHR_maintenance4.getDeviceImageMemoryRequirementsKHR',
-- 'getDeviceImageSparseMemoryRequirements',
-- 'Vulkan.Extensions.VK_KHR_maintenance4.getDeviceImageSparseMemoryRequirementsKHR'
data DeviceImageMemoryRequirements = DeviceImageMemoryRequirements
  { -- | @pCreateInfo@ is a pointer to a 'Vulkan.Core10.Image.ImageCreateInfo'
    -- structure containing parameters affecting creation of the image to
    -- query.
    createInfo :: SomeStruct ImageCreateInfo
  , -- | @planeAspect@ is a
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' value
    -- specifying the aspect corresponding to the image plane to query. This
    -- parameter is ignored unless @pCreateInfo@::@tiling@ is
    -- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
    -- or @pCreateInfo@::@flags@ has
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT' set.
    planeAspect :: ImageAspectFlagBits
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceImageMemoryRequirements)
#endif
deriving instance Show DeviceImageMemoryRequirements

instance ToCStruct DeviceImageMemoryRequirements where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceImageMemoryRequirements{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (ImageCreateInfo '[])) $ \cont -> withSomeCStruct @ImageCreateInfo (createInfo) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (ImageCreateInfo _)))) pCreateInfo''
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageAspectFlagBits)) (planeAspect)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (ImageCreateInfo '[])) $ \cont -> withSomeCStruct @ImageCreateInfo ((SomeStruct zero)) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (ImageCreateInfo _)))) pCreateInfo''
    lift $ f

instance FromCStruct DeviceImageMemoryRequirements where
  peekCStruct p = do
    pCreateInfo <- peekSomeCStruct . forgetExtensions =<< peek ((p `plusPtr` 16 :: Ptr (Ptr (ImageCreateInfo _))))
    planeAspect <- peek @ImageAspectFlagBits ((p `plusPtr` 24 :: Ptr ImageAspectFlagBits))
    pure $ DeviceImageMemoryRequirements
             pCreateInfo planeAspect

instance Zero DeviceImageMemoryRequirements where
  zero = DeviceImageMemoryRequirements
           (SomeStruct zero)
           zero


-- | VkPhysicalDeviceMaintenance4Features - Structure describing whether the
-- implementation supports maintenance4 functionality
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance4Features' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceMaintenance4Features' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance4Features = PhysicalDeviceMaintenance4Features
  { -- | #extension-features-maintenance4# @maintenance4@ indicates that the
    -- implementation supports the following:
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
deriving instance Generic (PhysicalDeviceMaintenance4Features)
#endif
deriving instance Show PhysicalDeviceMaintenance4Features

instance ToCStruct PhysicalDeviceMaintenance4Features where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance4Features{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (maintenance4))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance4Features where
  peekCStruct p = do
    maintenance4 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance4Features
             (bool32ToBool maintenance4)

instance Storable PhysicalDeviceMaintenance4Features where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance4Features where
  zero = PhysicalDeviceMaintenance4Features
           zero


-- | VkPhysicalDeviceMaintenance4Properties - Structure describing various
-- implementation-defined properties introduced with VK_KHR_maintenance4
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance4Properties' structure is included in
-- the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance4Properties = PhysicalDeviceMaintenance4Properties
  { -- | #extension-limits-maxBufferSize# @maxBufferSize@ is the maximum size
    -- 'Vulkan.Core10.Handles.Buffer' that /can/ be created.
    maxBufferSize :: DeviceSize }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance4Properties)
#endif
deriving instance Show PhysicalDeviceMaintenance4Properties

instance ToCStruct PhysicalDeviceMaintenance4Properties where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance4Properties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (maxBufferSize)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PhysicalDeviceMaintenance4Properties where
  peekCStruct p = do
    maxBufferSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ PhysicalDeviceMaintenance4Properties
             maxBufferSize

instance Storable PhysicalDeviceMaintenance4Properties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance4Properties where
  zero = PhysicalDeviceMaintenance4Properties
           zero

