{-# language CPP #-}
-- | = Name
--
-- VK_KHR_present_id2 - device extension
--
-- = VK_KHR_present_id2
--
-- [__Name String__]
--     @VK_KHR_present_id2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     480
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--
-- [__Contact__]
--
--     -   Daniel Stone
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_present_id2.adoc VK_KHR_present_id2>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   James Jones, NVIDIA
--
--     -   Daniel Stone, Collabora
--
--     -   Derek Foreman, Collabora
--
--     -   /contributors to
--         \`<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_id VK_KHR_present_id>\`/
--
-- == Description
--
-- This device extension allows an application that uses the
-- @VK_KHR_swapchain@ extension to provide an identifier for present
-- operations on a swapchain. An application /can/ use this to reference
-- specific present operations in other extensions.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePresentId2FeaturesKHR'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'PresentId2KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfaceCapabilitiesPresentId2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_PRESENT_ID_2_EXTENSION_NAME'
--
-- -   'KHR_PRESENT_ID_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_2_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_ID_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_ID_2_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PRESENT_ID_2_BIT_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2022-05-10 (Daniel Stone)
--
--     -   Repurposed VK_KHR_present_id to be driven by surface
--         capabilities
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_present_id2 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_present_id2  ( PhysicalDevicePresentId2FeaturesKHR(..)
                                             , PresentId2KHR(..)
                                             , SurfaceCapabilitiesPresentId2KHR(..)
                                             , KHR_PRESENT_ID_2_SPEC_VERSION
                                             , pattern KHR_PRESENT_ID_2_SPEC_VERSION
                                             , KHR_PRESENT_ID_2_EXTENSION_NAME
                                             , pattern KHR_PRESENT_ID_2_EXTENSION_NAME
                                             , SwapchainCreateFlagBitsKHR(..)
                                             , SwapchainCreateFlagsKHR
                                             ) where

import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_2_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_ID_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_ID_2_KHR))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagsKHR)
-- | VkPhysicalDevicePresentId2FeaturesKHR - Structure indicating support for
-- present id 2
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePresentId2FeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDevicePresentId2FeaturesKHR', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_id2 VK_KHR_present_id2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePresentId2FeaturesKHR = PhysicalDevicePresentId2FeaturesKHR
  { -- | #features-presentId2# @presentId2@ indicates that the implementation
    -- supports specifying present ID values in the 'PresentId2KHR' extension
    -- to the 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' struct.
    presentId2 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePresentId2FeaturesKHR)
#endif
deriving instance Show PhysicalDevicePresentId2FeaturesKHR

instance ToCStruct PhysicalDevicePresentId2FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePresentId2FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_2_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentId2))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_2_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePresentId2FeaturesKHR where
  peekCStruct p = do
    presentId2 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePresentId2FeaturesKHR
             (bool32ToBool presentId2)

instance Storable PhysicalDevicePresentId2FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePresentId2FeaturesKHR where
  zero = PhysicalDevicePresentId2FeaturesKHR
           zero


-- | VkPresentId2KHR - The list of presentation identifiers
--
-- = Description
--
-- For applications to be able to reference specific presentation events
-- queued by a call to
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR', an identifier
-- needs to be associated with them.
--
-- When the 'SurfaceCapabilitiesPresentId2KHR' surface capability is
-- present for a surface, applications /can/ include the 'PresentId2KHR'
-- structure in the @pNext@ chain of the
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' structure to
-- associate an identifier with each presentation request. The
-- @pPresentIds@ provides an identifier for the swapchain present at the
-- corresponding index in
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'’s @pSwapchains@
-- array.
--
-- If this presentId is non-zero, then the application /can/ later use this
-- value to refer to that image presentation. A value of zero indicates
-- that this presentation has no associated presentId. A non-zero presentId
-- /must/ be greater than any non-zero presentId passed previously by the
-- application for the same swapchain.
--
-- If a non-zero presentId was provided, this may be used with
-- 'Vulkan.Extensions.VK_KHR_present_wait2.waitForPresent2KHR' for the
-- application to synchronize against the presentation engine’s processing
-- of the presentation request.
--
-- The ID namespace used by this extension /must/ be shared with other
-- extensions that allow the application to provide a 64-bit monotonically
-- increasing presentation ID, such as the original VK_KHR_present_id.
--
-- This is to allow existing extensions that depend on VK_KHR_present_id to
-- use VK_KHR_present_id2 provided IDs without change, as well as to
-- simplify writing future extensions that require application provided
-- presentation IDs.
--
-- == Valid Usage
--
-- -   #VUID-VkPresentId2KHR-swapchainCount-10818# @swapchainCount@ /must/
--     be the same value as
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@swapchainCount@,
--     where this 'PresentId2KHR' is in the @pNext@ chain of the
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' structure
--
-- -   #VUID-VkPresentId2KHR-presentIds-10819# Each non-zero entry in
--     @presentIds@ /must/ be greater than all previously submitted present
--     ids for the associated swapchain in
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@pSwapchains@
--
-- -   #VUID-VkPresentId2KHR-None-10820# The swapchain must have been
--     created with
--     'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PRESENT_ID_2_BIT_KHR'
--     bit set in the
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR'
--     field
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPresentId2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_ID_2_KHR'
--
-- -   #VUID-VkPresentId2KHR-pPresentIds-parameter# If @pPresentIds@ is not
--     @NULL@, @pPresentIds@ /must/ be a valid pointer to an array of
--     @swapchainCount@ @uint64_t@ values
--
-- -   #VUID-VkPresentId2KHR-swapchainCount-arraylength# @swapchainCount@
--     /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_id2 VK_KHR_present_id2>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PresentId2KHR = PresentId2KHR
  { -- | @swapchainCount@ is the number of swapchains being presented to the
    -- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' command.
    swapchainCount :: Word32
  , -- | @pPresentIds@ is @NULL@ or a pointer to an array of uint64_t with
    -- @swapchainCount@ entries. If not @NULL@, each non-zero value in
    -- @pPresentIds@ specifies the present id to be associated with the
    -- presentation of the swapchain with the same index in the
    -- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' call.
    presentIds :: Vector Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentId2KHR)
#endif
deriving instance Show PresentId2KHR

instance ToCStruct PresentId2KHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentId2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_ID_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let pPresentIdsLength = Data.Vector.length $ (presentIds)
    swapchainCount'' <- lift $ if (swapchainCount) == 0
      then pure $ fromIntegral pPresentIdsLength
      else do
        unless (fromIntegral pPresentIdsLength == (swapchainCount) || pPresentIdsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pPresentIds must be empty or have 'swapchainCount' elements" Nothing Nothing
        pure (swapchainCount)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (swapchainCount'')
    pPresentIds'' <- if Data.Vector.null (presentIds)
      then pure nullPtr
      else do
        pPPresentIds <- ContT $ allocaBytes @Word64 (((Data.Vector.length (presentIds))) * 8)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPPresentIds `plusPtr` (8 * (i)) :: Ptr Word64) (e)) ((presentIds))
        pure $ pPPresentIds
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word64))) pPresentIds''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_ID_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PresentId2KHR where
  peekCStruct p = do
    swapchainCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPresentIds <- peek @(Ptr Word64) ((p `plusPtr` 24 :: Ptr (Ptr Word64)))
    let pPresentIdsLength = if pPresentIds == nullPtr then 0 else (fromIntegral swapchainCount)
    pPresentIds' <- generateM pPresentIdsLength (\i -> peek @Word64 ((pPresentIds `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pure $ PresentId2KHR
             swapchainCount pPresentIds'

instance Zero PresentId2KHR where
  zero = PresentId2KHR
           zero
           mempty


-- | VkSurfaceCapabilitiesPresentId2KHR - Structure describing
-- presentation-ID capabilities of a surface
--
-- = Description
--
-- This structure /can/ be included in the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR'
-- to determine support for present-wait. If @presentId2Supported@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE', it indicates that attaching an
-- ID to presentation requests is not possible for this surface.
--
-- Applications /must/ not attempt to include 'PresentId2KHR' in the
-- @pNext@ chain of a 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'
-- if @presentId2Supported@ is 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_id2 VK_KHR_present_id2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfaceCapabilitiesPresentId2KHR = SurfaceCapabilitiesPresentId2KHR
  { -- | @presentId2Supported@ is a boolean describing whether the surface is
    -- able to support the present-ID extension
    presentId2Supported :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceCapabilitiesPresentId2KHR)
#endif
deriving instance Show SurfaceCapabilitiesPresentId2KHR

instance ToCStruct SurfaceCapabilitiesPresentId2KHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilitiesPresentId2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_ID_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentId2Supported))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_ID_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SurfaceCapabilitiesPresentId2KHR where
  peekCStruct p = do
    presentId2Supported <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ SurfaceCapabilitiesPresentId2KHR
             (bool32ToBool presentId2Supported)

instance Storable SurfaceCapabilitiesPresentId2KHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceCapabilitiesPresentId2KHR where
  zero = SurfaceCapabilitiesPresentId2KHR
           zero


type KHR_PRESENT_ID_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PRESENT_ID_2_SPEC_VERSION"
pattern KHR_PRESENT_ID_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PRESENT_ID_2_SPEC_VERSION = 1


type KHR_PRESENT_ID_2_EXTENSION_NAME = "VK_KHR_present_id2"

-- No documentation found for TopLevel "VK_KHR_PRESENT_ID_2_EXTENSION_NAME"
pattern KHR_PRESENT_ID_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PRESENT_ID_2_EXTENSION_NAME = "VK_KHR_present_id2"

