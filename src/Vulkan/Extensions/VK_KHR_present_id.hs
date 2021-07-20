{-# language CPP #-}
-- | = Name
--
-- VK_KHR_present_id - device extension
--
-- == VK_KHR_present_id
--
-- [__Name String__]
--     @VK_KHR_present_id@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     295
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_swapchain@
--
-- [__Contact__]
--
--     -   Keith Packard
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_present_id:%20&body=@keithp%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-15
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Keith Packard, Valve
--
--     -   Ian Elliott, Google
--
--     -   Alon Or-bach, Samsung
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
--     -   'PhysicalDevicePresentIdFeaturesKHR'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'PresentIdKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PRESENT_ID_EXTENSION_NAME'
--
-- -   'KHR_PRESENT_ID_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_ID_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2019-05-15 (Keith Packard)
--
--     -   Initial version
--
-- = See Also
--
-- 'PhysicalDevicePresentIdFeaturesKHR', 'PresentIdKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_id Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_present_id  ( PhysicalDevicePresentIdFeaturesKHR(..)
                                            , PresentIdKHR(..)
                                            , KHR_PRESENT_ID_SPEC_VERSION
                                            , pattern KHR_PRESENT_ID_SPEC_VERSION
                                            , KHR_PRESENT_ID_EXTENSION_NAME
                                            , pattern KHR_PRESENT_ID_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_ID_KHR))
-- | VkPhysicalDevicePresentIdFeaturesKHR - Structure indicating support for
-- present id
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePresentIdFeaturesKHR' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePresentIdFeaturesKHR' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePresentIdFeaturesKHR = PhysicalDevicePresentIdFeaturesKHR
  { -- | #features-presentId# @presentId@ indicates that the implementation
    -- supports specifying present ID values in the 'PresentIdKHR' extension to
    -- the 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' struct.
    presentId :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePresentIdFeaturesKHR)
#endif
deriving instance Show PhysicalDevicePresentIdFeaturesKHR

instance ToCStruct PhysicalDevicePresentIdFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePresentIdFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentId))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePresentIdFeaturesKHR where
  peekCStruct p = do
    presentId <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePresentIdFeaturesKHR
             (bool32ToBool presentId)

instance Storable PhysicalDevicePresentIdFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePresentIdFeaturesKHR where
  zero = PhysicalDevicePresentIdFeaturesKHR
           zero


-- | VkPresentIdKHR - The list of presentation identifiers
--
-- = Description
--
-- For applications to be able to reference specific presentation events
-- queued by a call to
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR', an identifier
-- needs to be associated with them. When the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-presentId presentId>
-- feature is enabled, applications /can/ include the 'PresentIdKHR'
-- structure in the @pNext@ chain of the
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' structure to supply
-- identifiers.
--
-- Each 'Vulkan.Extensions.Handles.SwapchainKHR' has a presentId associated
-- with it. This value is initially set to zero when the
-- 'Vulkan.Extensions.Handles.SwapchainKHR' is created.
--
-- When a 'PresentIdKHR' structure with a non-NULL @pPresentIds@ is
-- included in the @pNext@ chain of a
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' structure, each
-- @pSwapchains@ entry has a presentId associated in the @pPresentIds@
-- array at the same index as the swapchain in the @pSwapchains@ array. If
-- this presentId is non-zero, then the application /can/ later use this
-- value to refer to that image presentation. A value of zero indicates
-- that this presentation has no associated presentId. A non-zero presentId
-- /must/ be greater than any non-zero presentId passed previously by the
-- application for the same swapchain.
--
-- There is no requirement for any precise timing relationship between the
-- presentation of the image to the user and the update of the presentId
-- value, but implementations /should/ make this as close as possible to
-- the presentation of the first pixel in the new image to the user.
--
-- == Valid Usage
--
-- -   #VUID-VkPresentIdKHR-swapchainCount-04998# @swapchainCount@ /must/
--     be the same value as
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@swapchainCount@,
--     where this 'PresentIdKHR' is in the pNext-chain of the
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' structure
--
-- -   #VUID-VkPresentIdKHR-presentIds-04999# Each @presentIds@ entry
--     /must/ be greater than any previous @presentIds@ entry passed for
--     the associated @pSwapchains@ entry
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPresentIdKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_ID_KHR'
--
-- -   #VUID-VkPresentIdKHR-pPresentIds-parameter# If @pPresentIds@ is not
--     @NULL@, @pPresentIds@ /must/ be a valid pointer to an array of
--     @swapchainCount@ @uint64_t@ values
--
-- -   #VUID-VkPresentIdKHR-swapchainCount-arraylength# @swapchainCount@
--     /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PresentIdKHR = PresentIdKHR
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
deriving instance Generic (PresentIdKHR)
#endif
deriving instance Show PresentIdKHR

instance ToCStruct PresentIdKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentIdKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_ID_KHR)
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
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_ID_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PresentIdKHR where
  peekCStruct p = do
    swapchainCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPresentIds <- peek @(Ptr Word64) ((p `plusPtr` 24 :: Ptr (Ptr Word64)))
    let pPresentIdsLength = if pPresentIds == nullPtr then 0 else (fromIntegral swapchainCount)
    pPresentIds' <- generateM pPresentIdsLength (\i -> peek @Word64 ((pPresentIds `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pure $ PresentIdKHR
             swapchainCount pPresentIds'

instance Zero PresentIdKHR where
  zero = PresentIdKHR
           zero
           mempty


type KHR_PRESENT_ID_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PRESENT_ID_SPEC_VERSION"
pattern KHR_PRESENT_ID_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PRESENT_ID_SPEC_VERSION = 1


type KHR_PRESENT_ID_EXTENSION_NAME = "VK_KHR_present_id"

-- No documentation found for TopLevel "VK_KHR_PRESENT_ID_EXTENSION_NAME"
pattern KHR_PRESENT_ID_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PRESENT_ID_EXTENSION_NAME = "VK_KHR_present_id"

