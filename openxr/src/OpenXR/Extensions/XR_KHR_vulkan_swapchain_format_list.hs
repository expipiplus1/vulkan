{-# language CPP #-}
-- | = Name
--
-- XR_KHR_vulkan_swapchain_format_list - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_vulkan_swapchain_format_list  XR_KHR_vulkan_swapchain_format_list>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 15
--
-- = Revision
--
-- 3
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- -   Requires @@
--
-- = See Also
--
-- 'VulkanSwapchainFormatListCreateInfoKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_vulkan_swapchain_format_list OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_vulkan_swapchain_format_list  ( VulkanSwapchainFormatListCreateInfoKHR(..)
                                                              , KHR_vulkan_swapchain_format_list_SPEC_VERSION
                                                              , pattern KHR_vulkan_swapchain_format_list_SPEC_VERSION
                                                              , KHR_VULKAN_SWAPCHAIN_FORMAT_LIST_EXTENSION_NAME
                                                              , pattern KHR_VULKAN_SWAPCHAIN_FORMAT_LIST_EXTENSION_NAME
                                                              ) where

import qualified OpenXR.VulkanTypes (Format)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import OpenXR.CStruct.Utils (advancePtrBytes)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_VULKAN_SWAPCHAIN_FORMAT_LIST_CREATE_INFO_KHR))
-- | XrVulkanSwapchainFormatListCreateInfoKHR - A list of Vulkan view formats
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrVulkanSwapchainFormatListCreateInfoKHR-extension-notenabled#
--     The @@ extension /must/ be enabled prior to using
--     'VulkanSwapchainFormatListCreateInfoKHR'
--
-- -   #VUID-XrVulkanSwapchainFormatListCreateInfoKHR-type-type# @type@
--     /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_VULKAN_SWAPCHAIN_FORMAT_LIST_CREATE_INFO_KHR'
--
-- -   #VUID-XrVulkanSwapchainFormatListCreateInfoKHR-next-next# @next@
--     /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrVulkanSwapchainFormatListCreateInfoKHR-viewFormats-parameter#
--     If @viewFormatCount@ is not @0@, @viewFormats@ /must/ be a pointer
--     to an array of @viewFormatCount@ valid @VkFormat@ values
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Image.createSwapchain'
data VulkanSwapchainFormatListCreateInfoKHR = VulkanSwapchainFormatListCreateInfoKHR
  { -- | @viewFormats@ is an array of @VkFormat@.
    viewFormats :: Vector OpenXR.VulkanTypes.Format }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VulkanSwapchainFormatListCreateInfoKHR)
#endif
deriving instance Show VulkanSwapchainFormatListCreateInfoKHR

instance ToCStruct VulkanSwapchainFormatListCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VulkanSwapchainFormatListCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VULKAN_SWAPCHAIN_FORMAT_LIST_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (viewFormats)) :: Word32))
    pViewFormats' <- ContT $ allocaBytesAligned @OpenXR.VulkanTypes.Format ((Data.Vector.length (viewFormats)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pViewFormats' `plusPtr` (4 * (i)) :: Ptr OpenXR.VulkanTypes.Format) (e)) (viewFormats)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr OpenXR.VulkanTypes.Format))) (pViewFormats')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VULKAN_SWAPCHAIN_FORMAT_LIST_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct VulkanSwapchainFormatListCreateInfoKHR where
  peekCStruct p = do
    viewFormatCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    viewFormats <- peek @(Ptr OpenXR.VulkanTypes.Format) ((p `plusPtr` 24 :: Ptr (Ptr OpenXR.VulkanTypes.Format)))
    viewFormats' <- generateM (fromIntegral viewFormatCount) (\i -> peek @OpenXR.VulkanTypes.Format ((viewFormats `advancePtrBytes` (4 * (i)) :: Ptr OpenXR.VulkanTypes.Format)))
    pure $ VulkanSwapchainFormatListCreateInfoKHR
             viewFormats'

instance Zero VulkanSwapchainFormatListCreateInfoKHR where
  zero = VulkanSwapchainFormatListCreateInfoKHR
           mempty


type KHR_vulkan_swapchain_format_list_SPEC_VERSION = 3

-- No documentation found for TopLevel "XR_KHR_vulkan_swapchain_format_list_SPEC_VERSION"
pattern KHR_vulkan_swapchain_format_list_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_vulkan_swapchain_format_list_SPEC_VERSION = 3


type KHR_VULKAN_SWAPCHAIN_FORMAT_LIST_EXTENSION_NAME = "XR_KHR_vulkan_swapchain_format_list"

-- No documentation found for TopLevel "XR_KHR_VULKAN_SWAPCHAIN_FORMAT_LIST_EXTENSION_NAME"
pattern KHR_VULKAN_SWAPCHAIN_FORMAT_LIST_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_VULKAN_SWAPCHAIN_FORMAT_LIST_EXTENSION_NAME = "XR_KHR_vulkan_swapchain_format_list"

