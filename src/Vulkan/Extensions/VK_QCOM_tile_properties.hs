{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_tile_properties - device extension
--
-- == VK_QCOM_tile_properties
--
-- [__Name String__]
--     @VK_QCOM_tile_properties@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     485
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_tile_properties] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_tile_properties extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_tile_properties.adoc VK_QCOM_tile_properties>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-07-11
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_EXT_subpass_merge_feedback@
--
-- [__Contributors__]
--
--     -   Jonathan Wicks, Qualcomm Technologies, Inc.
--
--     -   Jonathan Tinkham, Qualcomm Technologies, Inc.
--
--     -   Arpit Agarwal, Qualcomm Technologies, Inc.
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension allows an application to query the tile properties. This
-- extension supports both renderpasses and dynamic rendering.
--
-- == New Commands
--
-- -   'getDynamicRenderingTilePropertiesQCOM'
--
-- -   'getFramebufferTilePropertiesQCOM'
--
-- == New Structures
--
-- -   'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingInfoKHR'
--
-- -   'TilePropertiesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTilePropertiesFeaturesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_TILE_PROPERTIES_EXTENSION_NAME'
--
-- -   'QCOM_TILE_PROPERTIES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_PROPERTIES_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TILE_PROPERTIES_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2022-07-11 (Arpit Agarwal)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceTilePropertiesFeaturesQCOM',
-- 'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingInfoKHR',
-- 'TilePropertiesQCOM', 'getDynamicRenderingTilePropertiesQCOM',
-- 'getFramebufferTilePropertiesQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_tile_properties Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_tile_properties  ( getFramebufferTilePropertiesQCOM
                                                  , getDynamicRenderingTilePropertiesQCOM
                                                  , PhysicalDeviceTilePropertiesFeaturesQCOM(..)
                                                  , TilePropertiesQCOM(..)
                                                  , QCOM_TILE_PROPERTIES_SPEC_VERSION
                                                  , pattern QCOM_TILE_PROPERTIES_SPEC_VERSION
                                                  , QCOM_TILE_PROPERTIES_EXTENSION_NAME
                                                  , pattern QCOM_TILE_PROPERTIES_EXTENSION_NAME
                                                  , RenderingInfoKHR
                                                  ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.IO (throwIO)
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetDynamicRenderingTilePropertiesQCOM))
import Vulkan.Dynamic (DeviceCmds(pVkGetFramebufferTilePropertiesQCOM))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core10.Handles (Framebuffer)
import Vulkan.Core10.Handles (Framebuffer(..))
import Vulkan.Core10.FundamentalTypes (Offset2D)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (RenderingInfo)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_PROPERTIES_FEATURES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TILE_PROPERTIES_QCOM))
import Vulkan.Extensions.VK_KHR_dynamic_rendering (RenderingInfoKHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFramebufferTilePropertiesQCOM
  :: FunPtr (Ptr Device_T -> Framebuffer -> Ptr Word32 -> Ptr TilePropertiesQCOM -> IO Result) -> Ptr Device_T -> Framebuffer -> Ptr Word32 -> Ptr TilePropertiesQCOM -> IO Result

-- | vkGetFramebufferTilePropertiesQCOM - Get tile properties from the
-- attachments in framebuffer
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of tile properties available
-- is returned in @pPropertiesCount@. Otherwise, @pPropertiesCount@ /must/
-- point to a variable set by the user to the number of elements in the
-- @pProperties@ array, and on return the variable is overwritten with the
-- number of properties actually written to @pProperties@. If
-- @pPropertiesCount@ is less than the number of tile properties available,
-- at most @pPropertiesCount@ structures will be written, and
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available properties were returned.
--
-- The number of tile properties available is determined by the number of
-- merged subpasses, and each tile property is associated with a merged
-- subpass. There will be at most as many properties as there are subpasses
-- within the render pass. To obtain the tile properties for a given merged
-- subpass, the @pProperties@ array can be indexed using the
-- @postMergeIndex@ value provided in
-- 'Vulkan.Extensions.VK_EXT_subpass_merge_feedback.RenderPassSubpassFeedbackInfoEXT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetFramebufferTilePropertiesQCOM-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetFramebufferTilePropertiesQCOM-framebuffer-parameter#
--     @framebuffer@ /must/ be a valid 'Vulkan.Core10.Handles.Framebuffer'
--     handle
--
-- -   #VUID-vkGetFramebufferTilePropertiesQCOM-pPropertiesCount-parameter#
--     @pPropertiesCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetFramebufferTilePropertiesQCOM-pProperties-parameter# If
--     the value referenced by @pPropertiesCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertiesCount@ 'TilePropertiesQCOM' structures
--
-- -   #VUID-vkGetFramebufferTilePropertiesQCOM-framebuffer-parent#
--     @framebuffer@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Framebuffer',
-- 'TilePropertiesQCOM'
getFramebufferTilePropertiesQCOM :: forall io
                                  . (MonadIO io)
                                 => -- | @device@ is a logical device associated with the framebuffer.
                                    Device
                                 -> -- | @framebuffer@ is a handle of the framebuffer to query.
                                    Framebuffer
                                 -> io (Result, ("properties" ::: Vector TilePropertiesQCOM))
getFramebufferTilePropertiesQCOM device framebuffer = liftIO . evalContT $ do
  let vkGetFramebufferTilePropertiesQCOMPtr = pVkGetFramebufferTilePropertiesQCOM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetFramebufferTilePropertiesQCOMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetFramebufferTilePropertiesQCOM is null" Nothing Nothing
  let vkGetFramebufferTilePropertiesQCOM' = mkVkGetFramebufferTilePropertiesQCOM vkGetFramebufferTilePropertiesQCOMPtr
  let device' = deviceHandle (device)
  pPPropertiesCount <- ContT $ bracket (callocBytes @Word32 4) free
  _ <- lift $ traceAroundEvent "vkGetFramebufferTilePropertiesQCOM" (vkGetFramebufferTilePropertiesQCOM'
                                                                       device'
                                                                       (framebuffer)
                                                                       (pPPropertiesCount)
                                                                       (nullPtr))
  pPropertiesCount <- lift $ peek @Word32 pPPropertiesCount
  pPProperties <- ContT $ bracket (callocBytes @TilePropertiesQCOM ((fromIntegral (pPropertiesCount)) * 48)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 48) :: Ptr TilePropertiesQCOM) . ($ ())) [0..(fromIntegral (pPropertiesCount)) - 1]
  r <- lift $ traceAroundEvent "vkGetFramebufferTilePropertiesQCOM" (vkGetFramebufferTilePropertiesQCOM'
                                                                       device'
                                                                       (framebuffer)
                                                                       (pPPropertiesCount)
                                                                       ((pPProperties)))
  pPropertiesCount' <- lift $ peek @Word32 pPPropertiesCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertiesCount')) (\i -> peekCStruct @TilePropertiesQCOM (((pPProperties) `advancePtrBytes` (48 * (i)) :: Ptr TilePropertiesQCOM)))
  pure $ (r, pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDynamicRenderingTilePropertiesQCOM
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct RenderingInfo) -> Ptr TilePropertiesQCOM -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct RenderingInfo) -> Ptr TilePropertiesQCOM -> IO Result

-- | vkGetDynamicRenderingTilePropertiesQCOM - Get the properties when using
-- dynamic rendering
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo',
-- 'TilePropertiesQCOM'
getDynamicRenderingTilePropertiesQCOM :: forall a io
                                       . ( Extendss RenderingInfo a
                                         , PokeChain a
                                         , MonadIO io )
                                      => -- | @device@ is a logical device associated with the render pass.
                                         --
                                         -- #VUID-vkGetDynamicRenderingTilePropertiesQCOM-device-parameter# @device@
                                         -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                         Device
                                      -> -- | @pRenderingInfo@ is a pointer to the
                                         -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'
                                         -- structure specifying details of the render pass instance in dynamic
                                         -- rendering.
                                         --
                                         -- #VUID-vkGetDynamicRenderingTilePropertiesQCOM-pRenderingInfo-parameter#
                                         -- @pRenderingInfo@ /must/ be a valid pointer to a valid
                                         -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'
                                         -- structure
                                         (RenderingInfo a)
                                      -> io (TilePropertiesQCOM)
getDynamicRenderingTilePropertiesQCOM device
                                        renderingInfo = liftIO . evalContT $ do
  let vkGetDynamicRenderingTilePropertiesQCOMPtr = pVkGetDynamicRenderingTilePropertiesQCOM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDynamicRenderingTilePropertiesQCOMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDynamicRenderingTilePropertiesQCOM is null" Nothing Nothing
  let vkGetDynamicRenderingTilePropertiesQCOM' = mkVkGetDynamicRenderingTilePropertiesQCOM vkGetDynamicRenderingTilePropertiesQCOMPtr
  pRenderingInfo <- ContT $ withCStruct (renderingInfo)
  pPProperties <- ContT (withZeroCStruct @TilePropertiesQCOM)
  _ <- lift $ traceAroundEvent "vkGetDynamicRenderingTilePropertiesQCOM" (vkGetDynamicRenderingTilePropertiesQCOM'
                                                                            (deviceHandle (device))
                                                                            (forgetExtensions pRenderingInfo)
                                                                            (pPProperties))
  pProperties <- lift $ peekCStruct @TilePropertiesQCOM pPProperties
  pure $ (pProperties)


-- | VkPhysicalDeviceTilePropertiesFeaturesQCOM - Structure describing tile
-- properties features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceTilePropertiesFeaturesQCOM' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceTilePropertiesFeaturesQCOM' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTilePropertiesFeaturesQCOM = PhysicalDeviceTilePropertiesFeaturesQCOM
  { -- | #features-tileProperties# @tileProperties@ indicates that the
    -- implementation supports queries for returning tile properties.
    tileProperties :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTilePropertiesFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceTilePropertiesFeaturesQCOM

instance ToCStruct PhysicalDeviceTilePropertiesFeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTilePropertiesFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_PROPERTIES_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (tileProperties))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_PROPERTIES_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTilePropertiesFeaturesQCOM where
  peekCStruct p = do
    tileProperties <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceTilePropertiesFeaturesQCOM
             (bool32ToBool tileProperties)

instance Storable PhysicalDeviceTilePropertiesFeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTilePropertiesFeaturesQCOM where
  zero = PhysicalDeviceTilePropertiesFeaturesQCOM
           zero


-- | VkTilePropertiesQCOM - Structure holding available tile properties
--
-- = Description
--
-- All tiles will be tightly packed around the first tile, with edges being
-- multiples of tile width and\/or height from the origin.
--
-- Note
--
-- Reported value for @apronSize@ will be zero and its functionality will
-- be described in a future extension.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>,
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.FundamentalTypes.Offset2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDynamicRenderingTilePropertiesQCOM',
-- 'getFramebufferTilePropertiesQCOM'
data TilePropertiesQCOM = TilePropertiesQCOM
  { -- | @tileSize@ is the dimensions of a tile, with width and height describing
    -- the width and height of a tile in pixels, and depth corresponding to the
    -- number of slices the tile spans.
    tileSize :: Extent3D
  , -- | @apronSize@ is the dimension of the apron.
    apronSize :: Extent2D
  , -- | @origin@ is the top-left corner of the first tile in attachment space.
    origin :: Offset2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TilePropertiesQCOM)
#endif
deriving instance Show TilePropertiesQCOM

instance ToCStruct TilePropertiesQCOM where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TilePropertiesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TILE_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent3D)) (tileSize)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (apronSize)
    poke ((p `plusPtr` 36 :: Ptr Offset2D)) (origin)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TILE_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent3D)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Offset2D)) (zero)
    f

instance FromCStruct TilePropertiesQCOM where
  peekCStruct p = do
    tileSize <- peekCStruct @Extent3D ((p `plusPtr` 16 :: Ptr Extent3D))
    apronSize <- peekCStruct @Extent2D ((p `plusPtr` 28 :: Ptr Extent2D))
    origin <- peekCStruct @Offset2D ((p `plusPtr` 36 :: Ptr Offset2D))
    pure $ TilePropertiesQCOM
             tileSize apronSize origin

instance Storable TilePropertiesQCOM where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TilePropertiesQCOM where
  zero = TilePropertiesQCOM
           zero
           zero
           zero


type QCOM_TILE_PROPERTIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_TILE_PROPERTIES_SPEC_VERSION"
pattern QCOM_TILE_PROPERTIES_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_TILE_PROPERTIES_SPEC_VERSION = 1


type QCOM_TILE_PROPERTIES_EXTENSION_NAME = "VK_QCOM_tile_properties"

-- No documentation found for TopLevel "VK_QCOM_TILE_PROPERTIES_EXTENSION_NAME"
pattern QCOM_TILE_PROPERTIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_TILE_PROPERTIES_EXTENSION_NAME = "VK_QCOM_tile_properties"

