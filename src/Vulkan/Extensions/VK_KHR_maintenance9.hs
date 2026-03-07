{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance9 - device extension
--
-- = VK_KHR_maintenance9
--
-- [__Name String__]
--     @VK_KHR_maintenance9@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     585
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance9] @zmike%0A*Here describe the issue or question you have about the VK_KHR_maintenance9 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance9.adoc VK_KHR_maintenance9>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-29
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Shahbaz Youssefi, Google
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Daniel Story, Nintendo
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance9 VK_KHR_maintenance9>
-- adds a collection of minor features, none of which would warrant an
-- entire extension of their own.
--
-- The new features are as follows:
--
-- -   Support VkDevice with no queues. These can be used as effectively an
--     offline compiler to prepopulate pipeline caches, without expensive
--     queue creation or internal memory allocations.
--
-- -   Allow
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdSetEvent2'
--     to not provide a dependency, providing
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent'-style usage using
--     enums from @VK_KHR_synchronization2@
--
-- -   Add a
--     'Vulkan.Core10.Enums.QueryPoolCreateFlagBits.QueryPoolCreateFlagBits'::'Vulkan.Core10.Enums.QueryPoolCreateFlagBits.QUERY_POOL_CREATE_RESET_BIT_KHR'
--     flag to create a query pool with all queries initialized to the
--     reset state.
--
-- -   Allow any integer bit width for specific bit-wise operations.
--
-- -   Add a property to enable sparse support with
--     @VK_EXT_image_2d_view_of_3d@.
--
-- -   Add a property to indicate the implementation will return (0,0,0,0)
--     or (0,0,0,1) to vertex shaders that read unassigned attributes.
--
-- -   The effects of image memory barriers and image layout transitions on
--     3D images created with VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT are
--     scoped to the slices specified by the user-provided
--     VkImageSubresourceRange.
--
-- -   Queue family ownership transfers are no longer required for buffers
--     and linear images, and a new physical device queue family property
--     is exposed to indicate whether queue family ownership transfers are
--     required for optimal images.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance9FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMaintenance9PropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2':
--
--     -   'QueueFamilyOwnershipTransferPropertiesKHR'
--
-- == New Enums
--
-- -   'DefaultVertexAttributeValueKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_9_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_9_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits':
--
--     -   'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_ASYMMETRIC_EVENT_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.QueryPoolCreateFlagBits.QueryPoolCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.QueryPoolCreateFlagBits.QUERY_POOL_CREATE_RESET_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_OWNERSHIP_TRANSFER_PROPERTIES_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-05-29 (Contributors)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_maintenance9 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance9  ( PhysicalDeviceMaintenance9FeaturesKHR(..)
                                              , PhysicalDeviceMaintenance9PropertiesKHR(..)
                                              , QueueFamilyOwnershipTransferPropertiesKHR(..)
                                              , DefaultVertexAttributeValueKHR( DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_ZERO_KHR
                                                                              , DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_ONE_KHR
                                                                              , ..
                                                                              )
                                              , KHR_MAINTENANCE_9_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE_9_SPEC_VERSION
                                              , KHR_MAINTENANCE_9_EXTENSION_NAME
                                              , pattern KHR_MAINTENANCE_9_EXTENSION_NAME
                                              ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_OWNERSHIP_TRANSFER_PROPERTIES_KHR))
-- | VkPhysicalDeviceMaintenance9FeaturesKHR - Structure describing whether
-- the implementation supports maintenance9 functionality
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance9FeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceMaintenance9FeaturesKHR', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance9 VK_KHR_maintenance9>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance9FeaturesKHR = PhysicalDeviceMaintenance9FeaturesKHR
  { -- | #features-maintenance9# @maintenance9@ indicates that the implementation
    -- supports the following:
    --
    -- -   The restriction that certain bitfield SPIR-V instructions only
    --     operate on 32-bit integers is relaxed.
    --
    -- -   The value returned when a vertex shader reads an unbound vertex
    --     attribute is defined by way of the
    --     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-defaultVertexAttributeValue defaultVertexAttributeValue>
    --     property.
    --
    -- -   A new
    --     'Vulkan.Core10.Enums.QueryPoolCreateFlagBits.QueryPoolCreateFlagBits'::'Vulkan.Core10.Enums.QueryPoolCreateFlagBits.QUERY_POOL_CREATE_RESET_BIT_KHR'
    --     flag /can/ be used to initialize all queries in query pool to the
    --     reset state on creation.
    --
    -- -   'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdSetEvent2'
    --     /may/ not provide a dependency other than the event src stage mask.
    --
    -- -   The effects of image memory barriers and image layout transitions on
    --     3D images created with
    --     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
    --     are limited to only those slices specified in
    --     'Vulkan.Core10.ImageView.ImageSubresourceRange'
    --
    -- -   A device /can/ be created with no queues. This can be used for
    --     compiling pipelines or shaders for the purpose of filling pipeline
    --     caches.
    --
    -- -   Queue family ownership transfers are no longer required for buffers
    --     and linear images. For optimally tiled images, a new physical device
    --     query is added to check if resources created with
    --     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE' /can/
    --     automatically transfer ownership between two queue families.
    --
    -- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-image2DViewOf3DSparse image2DViewOf3DSparse>
    --     enables 2D views of 3D sparse images.
    maintenance9 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance9FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance9FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance9FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance9FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (maintenance9))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance9FeaturesKHR where
  peekCStruct p = do
    maintenance9 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance9FeaturesKHR
             (bool32ToBool maintenance9)

instance Storable PhysicalDeviceMaintenance9FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance9FeaturesKHR where
  zero = PhysicalDeviceMaintenance9FeaturesKHR
           zero


-- | VkPhysicalDeviceMaintenance9PropertiesKHR - Structure describing various
-- implementation-defined properties introduced with VK_KHR_maintenance9
--
-- = Members
--
-- -   @sType@ is a 'Vulkan.Core10.Enums.StructureType.StructureType' value
--     identifying this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to a structure extending this
--     structure.
--
-- -   #limits-image2DViewOf3DSparse# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-image2DViewOf3D image2DViewOf3D>
--     feature is enabled, @image2DViewOf3DSparse@ indicates whether the
--     implementation supports binding a slice of a sparse 3D image to a 2D
--     image view.
--
-- -   #limits-defaultVertexAttributeValue# @defaultVertexAttributeValue@
--     is a 'DefaultVertexAttributeValueKHR' that indicates what value the
--     implementation will return when the vertex shader reads unbound
--     vertex attributes. Unbound attributes are those that have no
--     corresponding
--     'Vulkan.Core10.GraphicsPipeline.VertexInputAttributeDescription'::@location@
--     defined in the bound graphics pipeline or no corresponding
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@location@
--     set by the most recent call to
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     when the state is dynamic .
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance9PropertiesKHR' structure is included
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance9 VK_KHR_maintenance9>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'DefaultVertexAttributeValueKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance9PropertiesKHR = PhysicalDeviceMaintenance9PropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceMaintenance9PropertiesKHR" "image2DViewOf3DSparse"
    image2DViewOf3DSparse :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceMaintenance9PropertiesKHR" "defaultVertexAttributeValue"
    defaultVertexAttributeValue :: DefaultVertexAttributeValueKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance9PropertiesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance9PropertiesKHR

instance ToCStruct PhysicalDeviceMaintenance9PropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance9PropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (image2DViewOf3DSparse))
    poke ((p `plusPtr` 20 :: Ptr DefaultVertexAttributeValueKHR)) (defaultVertexAttributeValue)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr DefaultVertexAttributeValueKHR)) (zero)
    f

instance FromCStruct PhysicalDeviceMaintenance9PropertiesKHR where
  peekCStruct p = do
    image2DViewOf3DSparse <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    defaultVertexAttributeValue <- peek @DefaultVertexAttributeValueKHR ((p `plusPtr` 20 :: Ptr DefaultVertexAttributeValueKHR))
    pure $ PhysicalDeviceMaintenance9PropertiesKHR
             (bool32ToBool image2DViewOf3DSparse) defaultVertexAttributeValue

instance Storable PhysicalDeviceMaintenance9PropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance9PropertiesKHR where
  zero = PhysicalDeviceMaintenance9PropertiesKHR
           zero
           zero


-- | VkQueueFamilyOwnershipTransferPropertiesKHR - Structure describing queue
-- family ownership transfer properties
--
-- = Description
--
-- If this structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2',
-- then it is filled with the queue family ownership properties for the
-- specified queue family.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance9 VK_KHR_maintenance9>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data QueueFamilyOwnershipTransferPropertiesKHR = QueueFamilyOwnershipTransferPropertiesKHR
  { -- | @optimalImageTransferToQueueFamilies@ is a bitmask of queue family
    -- indices that indicates which queue families belonging to the same
    -- logical device support implicitly acquiring optimal image resources
    -- owned by this queue family, without the resources\' contents becoming
    -- undefined.
    optimalImageTransferToQueueFamilies :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyOwnershipTransferPropertiesKHR)
#endif
deriving instance Show QueueFamilyOwnershipTransferPropertiesKHR

instance ToCStruct QueueFamilyOwnershipTransferPropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyOwnershipTransferPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_OWNERSHIP_TRANSFER_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (optimalImageTransferToQueueFamilies)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_OWNERSHIP_TRANSFER_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct QueueFamilyOwnershipTransferPropertiesKHR where
  peekCStruct p = do
    optimalImageTransferToQueueFamilies <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ QueueFamilyOwnershipTransferPropertiesKHR
             optimalImageTransferToQueueFamilies

instance Storable QueueFamilyOwnershipTransferPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueueFamilyOwnershipTransferPropertiesKHR where
  zero = QueueFamilyOwnershipTransferPropertiesKHR
           zero


-- | VkDefaultVertexAttributeValueKHR - Values returned for unbound vertex
-- attributes
--
-- = Description
--
-- -   'DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_ZERO_KHR' - the value
--     read for an unbound vertex attribute is (0,0,0,0).
--
-- -   'DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_ONE_KHR' - the value
--     read for an unbound vertex attribute is (0,0,0,1).
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance9 VK_KHR_maintenance9>,
-- 'PhysicalDeviceMaintenance9PropertiesKHR'
newtype DefaultVertexAttributeValueKHR = DefaultVertexAttributeValueKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDefaultVertexAttributeValueKHR" "VK_DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_ZERO_KHR"
pattern DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_ZERO_KHR = DefaultVertexAttributeValueKHR 0

-- No documentation found for Nested "VkDefaultVertexAttributeValueKHR" "VK_DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_ONE_KHR"
pattern DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_ONE_KHR = DefaultVertexAttributeValueKHR 1

{-# COMPLETE
  DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_ZERO_KHR
  , DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_ONE_KHR ::
    DefaultVertexAttributeValueKHR
  #-}

conNameDefaultVertexAttributeValueKHR :: String
conNameDefaultVertexAttributeValueKHR = "DefaultVertexAttributeValueKHR"

enumPrefixDefaultVertexAttributeValueKHR :: String
enumPrefixDefaultVertexAttributeValueKHR = "DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_"

showTableDefaultVertexAttributeValueKHR :: [(DefaultVertexAttributeValueKHR, String)]
showTableDefaultVertexAttributeValueKHR =
  [
    ( DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_ZERO_KHR
    , "ZERO_KHR"
    )
  ,
    ( DEFAULT_VERTEX_ATTRIBUTE_VALUE_ZERO_ZERO_ZERO_ONE_KHR
    , "ONE_KHR"
    )
  ]

instance Show DefaultVertexAttributeValueKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixDefaultVertexAttributeValueKHR
      showTableDefaultVertexAttributeValueKHR
      conNameDefaultVertexAttributeValueKHR
      (\(DefaultVertexAttributeValueKHR x) -> x)
      (showsPrec 11)

instance Read DefaultVertexAttributeValueKHR where
  readPrec =
    enumReadPrec
      enumPrefixDefaultVertexAttributeValueKHR
      showTableDefaultVertexAttributeValueKHR
      conNameDefaultVertexAttributeValueKHR
      DefaultVertexAttributeValueKHR

type KHR_MAINTENANCE_9_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_9_SPEC_VERSION"
pattern KHR_MAINTENANCE_9_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE_9_SPEC_VERSION = 1


type KHR_MAINTENANCE_9_EXTENSION_NAME = "VK_KHR_maintenance9"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_9_EXTENSION_NAME"
pattern KHR_MAINTENANCE_9_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE_9_EXTENSION_NAME = "VK_KHR_maintenance9"

