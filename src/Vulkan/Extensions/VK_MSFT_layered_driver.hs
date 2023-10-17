{-# language CPP #-}
-- | = Name
--
-- VK_MSFT_layered_driver - device extension
--
-- == VK_MSFT_layered_driver
--
-- [__Name String__]
--     @VK_MSFT_layered_driver@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     531
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Jesse Natalie
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_MSFT_layered_driver] @jenatali%0A*Here describe the issue or question you have about the VK_MSFT_layered_driver extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_MSFT_layered_driver.adoc VK_MSFT_layered_driver>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-06-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jesse Natalie, Microsoft
--
-- == Description
--
-- This extension adds new physical device properties to allow applications
-- and the Vulkan ICD loader to understand when a physical device is
-- implemented as a layered driver on top of another underlying API.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceLayeredDriverPropertiesMSFT'
--
-- == New Enums
--
-- -   'LayeredDriverUnderlyingApiMSFT'
--
-- == New Enum Constants
--
-- -   'MSFT_LAYERED_DRIVER_EXTENSION_NAME'
--
-- -   'MSFT_LAYERED_DRIVER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_DRIVER_PROPERTIES_MSFT'
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-06-21 (Jesse Natalie)
--
--     -   Initial revision
--
-- == See Also
--
-- 'LayeredDriverUnderlyingApiMSFT',
-- 'PhysicalDeviceLayeredDriverPropertiesMSFT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_MSFT_layered_driver Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_MSFT_layered_driver  ( PhysicalDeviceLayeredDriverPropertiesMSFT(..)
                                                 , LayeredDriverUnderlyingApiMSFT( LAYERED_DRIVER_UNDERLYING_API_NONE_MSFT
                                                                                 , LAYERED_DRIVER_UNDERLYING_API_D3D12_MSFT
                                                                                 , ..
                                                                                 )
                                                 , MSFT_LAYERED_DRIVER_SPEC_VERSION
                                                 , pattern MSFT_LAYERED_DRIVER_SPEC_VERSION
                                                 , MSFT_LAYERED_DRIVER_EXTENSION_NAME
                                                 , pattern MSFT_LAYERED_DRIVER_EXTENSION_NAME
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
import Data.Kind (Type)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_DRIVER_PROPERTIES_MSFT))
-- | VkPhysicalDeviceLayeredDriverPropertiesMSFT - Structure containing
-- information about driver layering for a physical device
--
-- = Description
--
-- These are properties of the driver layering information of a physical
-- device.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_MSFT_layered_driver VK_MSFT_layered_driver>,
-- 'LayeredDriverUnderlyingApiMSFT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLayeredDriverPropertiesMSFT = PhysicalDeviceLayeredDriverPropertiesMSFT
  { -- | @underlyingAPI@ is a 'LayeredDriverUnderlyingApiMSFT' value indicating
    -- which underlying API is used to implement the layered driver, or
    -- 'LAYERED_DRIVER_UNDERLYING_API_NONE_MSFT' if the driver is not layered.
    underlyingAPI :: LayeredDriverUnderlyingApiMSFT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLayeredDriverPropertiesMSFT)
#endif
deriving instance Show PhysicalDeviceLayeredDriverPropertiesMSFT

instance ToCStruct PhysicalDeviceLayeredDriverPropertiesMSFT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLayeredDriverPropertiesMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_DRIVER_PROPERTIES_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr LayeredDriverUnderlyingApiMSFT)) (underlyingAPI)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_DRIVER_PROPERTIES_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr LayeredDriverUnderlyingApiMSFT)) (zero)
    f

instance FromCStruct PhysicalDeviceLayeredDriverPropertiesMSFT where
  peekCStruct p = do
    underlyingAPI <- peek @LayeredDriverUnderlyingApiMSFT ((p `plusPtr` 16 :: Ptr LayeredDriverUnderlyingApiMSFT))
    pure $ PhysicalDeviceLayeredDriverPropertiesMSFT
             underlyingAPI

instance Storable PhysicalDeviceLayeredDriverPropertiesMSFT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceLayeredDriverPropertiesMSFT where
  zero = PhysicalDeviceLayeredDriverPropertiesMSFT
           zero


-- | VkLayeredDriverUnderlyingApiMSFT - Layered driver underlying APIs
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_MSFT_layered_driver VK_MSFT_layered_driver>,
-- 'PhysicalDeviceLayeredDriverPropertiesMSFT'
newtype LayeredDriverUnderlyingApiMSFT = LayeredDriverUnderlyingApiMSFT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkLayeredDriverUnderlyingApiMSFT" "VK_LAYERED_DRIVER_UNDERLYING_API_NONE_MSFT"
pattern LAYERED_DRIVER_UNDERLYING_API_NONE_MSFT = LayeredDriverUnderlyingApiMSFT 0

-- No documentation found for Nested "VkLayeredDriverUnderlyingApiMSFT" "VK_LAYERED_DRIVER_UNDERLYING_API_D3D12_MSFT"
pattern LAYERED_DRIVER_UNDERLYING_API_D3D12_MSFT = LayeredDriverUnderlyingApiMSFT 1

{-# COMPLETE
  LAYERED_DRIVER_UNDERLYING_API_NONE_MSFT
  , LAYERED_DRIVER_UNDERLYING_API_D3D12_MSFT ::
    LayeredDriverUnderlyingApiMSFT
  #-}

conNameLayeredDriverUnderlyingApiMSFT :: String
conNameLayeredDriverUnderlyingApiMSFT = "LayeredDriverUnderlyingApiMSFT"

enumPrefixLayeredDriverUnderlyingApiMSFT :: String
enumPrefixLayeredDriverUnderlyingApiMSFT = "LAYERED_DRIVER_UNDERLYING_API_"

showTableLayeredDriverUnderlyingApiMSFT :: [(LayeredDriverUnderlyingApiMSFT, String)]
showTableLayeredDriverUnderlyingApiMSFT =
  [
    ( LAYERED_DRIVER_UNDERLYING_API_NONE_MSFT
    , "NONE_MSFT"
    )
  ,
    ( LAYERED_DRIVER_UNDERLYING_API_D3D12_MSFT
    , "D3D12_MSFT"
    )
  ]

instance Show LayeredDriverUnderlyingApiMSFT where
  showsPrec =
    enumShowsPrec
      enumPrefixLayeredDriverUnderlyingApiMSFT
      showTableLayeredDriverUnderlyingApiMSFT
      conNameLayeredDriverUnderlyingApiMSFT
      (\(LayeredDriverUnderlyingApiMSFT x) -> x)
      (showsPrec 11)

instance Read LayeredDriverUnderlyingApiMSFT where
  readPrec =
    enumReadPrec
      enumPrefixLayeredDriverUnderlyingApiMSFT
      showTableLayeredDriverUnderlyingApiMSFT
      conNameLayeredDriverUnderlyingApiMSFT
      LayeredDriverUnderlyingApiMSFT

type MSFT_LAYERED_DRIVER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_MSFT_LAYERED_DRIVER_SPEC_VERSION"
pattern MSFT_LAYERED_DRIVER_SPEC_VERSION :: forall a . Integral a => a
pattern MSFT_LAYERED_DRIVER_SPEC_VERSION = 1


type MSFT_LAYERED_DRIVER_EXTENSION_NAME = "VK_MSFT_layered_driver"

-- No documentation found for TopLevel "VK_MSFT_LAYERED_DRIVER_EXTENSION_NAME"
pattern MSFT_LAYERED_DRIVER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MSFT_LAYERED_DRIVER_EXTENSION_NAME = "VK_MSFT_layered_driver"

