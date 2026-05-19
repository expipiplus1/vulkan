{-# language CPP #-}
-- | = Name
--
-- VK_SEC_throttle_hint - device extension
--
-- = VK_SEC_throttle_hint
--
-- [__Name String__]
--     @VK_SEC_throttle_hint@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     675
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Ralph Potter <<data:image/png;base64, GitLab>>r_potter
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-04-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jonghyuk Eun, Samsung
--
--     -   Minyoung Son, Samsung
--
--     -   Jihyoung Hong, Samsung
--
--     -   Pavan Lanka, Samsung
--
--     -   Ralph Potter, Samsung
--
-- == Description
--
-- This extension is intended to convey throttle hints to the device, but
-- does not specify the throttling behavior or any minimum guarantees. This
-- extension is intended to communicate information from layered API
-- implementations such as ANGLE to internal proprietary system schedulers.
--
-- It has no behavioral implications beyond enabling more intelligent
-- behavior from the system scheduler.
--
-- Application developers should avoid using this extension. It is
-- documented solely for the benefit of tools and layer developers, who may
-- need to manipulate @pNext@ chains that include these structures.
--
-- There is currently no specification language written for this extension.
-- The links to APIs defined by the extension are to stubs that only
-- include generated content such as API declarations and implicit valid
-- usage statements.
--
-- This extension is only intended for use in specific embedded
-- environments with known implementation details, and is therefore
-- undocumented.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceThrottleHintFeaturesSEC'
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo':
--
--     -   'ThrottleHintSubmitInfoSEC'
--
-- == New Enums
--
-- -   'ThrottleHintTypeSEC'
--
-- == New Enum Constants
--
-- -   'SEC_THROTTLE_HINT_EXTENSION_NAME'
--
-- -   'SEC_THROTTLE_HINT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_THROTTLE_HINT_FEATURES_SEC'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_THROTTLE_HINT_SUBMIT_INFO_SEC'
--
-- == Stub API References
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_SEC_throttle_hint
-- > typedef struct VkPhysicalDeviceThrottleHintFeaturesSEC {
-- >     VkStructureType    sType;
-- >     void*              pNext;
-- >     VkBool32           throttleHint;
-- > } VkPhysicalDeviceThrottleHintFeaturesSEC;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceThrottleHintFeaturesSEC-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_THROTTLE_HINT_FEATURES_SEC'
--
-- === Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_SEC_throttle_hint
-- > typedef struct VkThrottleHintSubmitInfoSEC {
-- >     VkStructureType          sType;
-- >     const void*              pNext;
-- >     VkThrottleHintTypeSEC    throttleHint;
-- > } VkThrottleHintSubmitInfoSEC;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkThrottleHintSubmitInfoSEC-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_THROTTLE_HINT_SUBMIT_INFO_SEC'
--
-- -   #VUID-VkThrottleHintSubmitInfoSEC-throttleHint-parameter#
--     @throttleHint@ /must/ be a valid 'ThrottleHintTypeSEC' value
--
-- === Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Core10.Queue.SubmitInfo'
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_SEC_throttle_hint
-- > typedef enum VkThrottleHintTypeSEC {
-- >     VK_THROTTLE_HINT_TYPE_DEFAULT_SEC = 0,
-- >     VK_THROTTLE_HINT_TYPE_LOW_SEC = 1,
-- >     VK_THROTTLE_HINT_TYPE_HIGH_SEC = 2,
-- > } VkThrottleHintTypeSEC;
--
-- == Version History
--
-- -   Revision 1, 2026-04-09 (Ralph Potter)
--
--     -   Initial specification
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_SEC_throttle_hint Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_SEC_throttle_hint  ( PhysicalDeviceThrottleHintFeaturesSEC(..)
                                               , ThrottleHintSubmitInfoSEC(..)
                                               , ThrottleHintTypeSEC( THROTTLE_HINT_TYPE_DEFAULT_SEC
                                                                    , THROTTLE_HINT_TYPE_LOW_SEC
                                                                    , THROTTLE_HINT_TYPE_HIGH_SEC
                                                                    , ..
                                                                    )
                                               , SEC_THROTTLE_HINT_SPEC_VERSION
                                               , pattern SEC_THROTTLE_HINT_SPEC_VERSION
                                               , SEC_THROTTLE_HINT_EXTENSION_NAME
                                               , pattern SEC_THROTTLE_HINT_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_THROTTLE_HINT_FEATURES_SEC))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_THROTTLE_HINT_SUBMIT_INFO_SEC))
-- | VkPhysicalDeviceThrottleHintFeaturesSEC - Stub description of
-- VkPhysicalDeviceThrottleHintFeaturesSEC
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_SEC_throttle_hint VK_SEC_throttle_hint>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceThrottleHintFeaturesSEC = PhysicalDeviceThrottleHintFeaturesSEC
  { -- No documentation found for Nested "VkPhysicalDeviceThrottleHintFeaturesSEC" "throttleHint"
    throttleHint :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceThrottleHintFeaturesSEC)
#endif
deriving instance Show PhysicalDeviceThrottleHintFeaturesSEC

instance ToCStruct PhysicalDeviceThrottleHintFeaturesSEC where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceThrottleHintFeaturesSEC{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_THROTTLE_HINT_FEATURES_SEC)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (throttleHint))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_THROTTLE_HINT_FEATURES_SEC)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceThrottleHintFeaturesSEC where
  peekCStruct p = do
    throttleHint <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceThrottleHintFeaturesSEC
             (bool32ToBool throttleHint)

instance Storable PhysicalDeviceThrottleHintFeaturesSEC where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceThrottleHintFeaturesSEC where
  zero = PhysicalDeviceThrottleHintFeaturesSEC
           zero


-- | VkThrottleHintSubmitInfoSEC - Stub description of
-- VkThrottleHintSubmitInfoSEC
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Core10.Queue.SubmitInfo'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_SEC_throttle_hint VK_SEC_throttle_hint>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'ThrottleHintTypeSEC'
data ThrottleHintSubmitInfoSEC = ThrottleHintSubmitInfoSEC
  { -- | #VUID-VkThrottleHintSubmitInfoSEC-throttleHint-parameter# @throttleHint@
    -- /must/ be a valid 'ThrottleHintTypeSEC' value
    throttleHint :: ThrottleHintTypeSEC }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ThrottleHintSubmitInfoSEC)
#endif
deriving instance Show ThrottleHintSubmitInfoSEC

instance ToCStruct ThrottleHintSubmitInfoSEC where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ThrottleHintSubmitInfoSEC{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_THROTTLE_HINT_SUBMIT_INFO_SEC)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ThrottleHintTypeSEC)) (throttleHint)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_THROTTLE_HINT_SUBMIT_INFO_SEC)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ThrottleHintTypeSEC)) (zero)
    f

instance FromCStruct ThrottleHintSubmitInfoSEC where
  peekCStruct p = do
    throttleHint <- peek @ThrottleHintTypeSEC ((p `plusPtr` 16 :: Ptr ThrottleHintTypeSEC))
    pure $ ThrottleHintSubmitInfoSEC
             throttleHint

instance Storable ThrottleHintSubmitInfoSEC where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ThrottleHintSubmitInfoSEC where
  zero = ThrottleHintSubmitInfoSEC
           zero


-- | VkThrottleHintTypeSEC - Stub description of VkThrottleHintTypeSEC
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_SEC_throttle_hint VK_SEC_throttle_hint>,
-- 'ThrottleHintSubmitInfoSEC'
newtype ThrottleHintTypeSEC = ThrottleHintTypeSEC Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkThrottleHintTypeSEC" "VK_THROTTLE_HINT_TYPE_DEFAULT_SEC"
pattern THROTTLE_HINT_TYPE_DEFAULT_SEC = ThrottleHintTypeSEC 0

-- No documentation found for Nested "VkThrottleHintTypeSEC" "VK_THROTTLE_HINT_TYPE_LOW_SEC"
pattern THROTTLE_HINT_TYPE_LOW_SEC = ThrottleHintTypeSEC 1

-- No documentation found for Nested "VkThrottleHintTypeSEC" "VK_THROTTLE_HINT_TYPE_HIGH_SEC"
pattern THROTTLE_HINT_TYPE_HIGH_SEC = ThrottleHintTypeSEC 2

{-# COMPLETE
  THROTTLE_HINT_TYPE_DEFAULT_SEC
  , THROTTLE_HINT_TYPE_LOW_SEC
  , THROTTLE_HINT_TYPE_HIGH_SEC ::
    ThrottleHintTypeSEC
  #-}

conNameThrottleHintTypeSEC :: String
conNameThrottleHintTypeSEC = "ThrottleHintTypeSEC"

enumPrefixThrottleHintTypeSEC :: String
enumPrefixThrottleHintTypeSEC = "THROTTLE_HINT_TYPE_"

showTableThrottleHintTypeSEC :: [(ThrottleHintTypeSEC, String)]
showTableThrottleHintTypeSEC =
  [ (THROTTLE_HINT_TYPE_DEFAULT_SEC, "DEFAULT_SEC")
  , (THROTTLE_HINT_TYPE_LOW_SEC, "LOW_SEC")
  , (THROTTLE_HINT_TYPE_HIGH_SEC, "HIGH_SEC")
  ]

instance Show ThrottleHintTypeSEC where
  showsPrec =
    enumShowsPrec
      enumPrefixThrottleHintTypeSEC
      showTableThrottleHintTypeSEC
      conNameThrottleHintTypeSEC
      (\(ThrottleHintTypeSEC x) -> x)
      (showsPrec 11)

instance Read ThrottleHintTypeSEC where
  readPrec =
    enumReadPrec
      enumPrefixThrottleHintTypeSEC
      showTableThrottleHintTypeSEC
      conNameThrottleHintTypeSEC
      ThrottleHintTypeSEC

type SEC_THROTTLE_HINT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_SEC_THROTTLE_HINT_SPEC_VERSION"
pattern SEC_THROTTLE_HINT_SPEC_VERSION :: forall a . Integral a => a
pattern SEC_THROTTLE_HINT_SPEC_VERSION = 1


type SEC_THROTTLE_HINT_EXTENSION_NAME = "VK_SEC_throttle_hint"

-- No documentation found for TopLevel "VK_SEC_THROTTLE_HINT_EXTENSION_NAME"
pattern SEC_THROTTLE_HINT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern SEC_THROTTLE_HINT_EXTENSION_NAME = "VK_SEC_throttle_hint"

