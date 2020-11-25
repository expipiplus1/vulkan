{-# language CPP #-}
-- No documentation found for Chapter "VK_AMD_memory_overallocation_behavior"
module Vulkan.Extensions.VK_AMD_memory_overallocation_behavior  ( DeviceMemoryOverallocationCreateInfoAMD(..)
                                                                , MemoryOverallocationBehaviorAMD( MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD
                                                                                                 , MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD
                                                                                                 , MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD
                                                                                                 , ..
                                                                                                 )
                                                                , AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION
                                                                , pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION
                                                                , AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME
                                                                , pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME
                                                                ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD))

-- No documentation found for TopLevel "VkDeviceMemoryOverallocationCreateInfoAMD"
data DeviceMemoryOverallocationCreateInfoAMD = DeviceMemoryOverallocationCreateInfoAMD
  { -- No documentation found for Nested "VkDeviceMemoryOverallocationCreateInfoAMD" "overallocationBehavior"
    overallocationBehavior :: MemoryOverallocationBehaviorAMD }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceMemoryOverallocationCreateInfoAMD)
#endif
deriving instance Show DeviceMemoryOverallocationCreateInfoAMD

instance ToCStruct DeviceMemoryOverallocationCreateInfoAMD where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceMemoryOverallocationCreateInfoAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MemoryOverallocationBehaviorAMD)) (overallocationBehavior)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MemoryOverallocationBehaviorAMD)) (zero)
    f

instance FromCStruct DeviceMemoryOverallocationCreateInfoAMD where
  peekCStruct p = do
    overallocationBehavior <- peek @MemoryOverallocationBehaviorAMD ((p `plusPtr` 16 :: Ptr MemoryOverallocationBehaviorAMD))
    pure $ DeviceMemoryOverallocationCreateInfoAMD
             overallocationBehavior


instance Storable DeviceMemoryOverallocationCreateInfoAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceMemoryOverallocationCreateInfoAMD where
  zero = DeviceMemoryOverallocationCreateInfoAMD
           zero


-- No documentation found for TopLevel "VkMemoryOverallocationBehaviorAMD"
newtype MemoryOverallocationBehaviorAMD = MemoryOverallocationBehaviorAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkMemoryOverallocationBehaviorAMD" "VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD"
pattern MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD    = MemoryOverallocationBehaviorAMD 0
-- No documentation found for Nested "VkMemoryOverallocationBehaviorAMD" "VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD"
pattern MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD    = MemoryOverallocationBehaviorAMD 1
-- No documentation found for Nested "VkMemoryOverallocationBehaviorAMD" "VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD"
pattern MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD = MemoryOverallocationBehaviorAMD 2
{-# complete MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD,
             MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD,
             MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD :: MemoryOverallocationBehaviorAMD #-}

conNameMemoryOverallocationBehaviorAMD :: String
conNameMemoryOverallocationBehaviorAMD = "MemoryOverallocationBehaviorAMD"

enumPrefixMemoryOverallocationBehaviorAMD :: String
enumPrefixMemoryOverallocationBehaviorAMD = "MEMORY_OVERALLOCATION_BEHAVIOR_"

showTableMemoryOverallocationBehaviorAMD :: [(MemoryOverallocationBehaviorAMD, String)]
showTableMemoryOverallocationBehaviorAMD =
  [ (MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD   , "DEFAULT_AMD")
  , (MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD   , "ALLOWED_AMD")
  , (MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD, "DISALLOWED_AMD")
  ]


instance Show MemoryOverallocationBehaviorAMD where
showsPrec = enumShowsPrec enumPrefixMemoryOverallocationBehaviorAMD
                          showTableMemoryOverallocationBehaviorAMD
                          conNameMemoryOverallocationBehaviorAMD
                          (\(MemoryOverallocationBehaviorAMD x) -> x)
                          (showsPrec 11)


instance Read MemoryOverallocationBehaviorAMD where
  readPrec = enumReadPrec enumPrefixMemoryOverallocationBehaviorAMD
                          showTableMemoryOverallocationBehaviorAMD
                          conNameMemoryOverallocationBehaviorAMD
                          MemoryOverallocationBehaviorAMD


type AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION"
pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION = 1


type AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME = "VK_AMD_memory_overallocation_behavior"

-- No documentation found for TopLevel "VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME"
pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME = "VK_AMD_memory_overallocation_behavior"

