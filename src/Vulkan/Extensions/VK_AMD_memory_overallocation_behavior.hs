{-# language CPP #-}
-- | = Name
--
-- VK_AMD_memory_overallocation_behavior - device extension
--
-- == VK_AMD_memory_overallocation_behavior
--
-- [__Name String__]
--     @VK_AMD_memory_overallocation_behavior@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     190
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Martin Dinkov
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_AMD_memory_overallocation_behavior:%20&body=@mdinkov%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-09-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Martin Dinkov, AMD
--
--     -   Matthaeus Chajdas, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Jon Campbell, AMD
--
-- == Description
--
-- This extension allows controlling whether explicit overallocation beyond
-- the device memory heap sizes (reported by
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceMemoryProperties') is
-- allowed or not. Overallocation may lead to performance loss and is not
-- supported for all platforms.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DeviceMemoryOverallocationCreateInfoAMD'
--
-- == New Enums
--
-- -   'MemoryOverallocationBehaviorAMD'
--
-- == New Enum Constants
--
-- -   'AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME'
--
-- -   'AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD'
--
-- == Version History
--
-- -   Revision 1, 2018-09-19 (Martin Dinkov)
--
--     -   Initial draft.
--
-- = See Also
--
-- 'DeviceMemoryOverallocationCreateInfoAMD',
-- 'MemoryOverallocationBehaviorAMD'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_memory_overallocation_behavior Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

import Data.Foldable (asum)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base ((<$))
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
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
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD))
-- | VkDeviceMemoryOverallocationCreateInfoAMD - Specify memory
-- overallocation behavior for a Vulkan device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'MemoryOverallocationBehaviorAMD',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceMemoryOverallocationCreateInfoAMD = DeviceMemoryOverallocationCreateInfoAMD
  { -- | @overallocationBehavior@ is the desired overallocation behavior.
    --
    -- #VUID-VkDeviceMemoryOverallocationCreateInfoAMD-overallocationBehavior-parameter#
    -- @overallocationBehavior@ /must/ be a valid
    -- 'MemoryOverallocationBehaviorAMD' value
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


-- | VkMemoryOverallocationBehaviorAMD - Specify memory overallocation
-- behavior
--
-- = See Also
--
-- 'DeviceMemoryOverallocationCreateInfoAMD'
newtype MemoryOverallocationBehaviorAMD = MemoryOverallocationBehaviorAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD' lets the implementation
-- decide if overallocation is allowed.
pattern MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD    = MemoryOverallocationBehaviorAMD 0
-- | 'MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD' specifies overallocation is
-- allowed if platform permits.
pattern MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD    = MemoryOverallocationBehaviorAMD 1
-- | 'MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD' specifies the
-- application is not allowed to allocate device memory beyond the heap
-- sizes reported by
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceMemoryProperties'.
-- Allocations that are not explicitly made by the application within the
-- scope of the Vulkan instance are not accounted for.
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
  showsPrec p e = case lookup e showTableMemoryOverallocationBehaviorAMD of
    Just s -> showString enumPrefixMemoryOverallocationBehaviorAMD . showString s
    Nothing ->
      let MemoryOverallocationBehaviorAMD x = e
      in  showParen (p >= 11) (showString conNameMemoryOverallocationBehaviorAMD . showString " " . showsPrec 11 x)

instance Read MemoryOverallocationBehaviorAMD where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixMemoryOverallocationBehaviorAMD
          asum ((\(e, s) -> e <$ string s) <$> showTableMemoryOverallocationBehaviorAMD)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameMemoryOverallocationBehaviorAMD)
            v <- step readPrec
            pure (MemoryOverallocationBehaviorAMD v)
          )
    )


type AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION"
pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION = 1


type AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME = "VK_AMD_memory_overallocation_behavior"

-- No documentation found for TopLevel "VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME"
pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME = "VK_AMD_memory_overallocation_behavior"

