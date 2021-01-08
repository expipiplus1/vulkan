{-# language CPP #-}
-- | = Name
--
-- VK_EXT_validation_flags - instance extension
--
-- == VK_EXT_validation_flags
--
-- [__Name String__]
--     @VK_EXT_validation_flags@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     62
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Deprecated/ by @VK_EXT_validation_features@ extension
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Debugging tools>
--
-- [__Contact__]
--
--     -   Tobin Ehlis
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_validation_flags:%20&body=@tobine%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-08-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobin Ehlis, Google
--
--     -   Courtney Goeltzenleuchter, Google
--
-- == Description
--
-- This extension provides the 'ValidationFlagsEXT' struct that can be
-- included in the @pNext@ chain of the
-- 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo' structure passed
-- as the @pCreateInfo@ parameter of
-- 'Vulkan.Core10.DeviceInitialization.createInstance'. The structure
-- contains an array of 'ValidationCheckEXT' values that will be disabled
-- by the validation layers.
--
-- == Deprecation by @VK_EXT_validation_features@
--
-- Functionality in this extension is subsumed into the
-- @VK_EXT_validation_features@ extension.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo':
--
--     -   'ValidationFlagsEXT'
--
-- == New Enums
--
-- -   'ValidationCheckEXT'
--
-- == New Enum Constants
--
-- -   'EXT_VALIDATION_FLAGS_EXTENSION_NAME'
--
-- -   'EXT_VALIDATION_FLAGS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VALIDATION_FLAGS_EXT'
--
-- == Version History
--
-- -   Revision 2, 2019-08-19 (Mark Lobodzinski)
--
--     -   Marked as deprecated
--
-- -   Revision 1, 2016-08-26 (Courtney Goeltzenleuchter)
--
--     -   Initial draft
--
-- = See Also
--
-- 'ValidationCheckEXT', 'ValidationFlagsEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_validation_flags Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_validation_flags  ( ValidationFlagsEXT(..)
                                                  , ValidationCheckEXT( VALIDATION_CHECK_ALL_EXT
                                                                      , VALIDATION_CHECK_SHADERS_EXT
                                                                      , ..
                                                                      )
                                                  , EXT_VALIDATION_FLAGS_SPEC_VERSION
                                                  , pattern EXT_VALIDATION_FLAGS_SPEC_VERSION
                                                  , EXT_VALIDATION_FLAGS_EXTENSION_NAME
                                                  , pattern EXT_VALIDATION_FLAGS_EXTENSION_NAME
                                                  ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_VALIDATION_FLAGS_EXT))
-- | VkValidationFlagsEXT - Specify validation checks to disable for a Vulkan
-- instance
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'ValidationCheckEXT'
data ValidationFlagsEXT = ValidationFlagsEXT
  { -- | @pDisabledValidationChecks@ is a pointer to an array of
    -- 'ValidationCheckEXT' values specifying the validation checks to be
    -- disabled.
    --
    -- #VUID-VkValidationFlagsEXT-pDisabledValidationChecks-parameter#
    -- @pDisabledValidationChecks@ /must/ be a valid pointer to an array of
    -- @disabledValidationCheckCount@ valid 'ValidationCheckEXT' values
    disabledValidationChecks :: Vector ValidationCheckEXT }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ValidationFlagsEXT)
#endif
deriving instance Show ValidationFlagsEXT

instance ToCStruct ValidationFlagsEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ValidationFlagsEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VALIDATION_FLAGS_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (disabledValidationChecks)) :: Word32))
    pPDisabledValidationChecks' <- ContT $ allocaBytesAligned @ValidationCheckEXT ((Data.Vector.length (disabledValidationChecks)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDisabledValidationChecks' `plusPtr` (4 * (i)) :: Ptr ValidationCheckEXT) (e)) (disabledValidationChecks)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ValidationCheckEXT))) (pPDisabledValidationChecks')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VALIDATION_FLAGS_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ValidationFlagsEXT where
  peekCStruct p = do
    disabledValidationCheckCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pDisabledValidationChecks <- peek @(Ptr ValidationCheckEXT) ((p `plusPtr` 24 :: Ptr (Ptr ValidationCheckEXT)))
    pDisabledValidationChecks' <- generateM (fromIntegral disabledValidationCheckCount) (\i -> peek @ValidationCheckEXT ((pDisabledValidationChecks `advancePtrBytes` (4 * (i)) :: Ptr ValidationCheckEXT)))
    pure $ ValidationFlagsEXT
             pDisabledValidationChecks'

instance Zero ValidationFlagsEXT where
  zero = ValidationFlagsEXT
           mempty


-- | VkValidationCheckEXT - Specify validation checks to disable
--
-- = See Also
--
-- 'ValidationFlagsEXT'
newtype ValidationCheckEXT = ValidationCheckEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'VALIDATION_CHECK_ALL_EXT' specifies that all validation checks are
-- disabled.
pattern VALIDATION_CHECK_ALL_EXT     = ValidationCheckEXT 0
-- | 'VALIDATION_CHECK_SHADERS_EXT' specifies that shader validation is
-- disabled.
pattern VALIDATION_CHECK_SHADERS_EXT = ValidationCheckEXT 1
{-# complete VALIDATION_CHECK_ALL_EXT,
             VALIDATION_CHECK_SHADERS_EXT :: ValidationCheckEXT #-}

conNameValidationCheckEXT :: String
conNameValidationCheckEXT = "ValidationCheckEXT"

enumPrefixValidationCheckEXT :: String
enumPrefixValidationCheckEXT = "VALIDATION_CHECK_"

showTableValidationCheckEXT :: [(ValidationCheckEXT, String)]
showTableValidationCheckEXT = [(VALIDATION_CHECK_ALL_EXT, "ALL_EXT"), (VALIDATION_CHECK_SHADERS_EXT, "SHADERS_EXT")]

instance Show ValidationCheckEXT where
  showsPrec = enumShowsPrec enumPrefixValidationCheckEXT
                            showTableValidationCheckEXT
                            conNameValidationCheckEXT
                            (\(ValidationCheckEXT x) -> x)
                            (showsPrec 11)

instance Read ValidationCheckEXT where
  readPrec =
    enumReadPrec enumPrefixValidationCheckEXT showTableValidationCheckEXT conNameValidationCheckEXT ValidationCheckEXT


type EXT_VALIDATION_FLAGS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_VALIDATION_FLAGS_SPEC_VERSION"
pattern EXT_VALIDATION_FLAGS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_VALIDATION_FLAGS_SPEC_VERSION = 2


type EXT_VALIDATION_FLAGS_EXTENSION_NAME = "VK_EXT_validation_flags"

-- No documentation found for TopLevel "VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME"
pattern EXT_VALIDATION_FLAGS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_VALIDATION_FLAGS_EXTENSION_NAME = "VK_EXT_validation_flags"

