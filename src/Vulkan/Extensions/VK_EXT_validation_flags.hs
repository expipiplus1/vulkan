{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_validation_flags"
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_VALIDATION_FLAGS_EXT))

-- No documentation found for TopLevel "VkValidationFlagsEXT"
data ValidationFlagsEXT = ValidationFlagsEXT
  { -- No documentation found for Nested "VkValidationFlagsEXT" "pDisabledValidationChecks"
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
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VALIDATION_FLAGS_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPDisabledValidationChecks' <- ContT $ allocaBytesAligned @ValidationCheckEXT ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDisabledValidationChecks' `plusPtr` (4 * (i)) :: Ptr ValidationCheckEXT) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ValidationCheckEXT))) (pPDisabledValidationChecks')
    lift $ f

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


-- No documentation found for TopLevel "VkValidationCheckEXT"
newtype ValidationCheckEXT = ValidationCheckEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkValidationCheckEXT" "VK_VALIDATION_CHECK_ALL_EXT"
pattern VALIDATION_CHECK_ALL_EXT     = ValidationCheckEXT 0
-- No documentation found for Nested "VkValidationCheckEXT" "VK_VALIDATION_CHECK_SHADERS_EXT"
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

