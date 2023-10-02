{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_application_parameters"
module Vulkan.Extensions.VK_EXT_application_parameters  ( ApplicationParametersEXT(..)
                                                        , EXT_APPLICATION_PARAMETERS_SPEC_VERSION
                                                        , pattern EXT_APPLICATION_PARAMETERS_SPEC_VERSION
                                                        , EXT_APPLICATION_PARAMETERS_EXTENSION_NAME
                                                        , pattern EXT_APPLICATION_PARAMETERS_EXTENSION_NAME
                                                        ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_APPLICATION_PARAMETERS_EXT))
-- No documentation found for TopLevel "VkApplicationParametersEXT"
data ApplicationParametersEXT = ApplicationParametersEXT
  { -- No documentation found for Nested "VkApplicationParametersEXT" "vendorID"
    vendorID :: Word32
  , -- No documentation found for Nested "VkApplicationParametersEXT" "deviceID"
    deviceID :: Word32
  , -- No documentation found for Nested "VkApplicationParametersEXT" "key"
    key :: Word32
  , -- No documentation found for Nested "VkApplicationParametersEXT" "value"
    value :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ApplicationParametersEXT)
#endif
deriving instance Show ApplicationParametersEXT

instance ToCStruct ApplicationParametersEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ApplicationParametersEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_APPLICATION_PARAMETERS_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (vendorID)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (deviceID)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (key)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (value)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_APPLICATION_PARAMETERS_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    f

instance FromCStruct ApplicationParametersEXT where
  peekCStruct p = do
    vendorID <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    deviceID <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    key <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    value <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    pure $ ApplicationParametersEXT
             vendorID deviceID key value

instance Storable ApplicationParametersEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ApplicationParametersEXT where
  zero = ApplicationParametersEXT
           zero
           zero
           zero
           zero


type EXT_APPLICATION_PARAMETERS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_APPLICATION_PARAMETERS_SPEC_VERSION"
pattern EXT_APPLICATION_PARAMETERS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_APPLICATION_PARAMETERS_SPEC_VERSION = 1


type EXT_APPLICATION_PARAMETERS_EXTENSION_NAME = "VK_EXT_application_parameters"

-- No documentation found for TopLevel "VK_EXT_APPLICATION_PARAMETERS_EXTENSION_NAME"
pattern EXT_APPLICATION_PARAMETERS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_APPLICATION_PARAMETERS_EXTENSION_NAME = "VK_EXT_application_parameters"

