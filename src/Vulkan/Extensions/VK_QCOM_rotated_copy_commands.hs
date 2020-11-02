{-# language CPP #-}
module Vulkan.Extensions.VK_QCOM_rotated_copy_commands  ( CopyCommandTransformInfoQCOM(..)
                                                        , QCOM_rotated_copy_commands_SPEC_VERSION
                                                        , pattern QCOM_rotated_copy_commands_SPEC_VERSION
                                                        , QCOM_rotated_copy_commands_EXTENSION_NAME
                                                        , pattern QCOM_rotated_copy_commands_EXTENSION_NAME
                                                        , SurfaceTransformFlagBitsKHR(..)
                                                        , SurfaceTransformFlagsKHR
                                                        ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_COMMAND_TRANSFORM_INFO_QCOM))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
-- | VkCopyCommandTransformInfoQCOM - Structure describing transform
-- parameters of rotated copy command
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR'
data CopyCommandTransformInfoQCOM = CopyCommandTransformInfoQCOM
  { -- | @transform@ is a
    -- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR' value
    -- describing the transform to be applied.
    --
    -- @transform@ /must/ be
    -- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
    -- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
    -- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
    -- or
    -- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR'
    transform :: SurfaceTransformFlagBitsKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyCommandTransformInfoQCOM)
#endif
deriving instance Show CopyCommandTransformInfoQCOM

instance ToCStruct CopyCommandTransformInfoQCOM where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyCommandTransformInfoQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_COMMAND_TRANSFORM_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SurfaceTransformFlagBitsKHR)) (transform)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_COMMAND_TRANSFORM_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    f

instance FromCStruct CopyCommandTransformInfoQCOM where
  peekCStruct p = do
    transform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 16 :: Ptr SurfaceTransformFlagBitsKHR))
    pure $ CopyCommandTransformInfoQCOM
             transform

instance Storable CopyCommandTransformInfoQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CopyCommandTransformInfoQCOM where
  zero = CopyCommandTransformInfoQCOM
           zero


type QCOM_rotated_copy_commands_SPEC_VERSION = 0

-- No documentation found for TopLevel "VK_QCOM_rotated_copy_commands_SPEC_VERSION"
pattern QCOM_rotated_copy_commands_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_rotated_copy_commands_SPEC_VERSION = 0


type QCOM_rotated_copy_commands_EXTENSION_NAME = "VK_QCOM_rotated_copy_commands"

-- No documentation found for TopLevel "VK_QCOM_rotated_copy_commands_EXTENSION_NAME"
pattern QCOM_rotated_copy_commands_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_rotated_copy_commands_EXTENSION_NAME = "VK_QCOM_rotated_copy_commands"

