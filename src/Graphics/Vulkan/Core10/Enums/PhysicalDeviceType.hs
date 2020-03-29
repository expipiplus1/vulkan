{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.PhysicalDeviceType  (PhysicalDeviceType( PHYSICAL_DEVICE_TYPE_OTHER
                                                                           , PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
                                                                           , PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
                                                                           , PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
                                                                           , PHYSICAL_DEVICE_TYPE_CPU
                                                                           , ..
                                                                           )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Zero (Zero)
-- | VkPhysicalDeviceType - Supported physical device types
--
-- = Description
--
-- The physical device type is advertised for informational purposes only,
-- and does not directly affect the operation of the system. However, the
-- device type /may/ correlate with other advertised properties or
-- capabilities of the system, such as how many memory heaps there are.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'
newtype PhysicalDeviceType = PhysicalDeviceType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PHYSICAL_DEVICE_TYPE_OTHER' - the device does not match any other
-- available types.
pattern PHYSICAL_DEVICE_TYPE_OTHER = PhysicalDeviceType 0
-- | 'PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU' - the device is typically one
-- embedded in or tightly coupled with the host.
pattern PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = PhysicalDeviceType 1
-- | 'PHYSICAL_DEVICE_TYPE_DISCRETE_GPU' - the device is typically a separate
-- processor connected to the host via an interlink.
pattern PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = PhysicalDeviceType 2
-- | 'PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU' - the device is typically a virtual
-- node in a virtualization environment.
pattern PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = PhysicalDeviceType 3
-- | 'PHYSICAL_DEVICE_TYPE_CPU' - the device is typically running on the same
-- processors as the host.
pattern PHYSICAL_DEVICE_TYPE_CPU = PhysicalDeviceType 4
{-# complete PHYSICAL_DEVICE_TYPE_OTHER,
             PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU,
             PHYSICAL_DEVICE_TYPE_DISCRETE_GPU,
             PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU,
             PHYSICAL_DEVICE_TYPE_CPU :: PhysicalDeviceType #-}

instance Show PhysicalDeviceType where
  showsPrec p = \case
    PHYSICAL_DEVICE_TYPE_OTHER -> showString "PHYSICAL_DEVICE_TYPE_OTHER"
    PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> showString "PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU"
    PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> showString "PHYSICAL_DEVICE_TYPE_DISCRETE_GPU"
    PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> showString "PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU"
    PHYSICAL_DEVICE_TYPE_CPU -> showString "PHYSICAL_DEVICE_TYPE_CPU"
    PhysicalDeviceType x -> showParen (p >= 11) (showString "PhysicalDeviceType " . showsPrec 11 x)

instance Read PhysicalDeviceType where
  readPrec = parens (choose [("PHYSICAL_DEVICE_TYPE_OTHER", pure PHYSICAL_DEVICE_TYPE_OTHER)
                            , ("PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU", pure PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU)
                            , ("PHYSICAL_DEVICE_TYPE_DISCRETE_GPU", pure PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
                            , ("PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU", pure PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU)
                            , ("PHYSICAL_DEVICE_TYPE_CPU", pure PHYSICAL_DEVICE_TYPE_CPU)]
                     +++
                     prec 10 (do
                       expectP (Ident "PhysicalDeviceType")
                       v <- step readPrec
                       pure (PhysicalDeviceType v)))

