{-# language CPP #-}
-- No documentation found for Chapter "PhysicalDeviceType"
module Vulkan.Core10.Enums.PhysicalDeviceType  (PhysicalDeviceType( PHYSICAL_DEVICE_TYPE_OTHER
                                                                  , PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
                                                                  , PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
                                                                  , PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
                                                                  , PHYSICAL_DEVICE_TYPE_CPU
                                                                  , ..
                                                                  )) where

import Data.Foldable (asum)
import GHC.Base ((<$))
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
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
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
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'
newtype PhysicalDeviceType = PhysicalDeviceType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PHYSICAL_DEVICE_TYPE_OTHER' - the device does not match any other
-- available types.
pattern PHYSICAL_DEVICE_TYPE_OTHER          = PhysicalDeviceType 0
-- | 'PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU' - the device is typically one
-- embedded in or tightly coupled with the host.
pattern PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = PhysicalDeviceType 1
-- | 'PHYSICAL_DEVICE_TYPE_DISCRETE_GPU' - the device is typically a separate
-- processor connected to the host via an interlink.
pattern PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   = PhysicalDeviceType 2
-- | 'PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU' - the device is typically a virtual
-- node in a virtualization environment.
pattern PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU    = PhysicalDeviceType 3
-- | 'PHYSICAL_DEVICE_TYPE_CPU' - the device is typically running on the same
-- processors as the host.
pattern PHYSICAL_DEVICE_TYPE_CPU            = PhysicalDeviceType 4
{-# complete PHYSICAL_DEVICE_TYPE_OTHER,
             PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU,
             PHYSICAL_DEVICE_TYPE_DISCRETE_GPU,
             PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU,
             PHYSICAL_DEVICE_TYPE_CPU :: PhysicalDeviceType #-}

conNamePhysicalDeviceType :: String
conNamePhysicalDeviceType = "PhysicalDeviceType"

enumPrefixPhysicalDeviceType :: String
enumPrefixPhysicalDeviceType = "PHYSICAL_DEVICE_TYPE_"

showTablePhysicalDeviceType :: [(PhysicalDeviceType, String)]
showTablePhysicalDeviceType =
  [ (PHYSICAL_DEVICE_TYPE_OTHER         , "OTHER")
  , (PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU, "INTEGRATED_GPU")
  , (PHYSICAL_DEVICE_TYPE_DISCRETE_GPU  , "DISCRETE_GPU")
  , (PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU   , "VIRTUAL_GPU")
  , (PHYSICAL_DEVICE_TYPE_CPU           , "CPU")
  ]

instance Show PhysicalDeviceType where
  showsPrec p e = case lookup e showTablePhysicalDeviceType of
    Just s -> showString enumPrefixPhysicalDeviceType . showString s
    Nothing ->
      let PhysicalDeviceType x = e
      in  showParen (p >= 11) (showString conNamePhysicalDeviceType . showString " " . showsPrec 11 x)

instance Read PhysicalDeviceType where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPhysicalDeviceType
          asum ((\(e, s) -> e <$ string s) <$> showTablePhysicalDeviceType)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePhysicalDeviceType)
            v <- step readPrec
            pure (PhysicalDeviceType v)
          )
    )

