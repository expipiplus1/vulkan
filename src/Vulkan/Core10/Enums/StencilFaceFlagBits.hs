{-# language CPP #-}
-- No documentation found for Chapter "StencilFaceFlagBits"
module Vulkan.Core10.Enums.StencilFaceFlagBits  ( pattern STENCIL_FRONT_AND_BACK
                                                , StencilFaceFlags
                                                , StencilFaceFlagBits( STENCIL_FACE_FRONT_BIT
                                                                     , STENCIL_FACE_BACK_BIT
                                                                     , STENCIL_FACE_FRONT_AND_BACK
                                                                     , ..
                                                                     )
                                                ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VK_STENCIL_FRONT_AND_BACK"
pattern STENCIL_FRONT_AND_BACK = STENCIL_FACE_FRONT_AND_BACK


type StencilFaceFlags = StencilFaceFlagBits

-- No documentation found for TopLevel "VkStencilFaceFlagBits"
newtype StencilFaceFlagBits = StencilFaceFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkStencilFaceFlagBits" "VK_STENCIL_FACE_FRONT_BIT"
pattern STENCIL_FACE_FRONT_BIT      = StencilFaceFlagBits 0x00000001
-- No documentation found for Nested "VkStencilFaceFlagBits" "VK_STENCIL_FACE_BACK_BIT"
pattern STENCIL_FACE_BACK_BIT       = StencilFaceFlagBits 0x00000002
-- No documentation found for Nested "VkStencilFaceFlagBits" "VK_STENCIL_FACE_FRONT_AND_BACK"
pattern STENCIL_FACE_FRONT_AND_BACK = StencilFaceFlagBits 0x00000003

conNameStencilFaceFlagBits :: String
conNameStencilFaceFlagBits = "StencilFaceFlagBits"

enumPrefixStencilFaceFlagBits :: String
enumPrefixStencilFaceFlagBits = "STENCIL_FACE_"

showTableStencilFaceFlagBits :: [(StencilFaceFlagBits, String)]
showTableStencilFaceFlagBits =
  [ (STENCIL_FACE_FRONT_BIT     , "FRONT_BIT")
  , (STENCIL_FACE_BACK_BIT      , "BACK_BIT")
  , (STENCIL_FACE_FRONT_AND_BACK, "FRONT_AND_BACK")
  ]


instance Show StencilFaceFlagBits where
showsPrec = enumShowsPrec enumPrefixStencilFaceFlagBits
                          showTableStencilFaceFlagBits
                          conNameStencilFaceFlagBits
                          (\(StencilFaceFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read StencilFaceFlagBits where
  readPrec = enumReadPrec enumPrefixStencilFaceFlagBits
                          showTableStencilFaceFlagBits
                          conNameStencilFaceFlagBits
                          StencilFaceFlagBits

