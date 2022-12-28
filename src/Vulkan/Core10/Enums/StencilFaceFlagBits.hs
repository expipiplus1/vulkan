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

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
-- No documentation found for TopLevel "VK_STENCIL_FRONT_AND_BACK"
pattern STENCIL_FRONT_AND_BACK = STENCIL_FACE_FRONT_AND_BACK


type StencilFaceFlags = StencilFaceFlagBits

-- | VkStencilFaceFlagBits - Bitmask specifying sets of stencil state for
-- which to update the compare mask
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'StencilFaceFlags'
newtype StencilFaceFlagBits = StencilFaceFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'STENCIL_FACE_FRONT_BIT' specifies that only the front set of stencil
-- state is updated.
pattern STENCIL_FACE_FRONT_BIT = StencilFaceFlagBits 0x00000001

-- | 'STENCIL_FACE_BACK_BIT' specifies that only the back set of stencil
-- state is updated.
pattern STENCIL_FACE_BACK_BIT = StencilFaceFlagBits 0x00000002

-- | 'STENCIL_FACE_FRONT_AND_BACK' is the combination of
-- 'STENCIL_FACE_FRONT_BIT' and 'STENCIL_FACE_BACK_BIT', and specifies that
-- both sets of stencil state are updated.
pattern STENCIL_FACE_FRONT_AND_BACK = StencilFaceFlagBits 0x00000003

conNameStencilFaceFlagBits :: String
conNameStencilFaceFlagBits = "StencilFaceFlagBits"

enumPrefixStencilFaceFlagBits :: String
enumPrefixStencilFaceFlagBits = "STENCIL_FACE_"

showTableStencilFaceFlagBits :: [(StencilFaceFlagBits, String)]
showTableStencilFaceFlagBits =
  [ (STENCIL_FACE_FRONT_BIT, "FRONT_BIT")
  , (STENCIL_FACE_BACK_BIT, "BACK_BIT")
  ,
    ( STENCIL_FACE_FRONT_AND_BACK
    , "FRONT_AND_BACK"
    )
  ]

instance Show StencilFaceFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixStencilFaceFlagBits
      showTableStencilFaceFlagBits
      conNameStencilFaceFlagBits
      (\(StencilFaceFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read StencilFaceFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixStencilFaceFlagBits
      showTableStencilFaceFlagBits
      conNameStencilFaceFlagBits
      StencilFaceFlagBits
