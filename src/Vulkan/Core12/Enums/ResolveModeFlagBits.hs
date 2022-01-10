{-# language CPP #-}
-- No documentation found for Chapter "ResolveModeFlagBits"
module Vulkan.Core12.Enums.ResolveModeFlagBits  ( ResolveModeFlags
                                                , ResolveModeFlagBits( RESOLVE_MODE_NONE
                                                                     , RESOLVE_MODE_SAMPLE_ZERO_BIT
                                                                     , RESOLVE_MODE_AVERAGE_BIT
                                                                     , RESOLVE_MODE_MIN_BIT
                                                                     , RESOLVE_MODE_MAX_BIT
                                                                     , ..
                                                                     )
                                                ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type ResolveModeFlags = ResolveModeFlagBits

-- | VkResolveModeFlagBits - Bitmask indicating supported depth and stencil
-- resolve modes
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_depth_stencil_resolve VK_KHR_depth_stencil_resolve>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingAttachmentInfoKHR',
-- 'ResolveModeFlags',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'
newtype ResolveModeFlagBits = ResolveModeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'RESOLVE_MODE_NONE' indicates that no resolve operation is done.
pattern RESOLVE_MODE_NONE            = ResolveModeFlagBits 0x00000000
-- | 'RESOLVE_MODE_SAMPLE_ZERO_BIT' indicates that result of the resolve
-- operation is equal to the value of sample 0.
pattern RESOLVE_MODE_SAMPLE_ZERO_BIT = ResolveModeFlagBits 0x00000001
-- | 'RESOLVE_MODE_AVERAGE_BIT' indicates that result of the resolve
-- operation is the average of the sample values.
pattern RESOLVE_MODE_AVERAGE_BIT     = ResolveModeFlagBits 0x00000002
-- | 'RESOLVE_MODE_MIN_BIT' indicates that result of the resolve operation is
-- the minimum of the sample values.
pattern RESOLVE_MODE_MIN_BIT         = ResolveModeFlagBits 0x00000004
-- | 'RESOLVE_MODE_MAX_BIT' indicates that result of the resolve operation is
-- the maximum of the sample values.
pattern RESOLVE_MODE_MAX_BIT         = ResolveModeFlagBits 0x00000008

conNameResolveModeFlagBits :: String
conNameResolveModeFlagBits = "ResolveModeFlagBits"

enumPrefixResolveModeFlagBits :: String
enumPrefixResolveModeFlagBits = "RESOLVE_MODE_"

showTableResolveModeFlagBits :: [(ResolveModeFlagBits, String)]
showTableResolveModeFlagBits =
  [ (RESOLVE_MODE_NONE           , "NONE")
  , (RESOLVE_MODE_SAMPLE_ZERO_BIT, "SAMPLE_ZERO_BIT")
  , (RESOLVE_MODE_AVERAGE_BIT    , "AVERAGE_BIT")
  , (RESOLVE_MODE_MIN_BIT        , "MIN_BIT")
  , (RESOLVE_MODE_MAX_BIT        , "MAX_BIT")
  ]

instance Show ResolveModeFlagBits where
  showsPrec = enumShowsPrec enumPrefixResolveModeFlagBits
                            showTableResolveModeFlagBits
                            conNameResolveModeFlagBits
                            (\(ResolveModeFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ResolveModeFlagBits where
  readPrec = enumReadPrec enumPrefixResolveModeFlagBits
                          showTableResolveModeFlagBits
                          conNameResolveModeFlagBits
                          ResolveModeFlagBits

