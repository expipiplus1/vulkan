{-# language CPP #-}
-- No documentation found for Chapter "ChromaLocation"
module Vulkan.Core11.Enums.ChromaLocation  (ChromaLocation( CHROMA_LOCATION_COSITED_EVEN
                                                          , CHROMA_LOCATION_MIDPOINT
                                                          , ..
                                                          )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkChromaLocation"
newtype ChromaLocation = ChromaLocation Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkChromaLocation" "VK_CHROMA_LOCATION_COSITED_EVEN"
pattern CHROMA_LOCATION_COSITED_EVEN = ChromaLocation 0
-- No documentation found for Nested "VkChromaLocation" "VK_CHROMA_LOCATION_MIDPOINT"
pattern CHROMA_LOCATION_MIDPOINT     = ChromaLocation 1
{-# complete CHROMA_LOCATION_COSITED_EVEN,
             CHROMA_LOCATION_MIDPOINT :: ChromaLocation #-}

conNameChromaLocation :: String
conNameChromaLocation = "ChromaLocation"

enumPrefixChromaLocation :: String
enumPrefixChromaLocation = "CHROMA_LOCATION_"

showTableChromaLocation :: [(ChromaLocation, String)]
showTableChromaLocation = [(CHROMA_LOCATION_COSITED_EVEN, "COSITED_EVEN"), (CHROMA_LOCATION_MIDPOINT, "MIDPOINT")]


instance Show ChromaLocation where
showsPrec = enumShowsPrec enumPrefixChromaLocation
                          showTableChromaLocation
                          conNameChromaLocation
                          (\(ChromaLocation x) -> x)
                          (showsPrec 11)


instance Read ChromaLocation where
  readPrec = enumReadPrec enumPrefixChromaLocation showTableChromaLocation conNameChromaLocation ChromaLocation

