{-# language CPP #-}
-- No documentation found for Chapter "TessellationDomainOrigin"
module Vulkan.Core11.Enums.TessellationDomainOrigin  (TessellationDomainOrigin( TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT
                                                                              , TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
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
-- No documentation found for TopLevel "VkTessellationDomainOrigin"
newtype TessellationDomainOrigin = TessellationDomainOrigin Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkTessellationDomainOrigin" "VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT"
pattern TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT = TessellationDomainOrigin 0
-- No documentation found for Nested "VkTessellationDomainOrigin" "VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT"
pattern TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT = TessellationDomainOrigin 1
{-# complete TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT,
             TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT :: TessellationDomainOrigin #-}

conNameTessellationDomainOrigin :: String
conNameTessellationDomainOrigin = "TessellationDomainOrigin"

enumPrefixTessellationDomainOrigin :: String
enumPrefixTessellationDomainOrigin = "TESSELLATION_DOMAIN_ORIGIN_"

showTableTessellationDomainOrigin :: [(TessellationDomainOrigin, String)]
showTableTessellationDomainOrigin =
  [(TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT, "UPPER_LEFT"), (TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT, "LOWER_LEFT")]


instance Show TessellationDomainOrigin where
showsPrec = enumShowsPrec enumPrefixTessellationDomainOrigin
                          showTableTessellationDomainOrigin
                          conNameTessellationDomainOrigin
                          (\(TessellationDomainOrigin x) -> x)
                          (showsPrec 11)


instance Read TessellationDomainOrigin where
  readPrec = enumReadPrec enumPrefixTessellationDomainOrigin
                          showTableTessellationDomainOrigin
                          conNameTessellationDomainOrigin
                          TessellationDomainOrigin

