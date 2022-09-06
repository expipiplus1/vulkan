{-# language CPP #-}
-- No documentation found for Chapter "TessellationDomainOrigin"
module Vulkan.Core11.Enums.TessellationDomainOrigin  (TessellationDomainOrigin( TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT
                                                                              , TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
                                                                              , ..
                                                                              )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkTessellationDomainOrigin - Enum describing tessellation domain origin
--
-- = Description
--
-- This enum affects how the @VertexOrderCw@ and @VertexOrderCcw@
-- tessellation execution modes are interpreted, since the winding is
-- defined relative to the orientation of the domain.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.PipelineTessellationDomainOriginStateCreateInfo'
newtype TessellationDomainOrigin = TessellationDomainOrigin Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT' specifies that the origin of the
-- domain space is in the upper left corner, as shown in figure
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#img-tessellation-topology-ul>.
pattern TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT = TessellationDomainOrigin 0
-- | 'TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT' specifies that the origin of the
-- domain space is in the lower left corner, as shown in figure
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#img-tessellation-topology-ll>.
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

