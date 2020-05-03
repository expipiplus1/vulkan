{-# language CPP #-}
module Vulkan.Core11.Enums.TessellationDomainOrigin  (TessellationDomainOrigin( TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT
                                                                              , TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
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
import Vulkan.Zero (Zero)
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
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.PipelineTessellationDomainOriginStateCreateInfo'
newtype TessellationDomainOrigin = TessellationDomainOrigin Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT' specifies that the origin of the
-- domain space is in the upper left corner, as shown in figure
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#img-tessellation-topology-ul>.
pattern TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT = TessellationDomainOrigin 0
-- | 'TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT' specifies that the origin of the
-- domain space is in the lower left corner, as shown in figure
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#img-tessellation-topology-ll>.
pattern TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT = TessellationDomainOrigin 1
{-# complete TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT,
             TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT :: TessellationDomainOrigin #-}

instance Show TessellationDomainOrigin where
  showsPrec p = \case
    TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT -> showString "TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT"
    TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT -> showString "TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT"
    TessellationDomainOrigin x -> showParen (p >= 11) (showString "TessellationDomainOrigin " . showsPrec 11 x)

instance Read TessellationDomainOrigin where
  readPrec = parens (choose [("TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT", pure TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT)
                            , ("TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT", pure TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT)]
                     +++
                     prec 10 (do
                       expectP (Ident "TessellationDomainOrigin")
                       v <- step readPrec
                       pure (TessellationDomainOrigin v)))

