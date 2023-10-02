{-# language CPP #-}
-- No documentation found for Chapter "PipelineMatchControl"
module Vulkan.Core10.Enums.PipelineMatchControl  (PipelineMatchControl( PIPELINE_MATCH_CONTROL_APPLICATION_UUID_EXACT_MATCH
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

-- No documentation found for TopLevel "VkPipelineMatchControl"
newtype PipelineMatchControl = PipelineMatchControl Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPipelineMatchControl" "VK_PIPELINE_MATCH_CONTROL_APPLICATION_UUID_EXACT_MATCH"
pattern PIPELINE_MATCH_CONTROL_APPLICATION_UUID_EXACT_MATCH = PipelineMatchControl 0

{-# COMPLETE PIPELINE_MATCH_CONTROL_APPLICATION_UUID_EXACT_MATCH :: PipelineMatchControl #-}

conNamePipelineMatchControl :: String
conNamePipelineMatchControl = "PipelineMatchControl"

enumPrefixPipelineMatchControl :: String
enumPrefixPipelineMatchControl = "PIPELINE_MATCH_CONTROL_APPLICATION_UUID_EXACT_MATCH"

showTablePipelineMatchControl :: [(PipelineMatchControl, String)]
showTablePipelineMatchControl =
  [
    ( PIPELINE_MATCH_CONTROL_APPLICATION_UUID_EXACT_MATCH
    , ""
    )
  ]

instance Show PipelineMatchControl where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineMatchControl
      showTablePipelineMatchControl
      conNamePipelineMatchControl
      (\(PipelineMatchControl x) -> x)
      (showsPrec 11)

instance Read PipelineMatchControl where
  readPrec =
    enumReadPrec
      enumPrefixPipelineMatchControl
      showTablePipelineMatchControl
      conNamePipelineMatchControl
      PipelineMatchControl
