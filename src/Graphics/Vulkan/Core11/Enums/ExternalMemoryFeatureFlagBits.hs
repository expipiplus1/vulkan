{-# language CPP #-}
module Graphics.Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits  ( ExternalMemoryFeatureFlagBits( EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
                                                                                                  , EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
                                                                                                  , EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
                                                                                                  , ..
                                                                                                  )
                                                                   , ExternalMemoryFeatureFlags
                                                                   ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Zero (Zero)
-- | VkExternalMemoryFeatureFlagBits - Bitmask specifying features of an
-- external memory handle type
--
-- = Description
--
-- Because their semantics in external APIs roughly align with that of an
-- image or buffer with a dedicated allocation in Vulkan, implementations
-- are /required/ to report 'EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT'
-- for the following external handle types:
--
-- Implementations /must/ not report
-- 'EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT' for buffers with external
-- handle type
-- 'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'.
-- Implementations /must/ not report
-- 'EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT' for images or buffers with
-- external handle type
-- 'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT',
-- or
-- 'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT'.
--
-- = See Also
--
-- 'ExternalMemoryFeatureFlags'
newtype ExternalMemoryFeatureFlagBits = ExternalMemoryFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT' specifies that images or
-- buffers created with the specified parameters and handle type /must/ use
-- the mechanisms defined by
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedRequirements'
-- and
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'
-- to create (or import) a dedicated allocation for the image or buffer.
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT = ExternalMemoryFeatureFlagBits 0x00000001
-- | 'EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT' specifies that handles of this
-- type /can/ be exported from Vulkan memory objects.
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT = ExternalMemoryFeatureFlagBits 0x00000002
-- | 'EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT' specifies that handles of this
-- type /can/ be imported as Vulkan memory objects.
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT = ExternalMemoryFeatureFlagBits 0x00000004

type ExternalMemoryFeatureFlags = ExternalMemoryFeatureFlagBits

instance Show ExternalMemoryFeatureFlagBits where
  showsPrec p = \case
    EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT -> showString "EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT"
    EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT -> showString "EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT"
    EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT -> showString "EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT"
    ExternalMemoryFeatureFlagBits x -> showParen (p >= 11) (showString "ExternalMemoryFeatureFlagBits 0x" . showHex x)

instance Read ExternalMemoryFeatureFlagBits where
  readPrec = parens (choose [("EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT", pure EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT)
                            , ("EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT", pure EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT)
                            , ("EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT", pure EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ExternalMemoryFeatureFlagBits")
                       v <- step readPrec
                       pure (ExternalMemoryFeatureFlagBits v)))

