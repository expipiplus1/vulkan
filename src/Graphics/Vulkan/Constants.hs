{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Constants where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64
                , Word32
                )
import Data.Int( Int32
               )
import Foreign.Storable( Storable(..)
                       )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )


pattern SubpassExternal = 0xffffffff :: Word32

pattern UuidSize = 16
type UuidSize = 16

pattern RemainingMipLevels = 0xffffffff :: Word32

pattern LodClampNone = 1000.0

pattern MaxPhysicalDeviceNameSize = 256
type MaxPhysicalDeviceNameSize = 256

pattern AttachmentUnused = 0xffffffff :: Word32

pattern MaxMemoryTypes = 32
type MaxMemoryTypes = 32

pattern WholeSize = 0xffffffffffffffff :: Word64

pattern RemainingArrayLayers = 0xffffffff :: Word32

pattern QueueFamilyIgnored = 0xffffffff :: Word32

pattern MaxMemoryHeaps = 16
type MaxMemoryHeaps = 16

pattern False = 0
type False = 0
-- ** PipelineCacheHeaderVersion

newtype PipelineCacheHeaderVersion = PipelineCacheHeaderVersion Int32
  deriving (Eq, Ord, Storable)

instance Show PipelineCacheHeaderVersion where
  showsPrec _ PipelineCacheHeaderVersionOne = showString "PipelineCacheHeaderVersionOne"
  showsPrec p (PipelineCacheHeaderVersion x) = showParen (p >= 11) (showString "PipelineCacheHeaderVersion " . showsPrec 11 x)

instance Read PipelineCacheHeaderVersion where
  readPrec = parens ( choose [ ("PipelineCacheHeaderVersionOne", pure PipelineCacheHeaderVersionOne)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PipelineCacheHeaderVersion")
                        v <- step readPrec
                        pure (PipelineCacheHeaderVersion v)
                        )
                    )


pattern PipelineCacheHeaderVersionOne = PipelineCacheHeaderVersion 1


pattern MaxExtensionNameSize = 256
type MaxExtensionNameSize = 256

pattern MaxDescriptionSize = 256
type MaxDescriptionSize = 256

pattern True = 1
type True = 1
