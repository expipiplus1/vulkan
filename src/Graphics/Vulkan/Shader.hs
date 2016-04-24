{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Shader where

import Graphics.Vulkan.Device( Device(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Core( StructureType(..)
                           , Result(..)
                           , Flags(..)
                           )
import Foreign.C.Types( CSize(..)
                      )


data ShaderModuleCreateInfo =
  ShaderModuleCreateInfo{ sType :: StructureType 
                        , pNext :: Ptr Void 
                        , flags :: ShaderModuleCreateFlags 
                        , codeSize :: CSize 
                        , pCode :: Ptr Word32 
                        }
  deriving (Eq, Ord)

instance Storable ShaderModuleCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = ShaderModuleCreateInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: ShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: ShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: ShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 24) (codeSize (poked :: ShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 32) (pCode (poked :: ShaderModuleCreateInfo))


-- ** destroyShaderModule
foreign import ccall "vkDestroyShaderModule" destroyShaderModule ::
  Device -> ShaderModule -> Ptr AllocationCallbacks -> IO ()

-- ** ShaderModuleCreateFlags
-- | Opaque flag
newtype ShaderModuleCreateFlags = ShaderModuleCreateFlags Flags
  deriving (Eq, Ord, Storable)

-- ** ShaderStageFlags

newtype ShaderStageFlags = ShaderStageFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show ShaderStageFlags where
  showsPrec _ ShaderStageVertexBit = showString "ShaderStageVertexBit"
  showsPrec _ ShaderStageTessellationControlBit = showString "ShaderStageTessellationControlBit"
  showsPrec _ ShaderStageTessellationEvaluationBit = showString "ShaderStageTessellationEvaluationBit"
  showsPrec _ ShaderStageGeometryBit = showString "ShaderStageGeometryBit"
  showsPrec _ ShaderStageFragmentBit = showString "ShaderStageFragmentBit"
  showsPrec _ ShaderStageComputeBit = showString "ShaderStageComputeBit"
  showsPrec _ ShaderStageAllGraphics = showString "ShaderStageAllGraphics"
  showsPrec _ ShaderStageAll = showString "ShaderStageAll"
  showsPrec p (ShaderStageFlags x) = showParen (p >= 11) (showString "ShaderStageFlags " . showsPrec 11 x)

instance Read ShaderStageFlags where
  readPrec = parens ( choose [ ("ShaderStageVertexBit", pure ShaderStageVertexBit)
                             , ("ShaderStageTessellationControlBit", pure ShaderStageTessellationControlBit)
                             , ("ShaderStageTessellationEvaluationBit", pure ShaderStageTessellationEvaluationBit)
                             , ("ShaderStageGeometryBit", pure ShaderStageGeometryBit)
                             , ("ShaderStageFragmentBit", pure ShaderStageFragmentBit)
                             , ("ShaderStageComputeBit", pure ShaderStageComputeBit)
                             , ("ShaderStageAllGraphics", pure ShaderStageAllGraphics)
                             , ("ShaderStageAll", pure ShaderStageAll)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ShaderStageFlags")
                        v <- step readPrec
                        pure (ShaderStageFlags v)
                        )
                    )


pattern ShaderStageVertexBit = ShaderStageFlags 0x1

pattern ShaderStageTessellationControlBit = ShaderStageFlags 0x2

pattern ShaderStageTessellationEvaluationBit = ShaderStageFlags 0x4

pattern ShaderStageGeometryBit = ShaderStageFlags 0x8

pattern ShaderStageFragmentBit = ShaderStageFlags 0x10

pattern ShaderStageComputeBit = ShaderStageFlags 0x20

pattern ShaderStageAllGraphics = ShaderStageFlags 0x1f

pattern ShaderStageAll = ShaderStageFlags 0x7fffffff

newtype ShaderModule = ShaderModule Word64
  deriving (Eq, Ord, Storable)

-- ** createShaderModule
foreign import ccall "vkCreateShaderModule" createShaderModule ::
  Device ->
  Ptr ShaderModuleCreateInfo ->
    Ptr AllocationCallbacks -> Ptr ShaderModule -> IO Result

