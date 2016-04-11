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
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkInternalAllocationType(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkSystemAllocationScope(..)
                             , PFN_vkFreeFunction
                             , PFN_vkInternalFreeNotification
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize
                      , CSize(..)
                      )


data VkShaderModuleCreateInfo =
  VkShaderModuleCreateInfo{ sType :: VkStructureType 
                          , pNext :: Ptr Void 
                          , flags :: VkShaderModuleCreateFlags 
                          , codeSize :: CSize 
                          , pCode :: Ptr Word32 
                          }
  deriving (Eq)

instance Storable VkShaderModuleCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkShaderModuleCreateInfo <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 24) (codeSize (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 32) (pCode (poked :: VkShaderModuleCreateInfo))


-- ** vkDestroyShaderModule
foreign import ccall "vkDestroyShaderModule" vkDestroyShaderModule ::
  Device -> ShaderModule -> Ptr VkAllocationCallbacks -> IO ()

-- ** VkShaderModuleCreateFlags
-- | Opaque flag
newtype VkShaderModuleCreateFlags = VkShaderModuleCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkShaderStageFlags

newtype VkShaderStageFlagBits = VkShaderStageFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkShaderStageFlagBits
type VkShaderStageFlags = VkShaderStageFlagBits

instance Show VkShaderStageFlagBits where
  showsPrec _ VK_SHADER_STAGE_VERTEX_BIT = showString "VK_SHADER_STAGE_VERTEX_BIT"
  showsPrec _ VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT = showString "VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT"
  showsPrec _ VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = showString "VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT"
  showsPrec _ VK_SHADER_STAGE_GEOMETRY_BIT = showString "VK_SHADER_STAGE_GEOMETRY_BIT"
  showsPrec _ VK_SHADER_STAGE_FRAGMENT_BIT = showString "VK_SHADER_STAGE_FRAGMENT_BIT"
  showsPrec _ VK_SHADER_STAGE_COMPUTE_BIT = showString "VK_SHADER_STAGE_COMPUTE_BIT"
  showsPrec _ VK_SHADER_STAGE_ALL_GRAPHICS = showString "VK_SHADER_STAGE_ALL_GRAPHICS"
  showsPrec _ VK_SHADER_STAGE_ALL = showString "VK_SHADER_STAGE_ALL"
  showsPrec p (VkShaderStageFlagBits x) = showParen (p >= 11) (showString "VkShaderStageFlagBits " . showsPrec 11 x)

instance Read VkShaderStageFlagBits where
  readPrec = parens ( choose [ ("VK_SHADER_STAGE_VERTEX_BIT", pure VK_SHADER_STAGE_VERTEX_BIT)
                             , ("VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT", pure VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT)
                             , ("VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT", pure VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT)
                             , ("VK_SHADER_STAGE_GEOMETRY_BIT", pure VK_SHADER_STAGE_GEOMETRY_BIT)
                             , ("VK_SHADER_STAGE_FRAGMENT_BIT", pure VK_SHADER_STAGE_FRAGMENT_BIT)
                             , ("VK_SHADER_STAGE_COMPUTE_BIT", pure VK_SHADER_STAGE_COMPUTE_BIT)
                             , ("VK_SHADER_STAGE_ALL_GRAPHICS", pure VK_SHADER_STAGE_ALL_GRAPHICS)
                             , ("VK_SHADER_STAGE_ALL", pure VK_SHADER_STAGE_ALL)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkShaderStageFlagBits")
                        v <- step readPrec
                        pure (VkShaderStageFlagBits v)
                        )
                    )


pattern VK_SHADER_STAGE_VERTEX_BIT = VkShaderStageFlagBits 0x1

pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT = VkShaderStageFlagBits 0x2

pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = VkShaderStageFlagBits 0x4

pattern VK_SHADER_STAGE_GEOMETRY_BIT = VkShaderStageFlagBits 0x8

pattern VK_SHADER_STAGE_FRAGMENT_BIT = VkShaderStageFlagBits 0x10

pattern VK_SHADER_STAGE_COMPUTE_BIT = VkShaderStageFlagBits 0x20

pattern VK_SHADER_STAGE_ALL_GRAPHICS = VkShaderStageFlagBits 0x1f

pattern VK_SHADER_STAGE_ALL = VkShaderStageFlagBits 0x7fffffff

newtype ShaderModule = ShaderModule Word64
  deriving (Eq, Storable)

-- ** vkCreateShaderModule
foreign import ccall "vkCreateShaderModule" vkCreateShaderModule ::
  Device ->
  Ptr VkShaderModuleCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr ShaderModule -> IO VkResult

