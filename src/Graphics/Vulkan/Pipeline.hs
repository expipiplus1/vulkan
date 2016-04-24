{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Pipeline where

import Data.Vector.Storable.Sized( Vector
                                 )
import Graphics.Vulkan.Device( VkDevice(..)
                             )
import {-# SOURCE #-} Graphics.Vulkan.Pass( VkRenderPass(..)
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
import Graphics.Vulkan.PipelineCache( VkPipelineCache(..)
                                    )
import Data.Int( Int32
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
import Graphics.Vulkan.PipelineLayout( VkPipelineLayout(..)
                                     )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Shader( VkShaderStageFlagBits(..)
                             , VkShaderModule(..)
                             )
import Graphics.Vulkan.Sampler( VkSampleCountFlagBits(..)
                              , VkCompareOp(..)
                              )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkBool32(..)
                           , VkExtent2D(..)
                           , VkFlags(..)
                           , VkFormat(..)
                           , VkOffset2D(..)
                           , VkRect2D(..)
                           , VkViewport(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize
                      , CFloat
                      , CFloat(..)
                      , CChar
                      , CSize(..)
                      )


data VkPipelineTessellationStateCreateInfo =
  VkPipelineTessellationStateCreateInfo{ vkSType :: VkStructureType 
                                       , vkPNext :: Ptr Void 
                                       , vkFlags :: VkPipelineTessellationStateCreateFlags 
                                       , vkPatchControlPoints :: Word32 
                                       }
  deriving (Eq, Ord)

instance Storable VkPipelineTessellationStateCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPipelineTessellationStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineTessellationStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineTessellationStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineTessellationStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkPatchControlPoints (poked :: VkPipelineTessellationStateCreateInfo))



data VkVertexInputAttributeDescription =
  VkVertexInputAttributeDescription{ vkLocation :: Word32 
                                   , vkBinding :: Word32 
                                   , vkFormat :: VkFormat 
                                   , vkOffset :: Word32 
                                   }
  deriving (Eq, Ord)

instance Storable VkVertexInputAttributeDescription where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkVertexInputAttributeDescription <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 4)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkLocation (poked :: VkVertexInputAttributeDescription))
                *> poke (ptr `plusPtr` 4) (vkBinding (poked :: VkVertexInputAttributeDescription))
                *> poke (ptr `plusPtr` 8) (vkFormat (poked :: VkVertexInputAttributeDescription))
                *> poke (ptr `plusPtr` 12) (vkOffset (poked :: VkVertexInputAttributeDescription))



data VkGraphicsPipelineCreateInfo =
  VkGraphicsPipelineCreateInfo{ vkSType :: VkStructureType 
                              , vkPNext :: Ptr Void 
                              , vkFlags :: VkPipelineCreateFlags 
                              , vkStageCount :: Word32 
                              , vkPStages :: Ptr VkPipelineShaderStageCreateInfo 
                              , vkPVertexInputState :: Ptr VkPipelineVertexInputStateCreateInfo 
                              , vkPInputAssemblyState :: Ptr VkPipelineInputAssemblyStateCreateInfo 
                              , vkPTessellationState :: Ptr VkPipelineTessellationStateCreateInfo 
                              , vkPViewportState :: Ptr VkPipelineViewportStateCreateInfo 
                              , vkPRasterizationState :: Ptr VkPipelineRasterizationStateCreateInfo 
                              , vkPMultisampleState :: Ptr VkPipelineMultisampleStateCreateInfo 
                              , vkPDepthStencilState :: Ptr VkPipelineDepthStencilStateCreateInfo 
                              , vkPColorBlendState :: Ptr VkPipelineColorBlendStateCreateInfo 
                              , vkPDynamicState :: Ptr VkPipelineDynamicStateCreateInfo 
                              , vkLayout :: VkPipelineLayout 
                              , vkRenderPass :: VkRenderPass 
                              , vkSubpass :: Word32 
                              , vkBasePipelineHandle :: VkPipeline 
                              , vkBasePipelineIndex :: Int32 
                              }
  deriving (Eq, Ord)

instance Storable VkGraphicsPipelineCreateInfo where
  sizeOf ~_ = 144
  alignment ~_ = 8
  peek ptr = VkGraphicsPipelineCreateInfo <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 20)
                                          <*> peek (ptr `plusPtr` 24)
                                          <*> peek (ptr `plusPtr` 32)
                                          <*> peek (ptr `plusPtr` 40)
                                          <*> peek (ptr `plusPtr` 48)
                                          <*> peek (ptr `plusPtr` 56)
                                          <*> peek (ptr `plusPtr` 64)
                                          <*> peek (ptr `plusPtr` 72)
                                          <*> peek (ptr `plusPtr` 80)
                                          <*> peek (ptr `plusPtr` 88)
                                          <*> peek (ptr `plusPtr` 96)
                                          <*> peek (ptr `plusPtr` 104)
                                          <*> peek (ptr `plusPtr` 112)
                                          <*> peek (ptr `plusPtr` 120)
                                          <*> peek (ptr `plusPtr` 128)
                                          <*> peek (ptr `plusPtr` 136)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkStageCount (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPStages (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPVertexInputState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPInputAssemblyState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkPTessellationState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPViewportState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkPRasterizationState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkPMultisampleState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 80) (vkPDepthStencilState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 88) (vkPColorBlendState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 96) (vkPDynamicState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 104) (vkLayout (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 112) (vkRenderPass (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 120) (vkSubpass (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 128) (vkBasePipelineHandle (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 136) (vkBasePipelineIndex (poked :: VkGraphicsPipelineCreateInfo))


-- ** VkCullModeFlags

newtype VkCullModeFlagBits = VkCullModeFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkCullModeFlagBits
type VkCullModeFlags = VkCullModeFlagBits

instance Show VkCullModeFlagBits where
  showsPrec _ VK_CULL_MODE_FRONT_BIT = showString "VK_CULL_MODE_FRONT_BIT"
  showsPrec _ VK_CULL_MODE_BACK_BIT = showString "VK_CULL_MODE_BACK_BIT"
  showsPrec _ VK_CULL_MODE_NONE = showString "VK_CULL_MODE_NONE"
  showsPrec _ VK_CULL_MODE_FRONT_AND_BACK = showString "VK_CULL_MODE_FRONT_AND_BACK"
  showsPrec p (VkCullModeFlagBits x) = showParen (p >= 11) (showString "VkCullModeFlagBits " . showsPrec 11 x)

instance Read VkCullModeFlagBits where
  readPrec = parens ( choose [ ("VK_CULL_MODE_FRONT_BIT", pure VK_CULL_MODE_FRONT_BIT)
                             , ("VK_CULL_MODE_BACK_BIT", pure VK_CULL_MODE_BACK_BIT)
                             , ("VK_CULL_MODE_NONE", pure VK_CULL_MODE_NONE)
                             , ("VK_CULL_MODE_FRONT_AND_BACK", pure VK_CULL_MODE_FRONT_AND_BACK)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCullModeFlagBits")
                        v <- step readPrec
                        pure (VkCullModeFlagBits v)
                        )
                    )


pattern VK_CULL_MODE_FRONT_BIT = VkCullModeFlagBits 0x1

pattern VK_CULL_MODE_BACK_BIT = VkCullModeFlagBits 0x2

pattern VK_CULL_MODE_NONE = VkCullModeFlagBits 0x0

pattern VK_CULL_MODE_FRONT_AND_BACK = VkCullModeFlagBits 0x3

-- ** VkPipelineDepthStencilStateCreateFlags
-- | Opaque flag
newtype VkPipelineDepthStencilStateCreateFlags = VkPipelineDepthStencilStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)


data VkPipelineShaderStageCreateInfo =
  VkPipelineShaderStageCreateInfo{ vkSType :: VkStructureType 
                                 , vkPNext :: Ptr Void 
                                 , vkFlags :: VkPipelineShaderStageCreateFlags 
                                 , vkStage :: VkShaderStageFlagBits 
                                 , vkModule :: VkShaderModule 
                                 , vkPName :: Ptr CChar 
                                 , vkPSpecializationInfo :: Ptr VkSpecializationInfo 
                                 }
  deriving (Eq, Ord)

instance Storable VkPipelineShaderStageCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPipelineShaderStageCreateInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 20)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 32)
                                             <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkStage (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkModule (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPName (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPSpecializationInfo (poked :: VkPipelineShaderStageCreateInfo))


-- ** VkColorComponentFlags

newtype VkColorComponentFlagBits = VkColorComponentFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkColorComponentFlagBits
type VkColorComponentFlags = VkColorComponentFlagBits

instance Show VkColorComponentFlagBits where
  showsPrec _ VK_COLOR_COMPONENT_R_BIT = showString "VK_COLOR_COMPONENT_R_BIT"
  showsPrec _ VK_COLOR_COMPONENT_G_BIT = showString "VK_COLOR_COMPONENT_G_BIT"
  showsPrec _ VK_COLOR_COMPONENT_B_BIT = showString "VK_COLOR_COMPONENT_B_BIT"
  showsPrec _ VK_COLOR_COMPONENT_A_BIT = showString "VK_COLOR_COMPONENT_A_BIT"
  
  showsPrec p (VkColorComponentFlagBits x) = showParen (p >= 11) (showString "VkColorComponentFlagBits " . showsPrec 11 x)

instance Read VkColorComponentFlagBits where
  readPrec = parens ( choose [ ("VK_COLOR_COMPONENT_R_BIT", pure VK_COLOR_COMPONENT_R_BIT)
                             , ("VK_COLOR_COMPONENT_G_BIT", pure VK_COLOR_COMPONENT_G_BIT)
                             , ("VK_COLOR_COMPONENT_B_BIT", pure VK_COLOR_COMPONENT_B_BIT)
                             , ("VK_COLOR_COMPONENT_A_BIT", pure VK_COLOR_COMPONENT_A_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkColorComponentFlagBits")
                        v <- step readPrec
                        pure (VkColorComponentFlagBits v)
                        )
                    )


pattern VK_COLOR_COMPONENT_R_BIT = VkColorComponentFlagBits 0x1

pattern VK_COLOR_COMPONENT_G_BIT = VkColorComponentFlagBits 0x2

pattern VK_COLOR_COMPONENT_B_BIT = VkColorComponentFlagBits 0x4

pattern VK_COLOR_COMPONENT_A_BIT = VkColorComponentFlagBits 0x8



data VkComputePipelineCreateInfo =
  VkComputePipelineCreateInfo{ vkSType :: VkStructureType 
                             , vkPNext :: Ptr Void 
                             , vkFlags :: VkPipelineCreateFlags 
                             , vkStage :: VkPipelineShaderStageCreateInfo 
                             , vkLayout :: VkPipelineLayout 
                             , vkBasePipelineHandle :: VkPipeline 
                             , vkBasePipelineIndex :: Int32 
                             }
  deriving (Eq, Ord)

instance Storable VkComputePipelineCreateInfo where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = VkComputePipelineCreateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 72)
                                         <*> peek (ptr `plusPtr` 80)
                                         <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkStage (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkLayout (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 80) (vkBasePipelineHandle (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 88) (vkBasePipelineIndex (poked :: VkComputePipelineCreateInfo))


-- ** VkStencilOp

newtype VkStencilOp = VkStencilOp Int32
  deriving (Eq, Ord, Storable)

instance Show VkStencilOp where
  showsPrec _ VK_STENCIL_OP_KEEP = showString "VK_STENCIL_OP_KEEP"
  showsPrec _ VK_STENCIL_OP_ZERO = showString "VK_STENCIL_OP_ZERO"
  showsPrec _ VK_STENCIL_OP_REPLACE = showString "VK_STENCIL_OP_REPLACE"
  showsPrec _ VK_STENCIL_OP_INCREMENT_AND_CLAMP = showString "VK_STENCIL_OP_INCREMENT_AND_CLAMP"
  showsPrec _ VK_STENCIL_OP_DECREMENT_AND_CLAMP = showString "VK_STENCIL_OP_DECREMENT_AND_CLAMP"
  showsPrec _ VK_STENCIL_OP_INVERT = showString "VK_STENCIL_OP_INVERT"
  showsPrec _ VK_STENCIL_OP_INCREMENT_AND_WRAP = showString "VK_STENCIL_OP_INCREMENT_AND_WRAP"
  showsPrec _ VK_STENCIL_OP_DECREMENT_AND_WRAP = showString "VK_STENCIL_OP_DECREMENT_AND_WRAP"
  showsPrec p (VkStencilOp x) = showParen (p >= 11) (showString "VkStencilOp " . showsPrec 11 x)

instance Read VkStencilOp where
  readPrec = parens ( choose [ ("VK_STENCIL_OP_KEEP", pure VK_STENCIL_OP_KEEP)
                             , ("VK_STENCIL_OP_ZERO", pure VK_STENCIL_OP_ZERO)
                             , ("VK_STENCIL_OP_REPLACE", pure VK_STENCIL_OP_REPLACE)
                             , ("VK_STENCIL_OP_INCREMENT_AND_CLAMP", pure VK_STENCIL_OP_INCREMENT_AND_CLAMP)
                             , ("VK_STENCIL_OP_DECREMENT_AND_CLAMP", pure VK_STENCIL_OP_DECREMENT_AND_CLAMP)
                             , ("VK_STENCIL_OP_INVERT", pure VK_STENCIL_OP_INVERT)
                             , ("VK_STENCIL_OP_INCREMENT_AND_WRAP", pure VK_STENCIL_OP_INCREMENT_AND_WRAP)
                             , ("VK_STENCIL_OP_DECREMENT_AND_WRAP", pure VK_STENCIL_OP_DECREMENT_AND_WRAP)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkStencilOp")
                        v <- step readPrec
                        pure (VkStencilOp v)
                        )
                    )


pattern VK_STENCIL_OP_KEEP = VkStencilOp 0

pattern VK_STENCIL_OP_ZERO = VkStencilOp 1

pattern VK_STENCIL_OP_REPLACE = VkStencilOp 2

pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP = VkStencilOp 3

pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP = VkStencilOp 4

pattern VK_STENCIL_OP_INVERT = VkStencilOp 5

pattern VK_STENCIL_OP_INCREMENT_AND_WRAP = VkStencilOp 6

pattern VK_STENCIL_OP_DECREMENT_AND_WRAP = VkStencilOp 7


data VkSpecializationInfo =
  VkSpecializationInfo{ vkMapEntryCount :: Word32 
                      , vkPMapEntries :: Ptr VkSpecializationMapEntry 
                      , vkDataSize :: CSize 
                      , vkPData :: Ptr Void 
                      }
  deriving (Eq, Ord)

instance Storable VkSpecializationInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkSpecializationInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMapEntryCount (poked :: VkSpecializationInfo))
                *> poke (ptr `plusPtr` 8) (vkPMapEntries (poked :: VkSpecializationInfo))
                *> poke (ptr `plusPtr` 16) (vkDataSize (poked :: VkSpecializationInfo))
                *> poke (ptr `plusPtr` 24) (vkPData (poked :: VkSpecializationInfo))


-- ** VkPipelineColorBlendStateCreateFlags
-- | Opaque flag
newtype VkPipelineColorBlendStateCreateFlags = VkPipelineColorBlendStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

newtype VkPipeline = VkPipeline Word64
  deriving (Eq, Ord, Storable)

-- ** VkPipelineInputAssemblyStateCreateFlags
-- | Opaque flag
newtype VkPipelineInputAssemblyStateCreateFlags = VkPipelineInputAssemblyStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- ** vkCreateGraphicsPipelines
foreign import ccall "vkCreateGraphicsPipelines" vkCreateGraphicsPipelines ::
  VkDevice ->
  VkPipelineCache ->
    Word32 ->
      Ptr VkGraphicsPipelineCreateInfo ->
        Ptr VkAllocationCallbacks -> Ptr VkPipeline -> IO VkResult

-- ** VkFrontFace

newtype VkFrontFace = VkFrontFace Int32
  deriving (Eq, Ord, Storable)

instance Show VkFrontFace where
  showsPrec _ VK_FRONT_FACE_COUNTER_CLOCKWISE = showString "VK_FRONT_FACE_COUNTER_CLOCKWISE"
  showsPrec _ VK_FRONT_FACE_CLOCKWISE = showString "VK_FRONT_FACE_CLOCKWISE"
  showsPrec p (VkFrontFace x) = showParen (p >= 11) (showString "VkFrontFace " . showsPrec 11 x)

instance Read VkFrontFace where
  readPrec = parens ( choose [ ("VK_FRONT_FACE_COUNTER_CLOCKWISE", pure VK_FRONT_FACE_COUNTER_CLOCKWISE)
                             , ("VK_FRONT_FACE_CLOCKWISE", pure VK_FRONT_FACE_CLOCKWISE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFrontFace")
                        v <- step readPrec
                        pure (VkFrontFace v)
                        )
                    )


pattern VK_FRONT_FACE_COUNTER_CLOCKWISE = VkFrontFace 0

pattern VK_FRONT_FACE_CLOCKWISE = VkFrontFace 1

-- ** VkPolygonMode

newtype VkPolygonMode = VkPolygonMode Int32
  deriving (Eq, Ord, Storable)

instance Show VkPolygonMode where
  showsPrec _ VK_POLYGON_MODE_FILL = showString "VK_POLYGON_MODE_FILL"
  showsPrec _ VK_POLYGON_MODE_LINE = showString "VK_POLYGON_MODE_LINE"
  showsPrec _ VK_POLYGON_MODE_POINT = showString "VK_POLYGON_MODE_POINT"
  showsPrec p (VkPolygonMode x) = showParen (p >= 11) (showString "VkPolygonMode " . showsPrec 11 x)

instance Read VkPolygonMode where
  readPrec = parens ( choose [ ("VK_POLYGON_MODE_FILL", pure VK_POLYGON_MODE_FILL)
                             , ("VK_POLYGON_MODE_LINE", pure VK_POLYGON_MODE_LINE)
                             , ("VK_POLYGON_MODE_POINT", pure VK_POLYGON_MODE_POINT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPolygonMode")
                        v <- step readPrec
                        pure (VkPolygonMode v)
                        )
                    )


pattern VK_POLYGON_MODE_FILL = VkPolygonMode 0

pattern VK_POLYGON_MODE_LINE = VkPolygonMode 1

pattern VK_POLYGON_MODE_POINT = VkPolygonMode 2

-- ** VkPipelineViewportStateCreateFlags
-- | Opaque flag
newtype VkPipelineViewportStateCreateFlags = VkPipelineViewportStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- ** VkLogicOp

newtype VkLogicOp = VkLogicOp Int32
  deriving (Eq, Ord, Storable)

instance Show VkLogicOp where
  showsPrec _ VK_LOGIC_OP_CLEAR = showString "VK_LOGIC_OP_CLEAR"
  showsPrec _ VK_LOGIC_OP_AND = showString "VK_LOGIC_OP_AND"
  showsPrec _ VK_LOGIC_OP_AND_REVERSE = showString "VK_LOGIC_OP_AND_REVERSE"
  showsPrec _ VK_LOGIC_OP_COPY = showString "VK_LOGIC_OP_COPY"
  showsPrec _ VK_LOGIC_OP_AND_INVERTED = showString "VK_LOGIC_OP_AND_INVERTED"
  showsPrec _ VK_LOGIC_OP_NO_OP = showString "VK_LOGIC_OP_NO_OP"
  showsPrec _ VK_LOGIC_OP_XOR = showString "VK_LOGIC_OP_XOR"
  showsPrec _ VK_LOGIC_OP_OR = showString "VK_LOGIC_OP_OR"
  showsPrec _ VK_LOGIC_OP_NOR = showString "VK_LOGIC_OP_NOR"
  showsPrec _ VK_LOGIC_OP_EQUIVALENT = showString "VK_LOGIC_OP_EQUIVALENT"
  showsPrec _ VK_LOGIC_OP_INVERT = showString "VK_LOGIC_OP_INVERT"
  showsPrec _ VK_LOGIC_OP_OR_REVERSE = showString "VK_LOGIC_OP_OR_REVERSE"
  showsPrec _ VK_LOGIC_OP_COPY_INVERTED = showString "VK_LOGIC_OP_COPY_INVERTED"
  showsPrec _ VK_LOGIC_OP_OR_INVERTED = showString "VK_LOGIC_OP_OR_INVERTED"
  showsPrec _ VK_LOGIC_OP_NAND = showString "VK_LOGIC_OP_NAND"
  showsPrec _ VK_LOGIC_OP_SET = showString "VK_LOGIC_OP_SET"
  showsPrec p (VkLogicOp x) = showParen (p >= 11) (showString "VkLogicOp " . showsPrec 11 x)

instance Read VkLogicOp where
  readPrec = parens ( choose [ ("VK_LOGIC_OP_CLEAR", pure VK_LOGIC_OP_CLEAR)
                             , ("VK_LOGIC_OP_AND", pure VK_LOGIC_OP_AND)
                             , ("VK_LOGIC_OP_AND_REVERSE", pure VK_LOGIC_OP_AND_REVERSE)
                             , ("VK_LOGIC_OP_COPY", pure VK_LOGIC_OP_COPY)
                             , ("VK_LOGIC_OP_AND_INVERTED", pure VK_LOGIC_OP_AND_INVERTED)
                             , ("VK_LOGIC_OP_NO_OP", pure VK_LOGIC_OP_NO_OP)
                             , ("VK_LOGIC_OP_XOR", pure VK_LOGIC_OP_XOR)
                             , ("VK_LOGIC_OP_OR", pure VK_LOGIC_OP_OR)
                             , ("VK_LOGIC_OP_NOR", pure VK_LOGIC_OP_NOR)
                             , ("VK_LOGIC_OP_EQUIVALENT", pure VK_LOGIC_OP_EQUIVALENT)
                             , ("VK_LOGIC_OP_INVERT", pure VK_LOGIC_OP_INVERT)
                             , ("VK_LOGIC_OP_OR_REVERSE", pure VK_LOGIC_OP_OR_REVERSE)
                             , ("VK_LOGIC_OP_COPY_INVERTED", pure VK_LOGIC_OP_COPY_INVERTED)
                             , ("VK_LOGIC_OP_OR_INVERTED", pure VK_LOGIC_OP_OR_INVERTED)
                             , ("VK_LOGIC_OP_NAND", pure VK_LOGIC_OP_NAND)
                             , ("VK_LOGIC_OP_SET", pure VK_LOGIC_OP_SET)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkLogicOp")
                        v <- step readPrec
                        pure (VkLogicOp v)
                        )
                    )


pattern VK_LOGIC_OP_CLEAR = VkLogicOp 0

pattern VK_LOGIC_OP_AND = VkLogicOp 1

pattern VK_LOGIC_OP_AND_REVERSE = VkLogicOp 2

pattern VK_LOGIC_OP_COPY = VkLogicOp 3

pattern VK_LOGIC_OP_AND_INVERTED = VkLogicOp 4

pattern VK_LOGIC_OP_NO_OP = VkLogicOp 5

pattern VK_LOGIC_OP_XOR = VkLogicOp 6

pattern VK_LOGIC_OP_OR = VkLogicOp 7

pattern VK_LOGIC_OP_NOR = VkLogicOp 8

pattern VK_LOGIC_OP_EQUIVALENT = VkLogicOp 9

pattern VK_LOGIC_OP_INVERT = VkLogicOp 10

pattern VK_LOGIC_OP_OR_REVERSE = VkLogicOp 11

pattern VK_LOGIC_OP_COPY_INVERTED = VkLogicOp 12

pattern VK_LOGIC_OP_OR_INVERTED = VkLogicOp 13

pattern VK_LOGIC_OP_NAND = VkLogicOp 14

pattern VK_LOGIC_OP_SET = VkLogicOp 15

-- ** VkPipelineCreateFlags

newtype VkPipelineCreateFlagBits = VkPipelineCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkPipelineCreateFlagBits
type VkPipelineCreateFlags = VkPipelineCreateFlagBits

instance Show VkPipelineCreateFlagBits where
  showsPrec _ VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = showString "VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
  showsPrec _ VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = showString "VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
  showsPrec _ VK_PIPELINE_CREATE_DERIVATIVE_BIT = showString "VK_PIPELINE_CREATE_DERIVATIVE_BIT"
  
  showsPrec p (VkPipelineCreateFlagBits x) = showParen (p >= 11) (showString "VkPipelineCreateFlagBits " . showsPrec 11 x)

instance Read VkPipelineCreateFlagBits where
  readPrec = parens ( choose [ ("VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT", pure VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT)
                             , ("VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT", pure VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT)
                             , ("VK_PIPELINE_CREATE_DERIVATIVE_BIT", pure VK_PIPELINE_CREATE_DERIVATIVE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineCreateFlagBits")
                        v <- step readPrec
                        pure (VkPipelineCreateFlagBits v)
                        )
                    )


pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = VkPipelineCreateFlagBits 0x1

pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = VkPipelineCreateFlagBits 0x2

pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT = VkPipelineCreateFlagBits 0x4


-- ** VkPipelineRasterizationStateCreateFlags
-- | Opaque flag
newtype VkPipelineRasterizationStateCreateFlags = VkPipelineRasterizationStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- ** VkDynamicState

newtype VkDynamicState = VkDynamicState Int32
  deriving (Eq, Ord, Storable)

instance Show VkDynamicState where
  showsPrec _ VK_DYNAMIC_STATE_VIEWPORT = showString "VK_DYNAMIC_STATE_VIEWPORT"
  showsPrec _ VK_DYNAMIC_STATE_SCISSOR = showString "VK_DYNAMIC_STATE_SCISSOR"
  showsPrec _ VK_DYNAMIC_STATE_LINE_WIDTH = showString "VK_DYNAMIC_STATE_LINE_WIDTH"
  showsPrec _ VK_DYNAMIC_STATE_DEPTH_BIAS = showString "VK_DYNAMIC_STATE_DEPTH_BIAS"
  showsPrec _ VK_DYNAMIC_STATE_BLEND_CONSTANTS = showString "VK_DYNAMIC_STATE_BLEND_CONSTANTS"
  showsPrec _ VK_DYNAMIC_STATE_DEPTH_BOUNDS = showString "VK_DYNAMIC_STATE_DEPTH_BOUNDS"
  showsPrec _ VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK = showString "VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK"
  showsPrec _ VK_DYNAMIC_STATE_STENCIL_WRITE_MASK = showString "VK_DYNAMIC_STATE_STENCIL_WRITE_MASK"
  showsPrec _ VK_DYNAMIC_STATE_STENCIL_REFERENCE = showString "VK_DYNAMIC_STATE_STENCIL_REFERENCE"
  showsPrec p (VkDynamicState x) = showParen (p >= 11) (showString "VkDynamicState " . showsPrec 11 x)

instance Read VkDynamicState where
  readPrec = parens ( choose [ ("VK_DYNAMIC_STATE_VIEWPORT", pure VK_DYNAMIC_STATE_VIEWPORT)
                             , ("VK_DYNAMIC_STATE_SCISSOR", pure VK_DYNAMIC_STATE_SCISSOR)
                             , ("VK_DYNAMIC_STATE_LINE_WIDTH", pure VK_DYNAMIC_STATE_LINE_WIDTH)
                             , ("VK_DYNAMIC_STATE_DEPTH_BIAS", pure VK_DYNAMIC_STATE_DEPTH_BIAS)
                             , ("VK_DYNAMIC_STATE_BLEND_CONSTANTS", pure VK_DYNAMIC_STATE_BLEND_CONSTANTS)
                             , ("VK_DYNAMIC_STATE_DEPTH_BOUNDS", pure VK_DYNAMIC_STATE_DEPTH_BOUNDS)
                             , ("VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK", pure VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK)
                             , ("VK_DYNAMIC_STATE_STENCIL_WRITE_MASK", pure VK_DYNAMIC_STATE_STENCIL_WRITE_MASK)
                             , ("VK_DYNAMIC_STATE_STENCIL_REFERENCE", pure VK_DYNAMIC_STATE_STENCIL_REFERENCE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDynamicState")
                        v <- step readPrec
                        pure (VkDynamicState v)
                        )
                    )


pattern VK_DYNAMIC_STATE_VIEWPORT = VkDynamicState 0

pattern VK_DYNAMIC_STATE_SCISSOR = VkDynamicState 1

pattern VK_DYNAMIC_STATE_LINE_WIDTH = VkDynamicState 2

pattern VK_DYNAMIC_STATE_DEPTH_BIAS = VkDynamicState 3

pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS = VkDynamicState 4

pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS = VkDynamicState 5

pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK = VkDynamicState 6

pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK = VkDynamicState 7

pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE = VkDynamicState 8

-- ** VkPipelineBindPoint

newtype VkPipelineBindPoint = VkPipelineBindPoint Int32
  deriving (Eq, Ord, Storable)

instance Show VkPipelineBindPoint where
  showsPrec _ VK_PIPELINE_BIND_POINT_GRAPHICS = showString "VK_PIPELINE_BIND_POINT_GRAPHICS"
  showsPrec _ VK_PIPELINE_BIND_POINT_COMPUTE = showString "VK_PIPELINE_BIND_POINT_COMPUTE"
  showsPrec p (VkPipelineBindPoint x) = showParen (p >= 11) (showString "VkPipelineBindPoint " . showsPrec 11 x)

instance Read VkPipelineBindPoint where
  readPrec = parens ( choose [ ("VK_PIPELINE_BIND_POINT_GRAPHICS", pure VK_PIPELINE_BIND_POINT_GRAPHICS)
                             , ("VK_PIPELINE_BIND_POINT_COMPUTE", pure VK_PIPELINE_BIND_POINT_COMPUTE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineBindPoint")
                        v <- step readPrec
                        pure (VkPipelineBindPoint v)
                        )
                    )


pattern VK_PIPELINE_BIND_POINT_GRAPHICS = VkPipelineBindPoint 0

pattern VK_PIPELINE_BIND_POINT_COMPUTE = VkPipelineBindPoint 1

-- ** VkPipelineDynamicStateCreateFlags
-- | Opaque flag
newtype VkPipelineDynamicStateCreateFlags = VkPipelineDynamicStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)


data VkPipelineRasterizationStateCreateInfo =
  VkPipelineRasterizationStateCreateInfo{ vkSType :: VkStructureType 
                                        , vkPNext :: Ptr Void 
                                        , vkFlags :: VkPipelineRasterizationStateCreateFlags 
                                        , vkDepthClampEnable :: VkBool32 
                                        , vkRasterizerDiscardEnable :: VkBool32 
                                        , vkPolygonMode :: VkPolygonMode 
                                        , vkCullMode :: VkCullModeFlags 
                                        , vkFrontFace :: VkFrontFace 
                                        , vkDepthBiasEnable :: VkBool32 
                                        , vkDepthBiasConstantFactor :: CFloat 
                                        , vkDepthBiasClamp :: CFloat 
                                        , vkDepthBiasSlopeFactor :: CFloat 
                                        , vkLineWidth :: CFloat 
                                        }
  deriving (Eq, Ord)

instance Storable VkPipelineRasterizationStateCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkPipelineRasterizationStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
                                                    <*> peek (ptr `plusPtr` 24)
                                                    <*> peek (ptr `plusPtr` 28)
                                                    <*> peek (ptr `plusPtr` 32)
                                                    <*> peek (ptr `plusPtr` 36)
                                                    <*> peek (ptr `plusPtr` 40)
                                                    <*> peek (ptr `plusPtr` 44)
                                                    <*> peek (ptr `plusPtr` 48)
                                                    <*> peek (ptr `plusPtr` 52)
                                                    <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkDepthClampEnable (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkRasterizerDiscardEnable (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkPolygonMode (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkCullMode (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkFrontFace (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkDepthBiasEnable (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkDepthBiasConstantFactor (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkDepthBiasClamp (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkDepthBiasSlopeFactor (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkLineWidth (poked :: VkPipelineRasterizationStateCreateInfo))


-- ** VkBlendOp

newtype VkBlendOp = VkBlendOp Int32
  deriving (Eq, Ord, Storable)

instance Show VkBlendOp where
  showsPrec _ VK_BLEND_OP_ADD = showString "VK_BLEND_OP_ADD"
  showsPrec _ VK_BLEND_OP_SUBTRACT = showString "VK_BLEND_OP_SUBTRACT"
  showsPrec _ VK_BLEND_OP_REVERSE_SUBTRACT = showString "VK_BLEND_OP_REVERSE_SUBTRACT"
  showsPrec _ VK_BLEND_OP_MIN = showString "VK_BLEND_OP_MIN"
  showsPrec _ VK_BLEND_OP_MAX = showString "VK_BLEND_OP_MAX"
  showsPrec p (VkBlendOp x) = showParen (p >= 11) (showString "VkBlendOp " . showsPrec 11 x)

instance Read VkBlendOp where
  readPrec = parens ( choose [ ("VK_BLEND_OP_ADD", pure VK_BLEND_OP_ADD)
                             , ("VK_BLEND_OP_SUBTRACT", pure VK_BLEND_OP_SUBTRACT)
                             , ("VK_BLEND_OP_REVERSE_SUBTRACT", pure VK_BLEND_OP_REVERSE_SUBTRACT)
                             , ("VK_BLEND_OP_MIN", pure VK_BLEND_OP_MIN)
                             , ("VK_BLEND_OP_MAX", pure VK_BLEND_OP_MAX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBlendOp")
                        v <- step readPrec
                        pure (VkBlendOp v)
                        )
                    )


pattern VK_BLEND_OP_ADD = VkBlendOp 0

pattern VK_BLEND_OP_SUBTRACT = VkBlendOp 1

pattern VK_BLEND_OP_REVERSE_SUBTRACT = VkBlendOp 2

pattern VK_BLEND_OP_MIN = VkBlendOp 3

pattern VK_BLEND_OP_MAX = VkBlendOp 4

-- ** vkDestroyPipeline
foreign import ccall "vkDestroyPipeline" vkDestroyPipeline ::
  VkDevice -> VkPipeline -> Ptr VkAllocationCallbacks -> IO ()

-- ** VkPipelineShaderStageCreateFlags
-- | Opaque flag
newtype VkPipelineShaderStageCreateFlags = VkPipelineShaderStageCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)


data VkPipelineViewportStateCreateInfo =
  VkPipelineViewportStateCreateInfo{ vkSType :: VkStructureType 
                                   , vkPNext :: Ptr Void 
                                   , vkFlags :: VkPipelineViewportStateCreateFlags 
                                   , vkViewportCount :: Word32 
                                   , vkPViewports :: Ptr VkViewport 
                                   , vkScissorCount :: Word32 
                                   , vkPScissors :: Ptr VkRect2D 
                                   }
  deriving (Eq, Ord)

instance Storable VkPipelineViewportStateCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPipelineViewportStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
                                               <*> peek (ptr `plusPtr` 20)
                                               <*> peek (ptr `plusPtr` 24)
                                               <*> peek (ptr `plusPtr` 32)
                                               <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkViewportCount (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPViewports (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkScissorCount (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPScissors (poked :: VkPipelineViewportStateCreateInfo))


-- ** VkPipelineTessellationStateCreateFlags
-- | Opaque flag
newtype VkPipelineTessellationStateCreateFlags = VkPipelineTessellationStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)


data VkPipelineVertexInputStateCreateInfo =
  VkPipelineVertexInputStateCreateInfo{ vkSType :: VkStructureType 
                                      , vkPNext :: Ptr Void 
                                      , vkFlags :: VkPipelineVertexInputStateCreateFlags 
                                      , vkVertexBindingDescriptionCount :: Word32 
                                      , vkPVertexBindingDescriptions :: Ptr VkVertexInputBindingDescription 
                                      , vkVertexAttributeDescriptionCount :: Word32 
                                      , vkPVertexAttributeDescriptions :: Ptr VkVertexInputAttributeDescription 
                                      }
  deriving (Eq, Ord)

instance Storable VkPipelineVertexInputStateCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPipelineVertexInputStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 20)
                                                  <*> peek (ptr `plusPtr` 24)
                                                  <*> peek (ptr `plusPtr` 32)
                                                  <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkVertexBindingDescriptionCount (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPVertexBindingDescriptions (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkVertexAttributeDescriptionCount (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPVertexAttributeDescriptions (poked :: VkPipelineVertexInputStateCreateInfo))


-- ** VkPrimitiveTopology

newtype VkPrimitiveTopology = VkPrimitiveTopology Int32
  deriving (Eq, Ord, Storable)

instance Show VkPrimitiveTopology where
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_POINT_LIST = showString "VK_PRIMITIVE_TOPOLOGY_POINT_LIST"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_LIST = showString "VK_PRIMITIVE_TOPOLOGY_LINE_LIST"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_STRIP = showString "VK_PRIMITIVE_TOPOLOGY_LINE_STRIP"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY = showString "VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY = showString "VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_PATCH_LIST = showString "VK_PRIMITIVE_TOPOLOGY_PATCH_LIST"
  showsPrec p (VkPrimitiveTopology x) = showParen (p >= 11) (showString "VkPrimitiveTopology " . showsPrec 11 x)

instance Read VkPrimitiveTopology where
  readPrec = parens ( choose [ ("VK_PRIMITIVE_TOPOLOGY_POINT_LIST", pure VK_PRIMITIVE_TOPOLOGY_POINT_LIST)
                             , ("VK_PRIMITIVE_TOPOLOGY_LINE_LIST", pure VK_PRIMITIVE_TOPOLOGY_LINE_LIST)
                             , ("VK_PRIMITIVE_TOPOLOGY_LINE_STRIP", pure VK_PRIMITIVE_TOPOLOGY_LINE_STRIP)
                             , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST", pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)
                             , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP", pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP)
                             , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN", pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN)
                             , ("VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY", pure VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY)
                             , ("VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY", pure VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY)
                             , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY", pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY)
                             , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY", pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY)
                             , ("VK_PRIMITIVE_TOPOLOGY_PATCH_LIST", pure VK_PRIMITIVE_TOPOLOGY_PATCH_LIST)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPrimitiveTopology")
                        v <- step readPrec
                        pure (VkPrimitiveTopology v)
                        )
                    )


pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST = VkPrimitiveTopology 0

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST = VkPrimitiveTopology 1

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP = VkPrimitiveTopology 2

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = VkPrimitiveTopology 3

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP = VkPrimitiveTopology 4

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN = VkPrimitiveTopology 5

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY = VkPrimitiveTopology 6

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY = VkPrimitiveTopology 7

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY = VkPrimitiveTopology 8

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY = VkPrimitiveTopology 9

pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST = VkPrimitiveTopology 10


data VkPipelineInputAssemblyStateCreateInfo =
  VkPipelineInputAssemblyStateCreateInfo{ vkSType :: VkStructureType 
                                        , vkPNext :: Ptr Void 
                                        , vkFlags :: VkPipelineInputAssemblyStateCreateFlags 
                                        , vkTopology :: VkPrimitiveTopology 
                                        , vkPrimitiveRestartEnable :: VkBool32 
                                        }
  deriving (Eq, Ord)

instance Storable VkPipelineInputAssemblyStateCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineInputAssemblyStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
                                                    <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkTopology (poked :: VkPipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPrimitiveRestartEnable (poked :: VkPipelineInputAssemblyStateCreateInfo))



data VkPipelineColorBlendStateCreateInfo =
  VkPipelineColorBlendStateCreateInfo{ vkSType :: VkStructureType 
                                     , vkPNext :: Ptr Void 
                                     , vkFlags :: VkPipelineColorBlendStateCreateFlags 
                                     , vkLogicOpEnable :: VkBool32 
                                     , vkLogicOp :: VkLogicOp 
                                     , vkAttachmentCount :: Word32 
                                     , vkPAttachments :: Ptr VkPipelineColorBlendAttachmentState 
                                     , vkBlendConstants :: Vector 4 CFloat 
                                     }
  deriving (Eq, Ord)

instance Storable VkPipelineColorBlendStateCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkPipelineColorBlendStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 28)
                                                 <*> peek (ptr `plusPtr` 32)
                                                 <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkLogicOpEnable (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkLogicOp (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkAttachmentCount (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPAttachments (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkBlendConstants (poked :: VkPipelineColorBlendStateCreateInfo))



data VkPipelineDynamicStateCreateInfo =
  VkPipelineDynamicStateCreateInfo{ vkSType :: VkStructureType 
                                  , vkPNext :: Ptr Void 
                                  , vkFlags :: VkPipelineDynamicStateCreateFlags 
                                  , vkDynamicStateCount :: Word32 
                                  , vkPDynamicStates :: Ptr VkDynamicState 
                                  }
  deriving (Eq, Ord)

instance Storable VkPipelineDynamicStateCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineDynamicStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 20)
                                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkDynamicStateCount (poked :: VkPipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPDynamicStates (poked :: VkPipelineDynamicStateCreateInfo))



data VkSpecializationMapEntry =
  VkSpecializationMapEntry{ vkConstantID :: Word32 
                          , vkOffset :: Word32 
                          , vkSize :: CSize 
                          }
  deriving (Eq, Ord)

instance Storable VkSpecializationMapEntry where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkSpecializationMapEntry <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkConstantID (poked :: VkSpecializationMapEntry))
                *> poke (ptr `plusPtr` 4) (vkOffset (poked :: VkSpecializationMapEntry))
                *> poke (ptr `plusPtr` 8) (vkSize (poked :: VkSpecializationMapEntry))


-- ** VkPipelineVertexInputStateCreateFlags
-- | Opaque flag
newtype VkPipelineVertexInputStateCreateFlags = VkPipelineVertexInputStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- ** VkVertexInputRate

newtype VkVertexInputRate = VkVertexInputRate Int32
  deriving (Eq, Ord, Storable)

instance Show VkVertexInputRate where
  showsPrec _ VK_VERTEX_INPUT_RATE_VERTEX = showString "VK_VERTEX_INPUT_RATE_VERTEX"
  showsPrec _ VK_VERTEX_INPUT_RATE_INSTANCE = showString "VK_VERTEX_INPUT_RATE_INSTANCE"
  showsPrec p (VkVertexInputRate x) = showParen (p >= 11) (showString "VkVertexInputRate " . showsPrec 11 x)

instance Read VkVertexInputRate where
  readPrec = parens ( choose [ ("VK_VERTEX_INPUT_RATE_VERTEX", pure VK_VERTEX_INPUT_RATE_VERTEX)
                             , ("VK_VERTEX_INPUT_RATE_INSTANCE", pure VK_VERTEX_INPUT_RATE_INSTANCE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkVertexInputRate")
                        v <- step readPrec
                        pure (VkVertexInputRate v)
                        )
                    )


pattern VK_VERTEX_INPUT_RATE_VERTEX = VkVertexInputRate 0

pattern VK_VERTEX_INPUT_RATE_INSTANCE = VkVertexInputRate 1

-- ** VkPipelineStageFlags

newtype VkPipelineStageFlagBits = VkPipelineStageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkPipelineStageFlagBits
type VkPipelineStageFlags = VkPipelineStageFlagBits

instance Show VkPipelineStageFlagBits where
  showsPrec _ VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT = showString "VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT"
  showsPrec _ VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT = showString "VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT"
  showsPrec _ VK_PIPELINE_STAGE_VERTEX_INPUT_BIT = showString "VK_PIPELINE_STAGE_VERTEX_INPUT_BIT"
  showsPrec _ VK_PIPELINE_STAGE_VERTEX_SHADER_BIT = showString "VK_PIPELINE_STAGE_VERTEX_SHADER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT = showString "VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = showString "VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT = showString "VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT = showString "VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = showString "VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT"
  showsPrec _ VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = showString "VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT"
  showsPrec _ VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = showString "VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT"
  showsPrec _ VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT = showString "VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_TRANSFER_BIT = showString "VK_PIPELINE_STAGE_TRANSFER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = showString "VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT"
  showsPrec _ VK_PIPELINE_STAGE_HOST_BIT = showString "VK_PIPELINE_STAGE_HOST_BIT"
  showsPrec _ VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT = showString "VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT"
  showsPrec _ VK_PIPELINE_STAGE_ALL_COMMANDS_BIT = showString "VK_PIPELINE_STAGE_ALL_COMMANDS_BIT"
  
  showsPrec p (VkPipelineStageFlagBits x) = showParen (p >= 11) (showString "VkPipelineStageFlagBits " . showsPrec 11 x)

instance Read VkPipelineStageFlagBits where
  readPrec = parens ( choose [ ("VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT", pure VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT)
                             , ("VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT", pure VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT)
                             , ("VK_PIPELINE_STAGE_VERTEX_INPUT_BIT", pure VK_PIPELINE_STAGE_VERTEX_INPUT_BIT)
                             , ("VK_PIPELINE_STAGE_VERTEX_SHADER_BIT", pure VK_PIPELINE_STAGE_VERTEX_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT", pure VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT", pure VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT", pure VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT", pure VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT", pure VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT)
                             , ("VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT", pure VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT)
                             , ("VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT", pure VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
                             , ("VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT", pure VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_TRANSFER_BIT", pure VK_PIPELINE_STAGE_TRANSFER_BIT)
                             , ("VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT", pure VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT)
                             , ("VK_PIPELINE_STAGE_HOST_BIT", pure VK_PIPELINE_STAGE_HOST_BIT)
                             , ("VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT", pure VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT)
                             , ("VK_PIPELINE_STAGE_ALL_COMMANDS_BIT", pure VK_PIPELINE_STAGE_ALL_COMMANDS_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineStageFlagBits")
                        v <- step readPrec
                        pure (VkPipelineStageFlagBits v)
                        )
                    )

-- | Before subsequent commands are processed
pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT = VkPipelineStageFlagBits 0x1
-- | Draw/DispatchIndirect command fetch
pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT = VkPipelineStageFlagBits 0x2
-- | Vertex/index fetch
pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT = VkPipelineStageFlagBits 0x4
-- | Vertex shading
pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT = VkPipelineStageFlagBits 0x8
-- | Tessellation control shading
pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT = VkPipelineStageFlagBits 0x10
-- | Tessellation evaluation shading
pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = VkPipelineStageFlagBits 0x20
-- | Geometry shading
pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT = VkPipelineStageFlagBits 0x40
-- | Fragment shading
pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT = VkPipelineStageFlagBits 0x80
-- | Early fragment (depth and stencil) tests
pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = VkPipelineStageFlagBits 0x100
-- | Late fragment (depth and stencil) tests
pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = VkPipelineStageFlagBits 0x200
-- | Color attachment writes
pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = VkPipelineStageFlagBits 0x400
-- | Compute shading
pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT = VkPipelineStageFlagBits 0x800
-- | Transfer/copy operations
pattern VK_PIPELINE_STAGE_TRANSFER_BIT = VkPipelineStageFlagBits 0x1000
-- | After previous commands have completed
pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = VkPipelineStageFlagBits 0x2000
-- | Indicates host (CPU) is a source/sink of the dependency
pattern VK_PIPELINE_STAGE_HOST_BIT = VkPipelineStageFlagBits 0x4000
-- | All stages of the graphics pipeline
pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT = VkPipelineStageFlagBits 0x8000
-- | All stages supported on the queue
pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT = VkPipelineStageFlagBits 0x10000



data VkPipelineColorBlendAttachmentState =
  VkPipelineColorBlendAttachmentState{ vkBlendEnable :: VkBool32 
                                     , vkSrcColorBlendFactor :: VkBlendFactor 
                                     , vkDstColorBlendFactor :: VkBlendFactor 
                                     , vkColorBlendOp :: VkBlendOp 
                                     , vkSrcAlphaBlendFactor :: VkBlendFactor 
                                     , vkDstAlphaBlendFactor :: VkBlendFactor 
                                     , vkAlphaBlendOp :: VkBlendOp 
                                     , vkColorWriteMask :: VkColorComponentFlags 
                                     }
  deriving (Eq, Ord)

instance Storable VkPipelineColorBlendAttachmentState where
  sizeOf ~_ = 32
  alignment ~_ = 4
  peek ptr = VkPipelineColorBlendAttachmentState <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 4)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 12)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBlendEnable (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 4) (vkSrcColorBlendFactor (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 8) (vkDstColorBlendFactor (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 12) (vkColorBlendOp (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 16) (vkSrcAlphaBlendFactor (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 20) (vkDstAlphaBlendFactor (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 24) (vkAlphaBlendOp (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 28) (vkColorWriteMask (poked :: VkPipelineColorBlendAttachmentState))


-- ** VkBlendFactor

newtype VkBlendFactor = VkBlendFactor Int32
  deriving (Eq, Ord, Storable)

instance Show VkBlendFactor where
  showsPrec _ VK_BLEND_FACTOR_ZERO = showString "VK_BLEND_FACTOR_ZERO"
  showsPrec _ VK_BLEND_FACTOR_ONE = showString "VK_BLEND_FACTOR_ONE"
  showsPrec _ VK_BLEND_FACTOR_SRC_COLOR = showString "VK_BLEND_FACTOR_SRC_COLOR"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR"
  showsPrec _ VK_BLEND_FACTOR_DST_COLOR = showString "VK_BLEND_FACTOR_DST_COLOR"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR = showString "VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR"
  showsPrec _ VK_BLEND_FACTOR_SRC_ALPHA = showString "VK_BLEND_FACTOR_SRC_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_DST_ALPHA = showString "VK_BLEND_FACTOR_DST_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA = showString "VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_CONSTANT_COLOR = showString "VK_BLEND_FACTOR_CONSTANT_COLOR"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = showString "VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR"
  showsPrec _ VK_BLEND_FACTOR_CONSTANT_ALPHA = showString "VK_BLEND_FACTOR_CONSTANT_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = showString "VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_SRC_ALPHA_SATURATE = showString "VK_BLEND_FACTOR_SRC_ALPHA_SATURATE"
  showsPrec _ VK_BLEND_FACTOR_SRC1_COLOR = showString "VK_BLEND_FACTOR_SRC1_COLOR"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR"
  showsPrec _ VK_BLEND_FACTOR_SRC1_ALPHA = showString "VK_BLEND_FACTOR_SRC1_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA"
  showsPrec p (VkBlendFactor x) = showParen (p >= 11) (showString "VkBlendFactor " . showsPrec 11 x)

instance Read VkBlendFactor where
  readPrec = parens ( choose [ ("VK_BLEND_FACTOR_ZERO", pure VK_BLEND_FACTOR_ZERO)
                             , ("VK_BLEND_FACTOR_ONE", pure VK_BLEND_FACTOR_ONE)
                             , ("VK_BLEND_FACTOR_SRC_COLOR", pure VK_BLEND_FACTOR_SRC_COLOR)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR", pure VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR)
                             , ("VK_BLEND_FACTOR_DST_COLOR", pure VK_BLEND_FACTOR_DST_COLOR)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR", pure VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR)
                             , ("VK_BLEND_FACTOR_SRC_ALPHA", pure VK_BLEND_FACTOR_SRC_ALPHA)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA", pure VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA)
                             , ("VK_BLEND_FACTOR_DST_ALPHA", pure VK_BLEND_FACTOR_DST_ALPHA)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA", pure VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA)
                             , ("VK_BLEND_FACTOR_CONSTANT_COLOR", pure VK_BLEND_FACTOR_CONSTANT_COLOR)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR", pure VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR)
                             , ("VK_BLEND_FACTOR_CONSTANT_ALPHA", pure VK_BLEND_FACTOR_CONSTANT_ALPHA)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA", pure VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA)
                             , ("VK_BLEND_FACTOR_SRC_ALPHA_SATURATE", pure VK_BLEND_FACTOR_SRC_ALPHA_SATURATE)
                             , ("VK_BLEND_FACTOR_SRC1_COLOR", pure VK_BLEND_FACTOR_SRC1_COLOR)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR", pure VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR)
                             , ("VK_BLEND_FACTOR_SRC1_ALPHA", pure VK_BLEND_FACTOR_SRC1_ALPHA)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA", pure VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBlendFactor")
                        v <- step readPrec
                        pure (VkBlendFactor v)
                        )
                    )


pattern VK_BLEND_FACTOR_ZERO = VkBlendFactor 0

pattern VK_BLEND_FACTOR_ONE = VkBlendFactor 1

pattern VK_BLEND_FACTOR_SRC_COLOR = VkBlendFactor 2

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR = VkBlendFactor 3

pattern VK_BLEND_FACTOR_DST_COLOR = VkBlendFactor 4

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR = VkBlendFactor 5

pattern VK_BLEND_FACTOR_SRC_ALPHA = VkBlendFactor 6

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = VkBlendFactor 7

pattern VK_BLEND_FACTOR_DST_ALPHA = VkBlendFactor 8

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA = VkBlendFactor 9

pattern VK_BLEND_FACTOR_CONSTANT_COLOR = VkBlendFactor 10

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = VkBlendFactor 11

pattern VK_BLEND_FACTOR_CONSTANT_ALPHA = VkBlendFactor 12

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = VkBlendFactor 13

pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE = VkBlendFactor 14

pattern VK_BLEND_FACTOR_SRC1_COLOR = VkBlendFactor 15

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR = VkBlendFactor 16

pattern VK_BLEND_FACTOR_SRC1_ALPHA = VkBlendFactor 17

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA = VkBlendFactor 18

newtype VkSampleMask = VkSampleMask Word32
  deriving (Eq, Ord, Storable)

-- ** VkPipelineMultisampleStateCreateFlags
-- | Opaque flag
newtype VkPipelineMultisampleStateCreateFlags = VkPipelineMultisampleStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)


data VkPipelineMultisampleStateCreateInfo =
  VkPipelineMultisampleStateCreateInfo{ vkSType :: VkStructureType 
                                      , vkPNext :: Ptr Void 
                                      , vkFlags :: VkPipelineMultisampleStateCreateFlags 
                                      , vkRasterizationSamples :: VkSampleCountFlagBits 
                                      , vkSampleShadingEnable :: VkBool32 
                                      , vkMinSampleShading :: CFloat 
                                      , vkPSampleMask :: Ptr VkSampleMask 
                                      , vkAlphaToCoverageEnable :: VkBool32 
                                      , vkAlphaToOneEnable :: VkBool32 
                                      }
  deriving (Eq, Ord)

instance Storable VkPipelineMultisampleStateCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPipelineMultisampleStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 20)
                                                  <*> peek (ptr `plusPtr` 24)
                                                  <*> peek (ptr `plusPtr` 28)
                                                  <*> peek (ptr `plusPtr` 32)
                                                  <*> peek (ptr `plusPtr` 40)
                                                  <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkRasterizationSamples (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkSampleShadingEnable (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkMinSampleShading (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPSampleMask (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkAlphaToCoverageEnable (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkAlphaToOneEnable (poked :: VkPipelineMultisampleStateCreateInfo))



data VkVertexInputBindingDescription =
  VkVertexInputBindingDescription{ vkBinding :: Word32 
                                 , vkStride :: Word32 
                                 , vkInputRate :: VkVertexInputRate 
                                 }
  deriving (Eq, Ord)

instance Storable VkVertexInputBindingDescription where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkVertexInputBindingDescription <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 4)
                                             <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBinding (poked :: VkVertexInputBindingDescription))
                *> poke (ptr `plusPtr` 4) (vkStride (poked :: VkVertexInputBindingDescription))
                *> poke (ptr `plusPtr` 8) (vkInputRate (poked :: VkVertexInputBindingDescription))



data VkPipelineDepthStencilStateCreateInfo =
  VkPipelineDepthStencilStateCreateInfo{ vkSType :: VkStructureType 
                                       , vkPNext :: Ptr Void 
                                       , vkFlags :: VkPipelineDepthStencilStateCreateFlags 
                                       , vkDepthTestEnable :: VkBool32 
                                       , vkDepthWriteEnable :: VkBool32 
                                       , vkDepthCompareOp :: VkCompareOp 
                                       , vkDepthBoundsTestEnable :: VkBool32 
                                       , vkStencilTestEnable :: VkBool32 
                                       , vkFront :: VkStencilOpState 
                                       , vkBack :: VkStencilOpState 
                                       , vkMinDepthBounds :: CFloat 
                                       , vkMaxDepthBounds :: CFloat 
                                       }
  deriving (Eq, Ord)

instance Storable VkPipelineDepthStencilStateCreateInfo where
  sizeOf ~_ = 104
  alignment ~_ = 8
  peek ptr = VkPipelineDepthStencilStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 20)
                                                   <*> peek (ptr `plusPtr` 24)
                                                   <*> peek (ptr `plusPtr` 28)
                                                   <*> peek (ptr `plusPtr` 32)
                                                   <*> peek (ptr `plusPtr` 36)
                                                   <*> peek (ptr `plusPtr` 40)
                                                   <*> peek (ptr `plusPtr` 68)
                                                   <*> peek (ptr `plusPtr` 96)
                                                   <*> peek (ptr `plusPtr` 100)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkDepthTestEnable (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkDepthWriteEnable (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkDepthCompareOp (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkDepthBoundsTestEnable (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkStencilTestEnable (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkFront (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 68) (vkBack (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 96) (vkMinDepthBounds (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 100) (vkMaxDepthBounds (poked :: VkPipelineDepthStencilStateCreateInfo))


-- ** vkCreateComputePipelines
foreign import ccall "vkCreateComputePipelines" vkCreateComputePipelines ::
  VkDevice ->
  VkPipelineCache ->
    Word32 ->
      Ptr VkComputePipelineCreateInfo ->
        Ptr VkAllocationCallbacks -> Ptr VkPipeline -> IO VkResult


data VkStencilOpState =
  VkStencilOpState{ vkFailOp :: VkStencilOp 
                  , vkPassOp :: VkStencilOp 
                  , vkDepthFailOp :: VkStencilOp 
                  , vkCompareOp :: VkCompareOp 
                  , vkCompareMask :: Word32 
                  , vkWriteMask :: Word32 
                  , vkReference :: Word32 
                  }
  deriving (Eq, Ord)

instance Storable VkStencilOpState where
  sizeOf ~_ = 28
  alignment ~_ = 4
  peek ptr = VkStencilOpState <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 4)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 12)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 20)
                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFailOp (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 4) (vkPassOp (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 8) (vkDepthFailOp (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 12) (vkCompareOp (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 16) (vkCompareMask (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 20) (vkWriteMask (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 24) (vkReference (poked :: VkStencilOpState))


