{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Pipeline where

import Data.Vector.Storable.Sized( Vector(..)
                                 )
import Graphics.Vulkan.Device( Device(..)
                             )
import {-# SOURCE #-} Graphics.Vulkan.Pass( RenderPass(..)
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
import Graphics.Vulkan.PipelineCache( PipelineCache(..)
                                    )
import Data.Int( Int32(..)
               , Int32
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
import Graphics.Vulkan.PipelineLayout( PipelineLayout(..)
                                     )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Shader( ShaderStageFlags(..)
                             , ShaderModule(..)
                             )
import Graphics.Vulkan.Sampler( CompareOp(..)
                              , SampleCountFlags(..)
                              )
import Graphics.Vulkan.Core( Bool32(..)
                           , StructureType(..)
                           , Viewport(..)
                           , Rect2D(..)
                           , Format(..)
                           , Result(..)
                           , Flags(..)
                           )
import Foreign.C.Types( CFloat(..)
                      , CChar(..)
                      , CSize(..)
                      )


data PipelineTessellationStateCreateInfo =
  PipelineTessellationStateCreateInfo{ sType :: StructureType 
                                     , pNext :: Ptr Void 
                                     , flags :: PipelineTessellationStateCreateFlags 
                                     , patchControlPoints :: Word32 
                                     }
  deriving (Eq)

instance Storable PipelineTessellationStateCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = PipelineTessellationStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PipelineTessellationStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PipelineTessellationStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: PipelineTessellationStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (patchControlPoints (poked :: PipelineTessellationStateCreateInfo))



data VertexInputAttributeDescription =
  VertexInputAttributeDescription{ location :: Word32 
                                 , binding :: Word32 
                                 , format :: Format 
                                 , offset :: Word32 
                                 }
  deriving (Eq)

instance Storable VertexInputAttributeDescription where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VertexInputAttributeDescription <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 4)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (location (poked :: VertexInputAttributeDescription))
                *> poke (ptr `plusPtr` 4) (binding (poked :: VertexInputAttributeDescription))
                *> poke (ptr `plusPtr` 8) (format (poked :: VertexInputAttributeDescription))
                *> poke (ptr `plusPtr` 12) (offset (poked :: VertexInputAttributeDescription))



data GraphicsPipelineCreateInfo =
  GraphicsPipelineCreateInfo{ sType :: StructureType 
                            , pNext :: Ptr Void 
                            , flags :: PipelineCreateFlags 
                            , stageCount :: Word32 
                            , pStages :: Ptr PipelineShaderStageCreateInfo 
                            , pVertexInputState :: Ptr PipelineVertexInputStateCreateInfo 
                            , pInputAssemblyState :: Ptr PipelineInputAssemblyStateCreateInfo 
                            , pTessellationState :: Ptr PipelineTessellationStateCreateInfo 
                            , pViewportState :: Ptr PipelineViewportStateCreateInfo 
                            , pRasterizationState :: Ptr PipelineRasterizationStateCreateInfo 
                            , pMultisampleState :: Ptr PipelineMultisampleStateCreateInfo 
                            , pDepthStencilState :: Ptr PipelineDepthStencilStateCreateInfo 
                            , pColorBlendState :: Ptr PipelineColorBlendStateCreateInfo 
                            , pDynamicState :: Ptr PipelineDynamicStateCreateInfo 
                            , layout :: PipelineLayout 
                            , renderPass :: RenderPass 
                            , subpass :: Word32 
                            , basePipelineHandle :: Pipeline 
                            , basePipelineIndex :: Int32 
                            }
  deriving (Eq)

instance Storable GraphicsPipelineCreateInfo where
  sizeOf ~_ = 144
  alignment ~_ = 8
  peek ptr = GraphicsPipelineCreateInfo <$> peek (ptr `plusPtr` 0)
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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 20) (stageCount (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 24) (pStages (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 32) (pVertexInputState (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 40) (pInputAssemblyState (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 48) (pTessellationState (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 56) (pViewportState (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 64) (pRasterizationState (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 72) (pMultisampleState (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 80) (pDepthStencilState (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 88) (pColorBlendState (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 96) (pDynamicState (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 104) (layout (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 112) (renderPass (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 120) (subpass (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 128) (basePipelineHandle (poked :: GraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 136) (basePipelineIndex (poked :: GraphicsPipelineCreateInfo))


-- ** VkCullModeFlags

newtype CullModeFlags = CullModeFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show CullModeFlags where
  showsPrec _ VK_CULL_MODE_FRONT_BIT = showString "VK_CULL_MODE_FRONT_BIT"
  showsPrec _ VK_CULL_MODE_BACK_BIT = showString "VK_CULL_MODE_BACK_BIT"
  showsPrec _ VK_CULL_MODE_NONE = showString "VK_CULL_MODE_NONE"
  showsPrec _ VK_CULL_MODE_FRONT_AND_BACK = showString "VK_CULL_MODE_FRONT_AND_BACK"
  showsPrec p (CullModeFlags x) = showParen (p >= 11) (showString "CullModeFlags " . showsPrec 11 x)

instance Read CullModeFlags where
  readPrec = parens ( choose [ ("VK_CULL_MODE_FRONT_BIT", pure VK_CULL_MODE_FRONT_BIT)
                             , ("VK_CULL_MODE_BACK_BIT", pure VK_CULL_MODE_BACK_BIT)
                             , ("VK_CULL_MODE_NONE", pure VK_CULL_MODE_NONE)
                             , ("VK_CULL_MODE_FRONT_AND_BACK", pure VK_CULL_MODE_FRONT_AND_BACK)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CullModeFlags")
                        v <- step readPrec
                        pure (CullModeFlags v)
                        )
                    )


pattern VK_CULL_MODE_FRONT_BIT = CullModeFlags 0x1

pattern VK_CULL_MODE_BACK_BIT = CullModeFlags 0x2

pattern VK_CULL_MODE_NONE = CullModeFlags 0x0

pattern VK_CULL_MODE_FRONT_AND_BACK = CullModeFlags 0x3

-- ** PipelineDepthStencilStateCreateFlags
-- | Opaque flag
newtype PipelineDepthStencilStateCreateFlags = PipelineDepthStencilStateCreateFlags Flags
  deriving (Eq, Storable)


data PipelineShaderStageCreateInfo =
  PipelineShaderStageCreateInfo{ sType :: StructureType 
                               , pNext :: Ptr Void 
                               , flags :: PipelineShaderStageCreateFlags 
                               , stage :: ShaderStageFlags 
                               , _module :: ShaderModule 
                               , pName :: Ptr CChar 
                               , pSpecializationInfo :: Ptr SpecializationInfo 
                               }
  deriving (Eq)

instance Storable PipelineShaderStageCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = PipelineShaderStageCreateInfo <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 20)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 32)
                                           <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: PipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 20) (stage (poked :: PipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 24) (_module (poked :: PipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 32) (pName (poked :: PipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 40) (pSpecializationInfo (poked :: PipelineShaderStageCreateInfo))


-- ** VkColorComponentFlags

newtype ColorComponentFlags = ColorComponentFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show ColorComponentFlags where
  showsPrec _ VK_COLOR_COMPONENT_R_BIT = showString "VK_COLOR_COMPONENT_R_BIT"
  showsPrec _ VK_COLOR_COMPONENT_G_BIT = showString "VK_COLOR_COMPONENT_G_BIT"
  showsPrec _ VK_COLOR_COMPONENT_B_BIT = showString "VK_COLOR_COMPONENT_B_BIT"
  showsPrec _ VK_COLOR_COMPONENT_A_BIT = showString "VK_COLOR_COMPONENT_A_BIT"
  
  showsPrec p (ColorComponentFlags x) = showParen (p >= 11) (showString "ColorComponentFlags " . showsPrec 11 x)

instance Read ColorComponentFlags where
  readPrec = parens ( choose [ ("VK_COLOR_COMPONENT_R_BIT", pure VK_COLOR_COMPONENT_R_BIT)
                             , ("VK_COLOR_COMPONENT_G_BIT", pure VK_COLOR_COMPONENT_G_BIT)
                             , ("VK_COLOR_COMPONENT_B_BIT", pure VK_COLOR_COMPONENT_B_BIT)
                             , ("VK_COLOR_COMPONENT_A_BIT", pure VK_COLOR_COMPONENT_A_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ColorComponentFlags")
                        v <- step readPrec
                        pure (ColorComponentFlags v)
                        )
                    )


pattern VK_COLOR_COMPONENT_R_BIT = ColorComponentFlags 0x1

pattern VK_COLOR_COMPONENT_G_BIT = ColorComponentFlags 0x2

pattern VK_COLOR_COMPONENT_B_BIT = ColorComponentFlags 0x4

pattern VK_COLOR_COMPONENT_A_BIT = ColorComponentFlags 0x8



data ComputePipelineCreateInfo =
  ComputePipelineCreateInfo{ sType :: StructureType 
                           , pNext :: Ptr Void 
                           , flags :: PipelineCreateFlags 
                           , stage :: PipelineShaderStageCreateInfo 
                           , layout :: PipelineLayout 
                           , basePipelineHandle :: Pipeline 
                           , basePipelineIndex :: Int32 
                           }
  deriving (Eq)

instance Storable ComputePipelineCreateInfo where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = ComputePipelineCreateInfo <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 72)
                                       <*> peek (ptr `plusPtr` 80)
                                       <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: ComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: ComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: ComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 24) (stage (poked :: ComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 72) (layout (poked :: ComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 80) (basePipelineHandle (poked :: ComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 88) (basePipelineIndex (poked :: ComputePipelineCreateInfo))


-- ** StencilOp

newtype StencilOp = StencilOp Int32
  deriving (Eq, Storable)

instance Show StencilOp where
  showsPrec _ VK_STENCIL_OP_KEEP = showString "VK_STENCIL_OP_KEEP"
  showsPrec _ VK_STENCIL_OP_ZERO = showString "VK_STENCIL_OP_ZERO"
  showsPrec _ VK_STENCIL_OP_REPLACE = showString "VK_STENCIL_OP_REPLACE"
  showsPrec _ VK_STENCIL_OP_INCREMENT_AND_CLAMP = showString "VK_STENCIL_OP_INCREMENT_AND_CLAMP"
  showsPrec _ VK_STENCIL_OP_DECREMENT_AND_CLAMP = showString "VK_STENCIL_OP_DECREMENT_AND_CLAMP"
  showsPrec _ VK_STENCIL_OP_INVERT = showString "VK_STENCIL_OP_INVERT"
  showsPrec _ VK_STENCIL_OP_INCREMENT_AND_WRAP = showString "VK_STENCIL_OP_INCREMENT_AND_WRAP"
  showsPrec _ VK_STENCIL_OP_DECREMENT_AND_WRAP = showString "VK_STENCIL_OP_DECREMENT_AND_WRAP"
  showsPrec p (StencilOp x) = showParen (p >= 11) (showString "StencilOp " . showsPrec 11 x)

instance Read StencilOp where
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
                        expectP (Ident "StencilOp")
                        v <- step readPrec
                        pure (StencilOp v)
                        )
                    )


pattern VK_STENCIL_OP_KEEP = StencilOp 0

pattern VK_STENCIL_OP_ZERO = StencilOp 1

pattern VK_STENCIL_OP_REPLACE = StencilOp 2

pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP = StencilOp 3

pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP = StencilOp 4

pattern VK_STENCIL_OP_INVERT = StencilOp 5

pattern VK_STENCIL_OP_INCREMENT_AND_WRAP = StencilOp 6

pattern VK_STENCIL_OP_DECREMENT_AND_WRAP = StencilOp 7


data SpecializationInfo =
  SpecializationInfo{ mapEntryCount :: Word32 
                    , pMapEntries :: Ptr SpecializationMapEntry 
                    , dataSize :: CSize 
                    , pData :: Ptr Void 
                    }
  deriving (Eq)

instance Storable SpecializationInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = SpecializationInfo <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (mapEntryCount (poked :: SpecializationInfo))
                *> poke (ptr `plusPtr` 8) (pMapEntries (poked :: SpecializationInfo))
                *> poke (ptr `plusPtr` 16) (dataSize (poked :: SpecializationInfo))
                *> poke (ptr `plusPtr` 24) (pData (poked :: SpecializationInfo))


-- ** PipelineColorBlendStateCreateFlags
-- | Opaque flag
newtype PipelineColorBlendStateCreateFlags = PipelineColorBlendStateCreateFlags Flags
  deriving (Eq, Storable)

newtype Pipeline = Pipeline Word64
  deriving (Eq, Storable)

-- ** PipelineInputAssemblyStateCreateFlags
-- | Opaque flag
newtype PipelineInputAssemblyStateCreateFlags = PipelineInputAssemblyStateCreateFlags Flags
  deriving (Eq, Storable)

-- ** vkCreateGraphicsPipelines
foreign import ccall "vkCreateGraphicsPipelines" vkCreateGraphicsPipelines ::
  Device ->
  PipelineCache ->
    Word32 ->
      Ptr GraphicsPipelineCreateInfo ->
        Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- ** FrontFace

newtype FrontFace = FrontFace Int32
  deriving (Eq, Storable)

instance Show FrontFace where
  showsPrec _ VK_FRONT_FACE_COUNTER_CLOCKWISE = showString "VK_FRONT_FACE_COUNTER_CLOCKWISE"
  showsPrec _ VK_FRONT_FACE_CLOCKWISE = showString "VK_FRONT_FACE_CLOCKWISE"
  showsPrec p (FrontFace x) = showParen (p >= 11) (showString "FrontFace " . showsPrec 11 x)

instance Read FrontFace where
  readPrec = parens ( choose [ ("VK_FRONT_FACE_COUNTER_CLOCKWISE", pure VK_FRONT_FACE_COUNTER_CLOCKWISE)
                             , ("VK_FRONT_FACE_CLOCKWISE", pure VK_FRONT_FACE_CLOCKWISE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "FrontFace")
                        v <- step readPrec
                        pure (FrontFace v)
                        )
                    )


pattern VK_FRONT_FACE_COUNTER_CLOCKWISE = FrontFace 0

pattern VK_FRONT_FACE_CLOCKWISE = FrontFace 1

-- ** PolygonMode

newtype PolygonMode = PolygonMode Int32
  deriving (Eq, Storable)

instance Show PolygonMode where
  showsPrec _ VK_POLYGON_MODE_FILL = showString "VK_POLYGON_MODE_FILL"
  showsPrec _ VK_POLYGON_MODE_LINE = showString "VK_POLYGON_MODE_LINE"
  showsPrec _ VK_POLYGON_MODE_POINT = showString "VK_POLYGON_MODE_POINT"
  showsPrec p (PolygonMode x) = showParen (p >= 11) (showString "PolygonMode " . showsPrec 11 x)

instance Read PolygonMode where
  readPrec = parens ( choose [ ("VK_POLYGON_MODE_FILL", pure VK_POLYGON_MODE_FILL)
                             , ("VK_POLYGON_MODE_LINE", pure VK_POLYGON_MODE_LINE)
                             , ("VK_POLYGON_MODE_POINT", pure VK_POLYGON_MODE_POINT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PolygonMode")
                        v <- step readPrec
                        pure (PolygonMode v)
                        )
                    )


pattern VK_POLYGON_MODE_FILL = PolygonMode 0

pattern VK_POLYGON_MODE_LINE = PolygonMode 1

pattern VK_POLYGON_MODE_POINT = PolygonMode 2

-- ** PipelineViewportStateCreateFlags
-- | Opaque flag
newtype PipelineViewportStateCreateFlags = PipelineViewportStateCreateFlags Flags
  deriving (Eq, Storable)

-- ** LogicOp

newtype LogicOp = LogicOp Int32
  deriving (Eq, Storable)

instance Show LogicOp where
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
  showsPrec p (LogicOp x) = showParen (p >= 11) (showString "LogicOp " . showsPrec 11 x)

instance Read LogicOp where
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
                        expectP (Ident "LogicOp")
                        v <- step readPrec
                        pure (LogicOp v)
                        )
                    )


pattern VK_LOGIC_OP_CLEAR = LogicOp 0

pattern VK_LOGIC_OP_AND = LogicOp 1

pattern VK_LOGIC_OP_AND_REVERSE = LogicOp 2

pattern VK_LOGIC_OP_COPY = LogicOp 3

pattern VK_LOGIC_OP_AND_INVERTED = LogicOp 4

pattern VK_LOGIC_OP_NO_OP = LogicOp 5

pattern VK_LOGIC_OP_XOR = LogicOp 6

pattern VK_LOGIC_OP_OR = LogicOp 7

pattern VK_LOGIC_OP_NOR = LogicOp 8

pattern VK_LOGIC_OP_EQUIVALENT = LogicOp 9

pattern VK_LOGIC_OP_INVERT = LogicOp 10

pattern VK_LOGIC_OP_OR_REVERSE = LogicOp 11

pattern VK_LOGIC_OP_COPY_INVERTED = LogicOp 12

pattern VK_LOGIC_OP_OR_INVERTED = LogicOp 13

pattern VK_LOGIC_OP_NAND = LogicOp 14

pattern VK_LOGIC_OP_SET = LogicOp 15

-- ** VkPipelineCreateFlags

newtype PipelineCreateFlags = PipelineCreateFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show PipelineCreateFlags where
  showsPrec _ VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = showString "VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
  showsPrec _ VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = showString "VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
  showsPrec _ VK_PIPELINE_CREATE_DERIVATIVE_BIT = showString "VK_PIPELINE_CREATE_DERIVATIVE_BIT"
  
  showsPrec p (PipelineCreateFlags x) = showParen (p >= 11) (showString "PipelineCreateFlags " . showsPrec 11 x)

instance Read PipelineCreateFlags where
  readPrec = parens ( choose [ ("VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT", pure VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT)
                             , ("VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT", pure VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT)
                             , ("VK_PIPELINE_CREATE_DERIVATIVE_BIT", pure VK_PIPELINE_CREATE_DERIVATIVE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PipelineCreateFlags")
                        v <- step readPrec
                        pure (PipelineCreateFlags v)
                        )
                    )


pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = PipelineCreateFlags 0x1

pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = PipelineCreateFlags 0x2

pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT = PipelineCreateFlags 0x4


-- ** PipelineRasterizationStateCreateFlags
-- | Opaque flag
newtype PipelineRasterizationStateCreateFlags = PipelineRasterizationStateCreateFlags Flags
  deriving (Eq, Storable)

-- ** DynamicState

newtype DynamicState = DynamicState Int32
  deriving (Eq, Storable)

instance Show DynamicState where
  showsPrec _ VK_DYNAMIC_STATE_VIEWPORT = showString "VK_DYNAMIC_STATE_VIEWPORT"
  showsPrec _ VK_DYNAMIC_STATE_SCISSOR = showString "VK_DYNAMIC_STATE_SCISSOR"
  showsPrec _ VK_DYNAMIC_STATE_LINE_WIDTH = showString "VK_DYNAMIC_STATE_LINE_WIDTH"
  showsPrec _ VK_DYNAMIC_STATE_DEPTH_BIAS = showString "VK_DYNAMIC_STATE_DEPTH_BIAS"
  showsPrec _ VK_DYNAMIC_STATE_BLEND_CONSTANTS = showString "VK_DYNAMIC_STATE_BLEND_CONSTANTS"
  showsPrec _ VK_DYNAMIC_STATE_DEPTH_BOUNDS = showString "VK_DYNAMIC_STATE_DEPTH_BOUNDS"
  showsPrec _ VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK = showString "VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK"
  showsPrec _ VK_DYNAMIC_STATE_STENCIL_WRITE_MASK = showString "VK_DYNAMIC_STATE_STENCIL_WRITE_MASK"
  showsPrec _ VK_DYNAMIC_STATE_STENCIL_REFERENCE = showString "VK_DYNAMIC_STATE_STENCIL_REFERENCE"
  showsPrec p (DynamicState x) = showParen (p >= 11) (showString "DynamicState " . showsPrec 11 x)

instance Read DynamicState where
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
                        expectP (Ident "DynamicState")
                        v <- step readPrec
                        pure (DynamicState v)
                        )
                    )


pattern VK_DYNAMIC_STATE_VIEWPORT = DynamicState 0

pattern VK_DYNAMIC_STATE_SCISSOR = DynamicState 1

pattern VK_DYNAMIC_STATE_LINE_WIDTH = DynamicState 2

pattern VK_DYNAMIC_STATE_DEPTH_BIAS = DynamicState 3

pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS = DynamicState 4

pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS = DynamicState 5

pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK = DynamicState 6

pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK = DynamicState 7

pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE = DynamicState 8

-- ** PipelineBindPoint

newtype PipelineBindPoint = PipelineBindPoint Int32
  deriving (Eq, Storable)

instance Show PipelineBindPoint where
  showsPrec _ VK_PIPELINE_BIND_POINT_GRAPHICS = showString "VK_PIPELINE_BIND_POINT_GRAPHICS"
  showsPrec _ VK_PIPELINE_BIND_POINT_COMPUTE = showString "VK_PIPELINE_BIND_POINT_COMPUTE"
  showsPrec p (PipelineBindPoint x) = showParen (p >= 11) (showString "PipelineBindPoint " . showsPrec 11 x)

instance Read PipelineBindPoint where
  readPrec = parens ( choose [ ("VK_PIPELINE_BIND_POINT_GRAPHICS", pure VK_PIPELINE_BIND_POINT_GRAPHICS)
                             , ("VK_PIPELINE_BIND_POINT_COMPUTE", pure VK_PIPELINE_BIND_POINT_COMPUTE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PipelineBindPoint")
                        v <- step readPrec
                        pure (PipelineBindPoint v)
                        )
                    )


pattern VK_PIPELINE_BIND_POINT_GRAPHICS = PipelineBindPoint 0

pattern VK_PIPELINE_BIND_POINT_COMPUTE = PipelineBindPoint 1

-- ** PipelineDynamicStateCreateFlags
-- | Opaque flag
newtype PipelineDynamicStateCreateFlags = PipelineDynamicStateCreateFlags Flags
  deriving (Eq, Storable)


data PipelineRasterizationStateCreateInfo =
  PipelineRasterizationStateCreateInfo{ sType :: StructureType 
                                      , pNext :: Ptr Void 
                                      , flags :: PipelineRasterizationStateCreateFlags 
                                      , depthClampEnable :: Bool32 
                                      , rasterizerDiscardEnable :: Bool32 
                                      , polygonMode :: PolygonMode 
                                      , cullMode :: CullModeFlags 
                                      , frontFace :: FrontFace 
                                      , depthBiasEnable :: Bool32 
                                      , depthBiasConstantFactor :: CFloat 
                                      , depthBiasClamp :: CFloat 
                                      , depthBiasSlopeFactor :: CFloat 
                                      , lineWidth :: CFloat 
                                      }
  deriving (Eq)

instance Storable PipelineRasterizationStateCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = PipelineRasterizationStateCreateInfo <$> peek (ptr `plusPtr` 0)
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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: PipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (depthClampEnable (poked :: PipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (rasterizerDiscardEnable (poked :: PipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (polygonMode (poked :: PipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (cullMode (poked :: PipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 36) (frontFace (poked :: PipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (depthBiasEnable (poked :: PipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 44) (depthBiasConstantFactor (poked :: PipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 48) (depthBiasClamp (poked :: PipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 52) (depthBiasSlopeFactor (poked :: PipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 56) (lineWidth (poked :: PipelineRasterizationStateCreateInfo))


-- ** BlendOp

newtype BlendOp = BlendOp Int32
  deriving (Eq, Storable)

instance Show BlendOp where
  showsPrec _ VK_BLEND_OP_ADD = showString "VK_BLEND_OP_ADD"
  showsPrec _ VK_BLEND_OP_SUBTRACT = showString "VK_BLEND_OP_SUBTRACT"
  showsPrec _ VK_BLEND_OP_REVERSE_SUBTRACT = showString "VK_BLEND_OP_REVERSE_SUBTRACT"
  showsPrec _ VK_BLEND_OP_MIN = showString "VK_BLEND_OP_MIN"
  showsPrec _ VK_BLEND_OP_MAX = showString "VK_BLEND_OP_MAX"
  showsPrec p (BlendOp x) = showParen (p >= 11) (showString "BlendOp " . showsPrec 11 x)

instance Read BlendOp where
  readPrec = parens ( choose [ ("VK_BLEND_OP_ADD", pure VK_BLEND_OP_ADD)
                             , ("VK_BLEND_OP_SUBTRACT", pure VK_BLEND_OP_SUBTRACT)
                             , ("VK_BLEND_OP_REVERSE_SUBTRACT", pure VK_BLEND_OP_REVERSE_SUBTRACT)
                             , ("VK_BLEND_OP_MIN", pure VK_BLEND_OP_MIN)
                             , ("VK_BLEND_OP_MAX", pure VK_BLEND_OP_MAX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "BlendOp")
                        v <- step readPrec
                        pure (BlendOp v)
                        )
                    )


pattern VK_BLEND_OP_ADD = BlendOp 0

pattern VK_BLEND_OP_SUBTRACT = BlendOp 1

pattern VK_BLEND_OP_REVERSE_SUBTRACT = BlendOp 2

pattern VK_BLEND_OP_MIN = BlendOp 3

pattern VK_BLEND_OP_MAX = BlendOp 4

-- ** vkDestroyPipeline
foreign import ccall "vkDestroyPipeline" vkDestroyPipeline ::
  Device -> Pipeline -> Ptr AllocationCallbacks -> IO ()

-- ** PipelineShaderStageCreateFlags
-- | Opaque flag
newtype PipelineShaderStageCreateFlags = PipelineShaderStageCreateFlags Flags
  deriving (Eq, Storable)


data PipelineViewportStateCreateInfo =
  PipelineViewportStateCreateInfo{ sType :: StructureType 
                                 , pNext :: Ptr Void 
                                 , flags :: PipelineViewportStateCreateFlags 
                                 , viewportCount :: Word32 
                                 , pViewports :: Ptr Viewport 
                                 , scissorCount :: Word32 
                                 , pScissors :: Ptr Rect2D 
                                 }
  deriving (Eq)

instance Storable PipelineViewportStateCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = PipelineViewportStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 20)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 32)
                                             <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: PipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (viewportCount (poked :: PipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (pViewports (poked :: PipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (scissorCount (poked :: PipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (pScissors (poked :: PipelineViewportStateCreateInfo))


-- ** PipelineTessellationStateCreateFlags
-- | Opaque flag
newtype PipelineTessellationStateCreateFlags = PipelineTessellationStateCreateFlags Flags
  deriving (Eq, Storable)


data PipelineVertexInputStateCreateInfo =
  PipelineVertexInputStateCreateInfo{ sType :: StructureType 
                                    , pNext :: Ptr Void 
                                    , flags :: PipelineVertexInputStateCreateFlags 
                                    , vertexBindingDescriptionCount :: Word32 
                                    , pVertexBindingDescriptions :: Ptr VertexInputBindingDescription 
                                    , vertexAttributeDescriptionCount :: Word32 
                                    , pVertexAttributeDescriptions :: Ptr VertexInputAttributeDescription 
                                    }
  deriving (Eq)

instance Storable PipelineVertexInputStateCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = PipelineVertexInputStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 20)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 32)
                                                <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: PipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vertexBindingDescriptionCount (poked :: PipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (pVertexBindingDescriptions (poked :: PipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vertexAttributeDescriptionCount (poked :: PipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (pVertexAttributeDescriptions (poked :: PipelineVertexInputStateCreateInfo))


-- ** PrimitiveTopology

newtype PrimitiveTopology = PrimitiveTopology Int32
  deriving (Eq, Storable)

instance Show PrimitiveTopology where
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
  showsPrec p (PrimitiveTopology x) = showParen (p >= 11) (showString "PrimitiveTopology " . showsPrec 11 x)

instance Read PrimitiveTopology where
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
                        expectP (Ident "PrimitiveTopology")
                        v <- step readPrec
                        pure (PrimitiveTopology v)
                        )
                    )


pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST = PrimitiveTopology 0

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST = PrimitiveTopology 1

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP = PrimitiveTopology 2

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = PrimitiveTopology 3

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP = PrimitiveTopology 4

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN = PrimitiveTopology 5

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY = PrimitiveTopology 6

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY = PrimitiveTopology 7

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY = PrimitiveTopology 8

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY = PrimitiveTopology 9

pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST = PrimitiveTopology 10


data PipelineInputAssemblyStateCreateInfo =
  PipelineInputAssemblyStateCreateInfo{ sType :: StructureType 
                                      , pNext :: Ptr Void 
                                      , flags :: PipelineInputAssemblyStateCreateFlags 
                                      , topology :: PrimitiveTopology 
                                      , primitiveRestartEnable :: Bool32 
                                      }
  deriving (Eq)

instance Storable PipelineInputAssemblyStateCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = PipelineInputAssemblyStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 20)
                                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: PipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (topology (poked :: PipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (primitiveRestartEnable (poked :: PipelineInputAssemblyStateCreateInfo))



data PipelineColorBlendStateCreateInfo =
  PipelineColorBlendStateCreateInfo{ sType :: StructureType 
                                   , pNext :: Ptr Void 
                                   , flags :: PipelineColorBlendStateCreateFlags 
                                   , logicOpEnable :: Bool32 
                                   , logicOp :: LogicOp 
                                   , attachmentCount :: Word32 
                                   , pAttachments :: Ptr PipelineColorBlendAttachmentState 
                                   , blendConstants :: Vector 4 CFloat 
                                   }
  deriving (Eq)

instance Storable PipelineColorBlendStateCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = PipelineColorBlendStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
                                               <*> peek (ptr `plusPtr` 20)
                                               <*> peek (ptr `plusPtr` 24)
                                               <*> peek (ptr `plusPtr` 28)
                                               <*> peek (ptr `plusPtr` 32)
                                               <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: PipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (logicOpEnable (poked :: PipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (logicOp (poked :: PipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (attachmentCount (poked :: PipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (pAttachments (poked :: PipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (blendConstants (poked :: PipelineColorBlendStateCreateInfo))



data PipelineDynamicStateCreateInfo =
  PipelineDynamicStateCreateInfo{ sType :: StructureType 
                                , pNext :: Ptr Void 
                                , flags :: PipelineDynamicStateCreateFlags 
                                , dynamicStateCount :: Word32 
                                , pDynamicStates :: Ptr DynamicState 
                                }
  deriving (Eq)

instance Storable PipelineDynamicStateCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = PipelineDynamicStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 20)
                                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: PipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (dynamicStateCount (poked :: PipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (pDynamicStates (poked :: PipelineDynamicStateCreateInfo))



data SpecializationMapEntry =
  SpecializationMapEntry{ constantID :: Word32 
                        , offset :: Word32 
                        , size :: CSize 
                        }
  deriving (Eq)

instance Storable SpecializationMapEntry where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = SpecializationMapEntry <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 4)
                                    <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (constantID (poked :: SpecializationMapEntry))
                *> poke (ptr `plusPtr` 4) (offset (poked :: SpecializationMapEntry))
                *> poke (ptr `plusPtr` 8) (size (poked :: SpecializationMapEntry))


-- ** PipelineVertexInputStateCreateFlags
-- | Opaque flag
newtype PipelineVertexInputStateCreateFlags = PipelineVertexInputStateCreateFlags Flags
  deriving (Eq, Storable)

-- ** VertexInputRate

newtype VertexInputRate = VertexInputRate Int32
  deriving (Eq, Storable)

instance Show VertexInputRate where
  showsPrec _ VK_VERTEX_INPUT_RATE_VERTEX = showString "VK_VERTEX_INPUT_RATE_VERTEX"
  showsPrec _ VK_VERTEX_INPUT_RATE_INSTANCE = showString "VK_VERTEX_INPUT_RATE_INSTANCE"
  showsPrec p (VertexInputRate x) = showParen (p >= 11) (showString "VertexInputRate " . showsPrec 11 x)

instance Read VertexInputRate where
  readPrec = parens ( choose [ ("VK_VERTEX_INPUT_RATE_VERTEX", pure VK_VERTEX_INPUT_RATE_VERTEX)
                             , ("VK_VERTEX_INPUT_RATE_INSTANCE", pure VK_VERTEX_INPUT_RATE_INSTANCE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VertexInputRate")
                        v <- step readPrec
                        pure (VertexInputRate v)
                        )
                    )


pattern VK_VERTEX_INPUT_RATE_VERTEX = VertexInputRate 0

pattern VK_VERTEX_INPUT_RATE_INSTANCE = VertexInputRate 1

-- ** VkPipelineStageFlags

newtype PipelineStageFlags = PipelineStageFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show PipelineStageFlags where
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
  
  showsPrec p (PipelineStageFlags x) = showParen (p >= 11) (showString "PipelineStageFlags " . showsPrec 11 x)

instance Read PipelineStageFlags where
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
                        expectP (Ident "PipelineStageFlags")
                        v <- step readPrec
                        pure (PipelineStageFlags v)
                        )
                    )

-- | Before subsequent commands are processed
pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT = PipelineStageFlags 0x1
-- | Draw/DispatchIndirect command fetch
pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT = PipelineStageFlags 0x2
-- | Vertex/index fetch
pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT = PipelineStageFlags 0x4
-- | Vertex shading
pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT = PipelineStageFlags 0x8
-- | Tessellation control shading
pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT = PipelineStageFlags 0x10
-- | Tessellation evaluation shading
pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = PipelineStageFlags 0x20
-- | Geometry shading
pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT = PipelineStageFlags 0x40
-- | Fragment shading
pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT = PipelineStageFlags 0x80
-- | Early fragment (depth and stencil) tests
pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = PipelineStageFlags 0x100
-- | Late fragment (depth and stencil) tests
pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = PipelineStageFlags 0x200
-- | Color attachment writes
pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = PipelineStageFlags 0x400
-- | Compute shading
pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT = PipelineStageFlags 0x800
-- | Transfer/copy operations
pattern VK_PIPELINE_STAGE_TRANSFER_BIT = PipelineStageFlags 0x1000
-- | After previous commands have completed
pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = PipelineStageFlags 0x2000
-- | Indicates host (CPU) is a source/sink of the dependency
pattern VK_PIPELINE_STAGE_HOST_BIT = PipelineStageFlags 0x4000
-- | All stages of the graphics pipeline
pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT = PipelineStageFlags 0x8000
-- | All stages supported on the queue
pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT = PipelineStageFlags 0x10000



data PipelineColorBlendAttachmentState =
  PipelineColorBlendAttachmentState{ blendEnable :: Bool32 
                                   , srcColorBlendFactor :: BlendFactor 
                                   , dstColorBlendFactor :: BlendFactor 
                                   , colorBlendOp :: BlendOp 
                                   , srcAlphaBlendFactor :: BlendFactor 
                                   , dstAlphaBlendFactor :: BlendFactor 
                                   , alphaBlendOp :: BlendOp 
                                   , colorWriteMask :: ColorComponentFlags 
                                   }
  deriving (Eq)

instance Storable PipelineColorBlendAttachmentState where
  sizeOf ~_ = 32
  alignment ~_ = 4
  peek ptr = PipelineColorBlendAttachmentState <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 4)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 12)
                                               <*> peek (ptr `plusPtr` 16)
                                               <*> peek (ptr `plusPtr` 20)
                                               <*> peek (ptr `plusPtr` 24)
                                               <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (blendEnable (poked :: PipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 4) (srcColorBlendFactor (poked :: PipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 8) (dstColorBlendFactor (poked :: PipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 12) (colorBlendOp (poked :: PipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 16) (srcAlphaBlendFactor (poked :: PipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 20) (dstAlphaBlendFactor (poked :: PipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 24) (alphaBlendOp (poked :: PipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 28) (colorWriteMask (poked :: PipelineColorBlendAttachmentState))


-- ** BlendFactor

newtype BlendFactor = BlendFactor Int32
  deriving (Eq, Storable)

instance Show BlendFactor where
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
  showsPrec p (BlendFactor x) = showParen (p >= 11) (showString "BlendFactor " . showsPrec 11 x)

instance Read BlendFactor where
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
                        expectP (Ident "BlendFactor")
                        v <- step readPrec
                        pure (BlendFactor v)
                        )
                    )


pattern VK_BLEND_FACTOR_ZERO = BlendFactor 0

pattern VK_BLEND_FACTOR_ONE = BlendFactor 1

pattern VK_BLEND_FACTOR_SRC_COLOR = BlendFactor 2

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR = BlendFactor 3

pattern VK_BLEND_FACTOR_DST_COLOR = BlendFactor 4

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR = BlendFactor 5

pattern VK_BLEND_FACTOR_SRC_ALPHA = BlendFactor 6

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = BlendFactor 7

pattern VK_BLEND_FACTOR_DST_ALPHA = BlendFactor 8

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA = BlendFactor 9

pattern VK_BLEND_FACTOR_CONSTANT_COLOR = BlendFactor 10

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = BlendFactor 11

pattern VK_BLEND_FACTOR_CONSTANT_ALPHA = BlendFactor 12

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = BlendFactor 13

pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE = BlendFactor 14

pattern VK_BLEND_FACTOR_SRC1_COLOR = BlendFactor 15

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR = BlendFactor 16

pattern VK_BLEND_FACTOR_SRC1_ALPHA = BlendFactor 17

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA = BlendFactor 18

newtype SampleMask = SampleMask Word32
  deriving (Eq, Storable)

-- ** PipelineMultisampleStateCreateFlags
-- | Opaque flag
newtype PipelineMultisampleStateCreateFlags = PipelineMultisampleStateCreateFlags Flags
  deriving (Eq, Storable)


data PipelineMultisampleStateCreateInfo =
  PipelineMultisampleStateCreateInfo{ sType :: StructureType 
                                    , pNext :: Ptr Void 
                                    , flags :: PipelineMultisampleStateCreateFlags 
                                    , rasterizationSamples :: SampleCountFlags 
                                    , sampleShadingEnable :: Bool32 
                                    , minSampleShading :: CFloat 
                                    , pSampleMask :: Ptr SampleMask 
                                    , alphaToCoverageEnable :: Bool32 
                                    , alphaToOneEnable :: Bool32 
                                    }
  deriving (Eq)

instance Storable PipelineMultisampleStateCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = PipelineMultisampleStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 20)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 28)
                                                <*> peek (ptr `plusPtr` 32)
                                                <*> peek (ptr `plusPtr` 40)
                                                <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: PipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (rasterizationSamples (poked :: PipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (sampleShadingEnable (poked :: PipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (minSampleShading (poked :: PipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (pSampleMask (poked :: PipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (alphaToCoverageEnable (poked :: PipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 44) (alphaToOneEnable (poked :: PipelineMultisampleStateCreateInfo))



data VertexInputBindingDescription =
  VertexInputBindingDescription{ binding :: Word32 
                               , stride :: Word32 
                               , inputRate :: VertexInputRate 
                               }
  deriving (Eq)

instance Storable VertexInputBindingDescription where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VertexInputBindingDescription <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (binding (poked :: VertexInputBindingDescription))
                *> poke (ptr `plusPtr` 4) (stride (poked :: VertexInputBindingDescription))
                *> poke (ptr `plusPtr` 8) (inputRate (poked :: VertexInputBindingDescription))



data PipelineDepthStencilStateCreateInfo =
  PipelineDepthStencilStateCreateInfo{ sType :: StructureType 
                                     , pNext :: Ptr Void 
                                     , flags :: PipelineDepthStencilStateCreateFlags 
                                     , depthTestEnable :: Bool32 
                                     , depthWriteEnable :: Bool32 
                                     , depthCompareOp :: CompareOp 
                                     , depthBoundsTestEnable :: Bool32 
                                     , stencilTestEnable :: Bool32 
                                     , front :: StencilOpState 
                                     , back :: StencilOpState 
                                     , minDepthBounds :: CFloat 
                                     , maxDepthBounds :: CFloat 
                                     }
  deriving (Eq)

instance Storable PipelineDepthStencilStateCreateInfo where
  sizeOf ~_ = 104
  alignment ~_ = 8
  peek ptr = PipelineDepthStencilStateCreateInfo <$> peek (ptr `plusPtr` 0)
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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: PipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: PipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: PipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (depthTestEnable (poked :: PipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (depthWriteEnable (poked :: PipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (depthCompareOp (poked :: PipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (depthBoundsTestEnable (poked :: PipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 36) (stencilTestEnable (poked :: PipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (front (poked :: PipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 68) (back (poked :: PipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 96) (minDepthBounds (poked :: PipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 100) (maxDepthBounds (poked :: PipelineDepthStencilStateCreateInfo))


-- ** vkCreateComputePipelines
foreign import ccall "vkCreateComputePipelines" vkCreateComputePipelines ::
  Device ->
  PipelineCache ->
    Word32 ->
      Ptr ComputePipelineCreateInfo ->
        Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result


data StencilOpState =
  StencilOpState{ failOp :: StencilOp 
                , passOp :: StencilOp 
                , depthFailOp :: StencilOp 
                , compareOp :: CompareOp 
                , compareMask :: Word32 
                , writeMask :: Word32 
                , reference :: Word32 
                }
  deriving (Eq)

instance Storable StencilOpState where
  sizeOf ~_ = 28
  alignment ~_ = 4
  peek ptr = StencilOpState <$> peek (ptr `plusPtr` 0)
                            <*> peek (ptr `plusPtr` 4)
                            <*> peek (ptr `plusPtr` 8)
                            <*> peek (ptr `plusPtr` 12)
                            <*> peek (ptr `plusPtr` 16)
                            <*> peek (ptr `plusPtr` 20)
                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (failOp (poked :: StencilOpState))
                *> poke (ptr `plusPtr` 4) (passOp (poked :: StencilOpState))
                *> poke (ptr `plusPtr` 8) (depthFailOp (poked :: StencilOpState))
                *> poke (ptr `plusPtr` 12) (compareOp (poked :: StencilOpState))
                *> poke (ptr `plusPtr` 16) (compareMask (poked :: StencilOpState))
                *> poke (ptr `plusPtr` 20) (writeMask (poked :: StencilOpState))
                *> poke (ptr `plusPtr` 24) (reference (poked :: StencilOpState))


