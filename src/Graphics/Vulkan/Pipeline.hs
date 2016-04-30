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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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


-- ** CullModeFlags

newtype CullModeFlags = CullModeFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show CullModeFlags where
  showsPrec _ CullModeFrontBit = showString "CullModeFrontBit"
  showsPrec _ CullModeBackBit = showString "CullModeBackBit"
  showsPrec _ CullModeNone = showString "CullModeNone"
  showsPrec _ CullModeFrontAndBack = showString "CullModeFrontAndBack"
  showsPrec p (CullModeFlags x) = showParen (p >= 11) (showString "CullModeFlags " . showsPrec 11 x)

instance Read CullModeFlags where
  readPrec = parens ( choose [ ("CullModeFrontBit", pure CullModeFrontBit)
                             , ("CullModeBackBit", pure CullModeBackBit)
                             , ("CullModeNone", pure CullModeNone)
                             , ("CullModeFrontAndBack", pure CullModeFrontAndBack)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CullModeFlags")
                        v <- step readPrec
                        pure (CullModeFlags v)
                        )
                    )


pattern CullModeFrontBit = CullModeFlags 0x1

pattern CullModeBackBit = CullModeFlags 0x2

pattern CullModeNone = CullModeFlags 0x0

pattern CullModeFrontAndBack = CullModeFlags 0x3

-- ** PipelineDepthStencilStateCreateFlags
-- | Opaque flag
newtype PipelineDepthStencilStateCreateFlags = PipelineDepthStencilStateCreateFlags Flags
  deriving (Eq, Ord, Storable)


data PipelineShaderStageCreateInfo =
  PipelineShaderStageCreateInfo{ sType :: StructureType 
                               , pNext :: Ptr Void 
                               , flags :: PipelineShaderStageCreateFlags 
                               , stage :: ShaderStageFlags 
                               , _module :: ShaderModule 
                               , pName :: Ptr CChar 
                               , pSpecializationInfo :: Ptr SpecializationInfo 
                               }
  deriving (Eq, Ord)

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


-- ** ColorComponentFlags

newtype ColorComponentFlags = ColorComponentFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show ColorComponentFlags where
  showsPrec _ ColorComponentRBit = showString "ColorComponentRBit"
  showsPrec _ ColorComponentGBit = showString "ColorComponentGBit"
  showsPrec _ ColorComponentBBit = showString "ColorComponentBBit"
  showsPrec _ ColorComponentABit = showString "ColorComponentABit"
  
  showsPrec p (ColorComponentFlags x) = showParen (p >= 11) (showString "ColorComponentFlags " . showsPrec 11 x)

instance Read ColorComponentFlags where
  readPrec = parens ( choose [ ("ColorComponentRBit", pure ColorComponentRBit)
                             , ("ColorComponentGBit", pure ColorComponentGBit)
                             , ("ColorComponentBBit", pure ColorComponentBBit)
                             , ("ColorComponentABit", pure ColorComponentABit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ColorComponentFlags")
                        v <- step readPrec
                        pure (ColorComponentFlags v)
                        )
                    )


pattern ColorComponentRBit = ColorComponentFlags 0x1

pattern ColorComponentGBit = ColorComponentFlags 0x2

pattern ColorComponentBBit = ColorComponentFlags 0x4

pattern ColorComponentABit = ColorComponentFlags 0x8



data ComputePipelineCreateInfo =
  ComputePipelineCreateInfo{ sType :: StructureType 
                           , pNext :: Ptr Void 
                           , flags :: PipelineCreateFlags 
                           , stage :: PipelineShaderStageCreateInfo 
                           , layout :: PipelineLayout 
                           , basePipelineHandle :: Pipeline 
                           , basePipelineIndex :: Int32 
                           }
  deriving (Eq, Ord)

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
  deriving (Eq, Ord, Storable)

instance Show StencilOp where
  showsPrec _ StencilOpKeep = showString "StencilOpKeep"
  showsPrec _ StencilOpZero = showString "StencilOpZero"
  showsPrec _ StencilOpReplace = showString "StencilOpReplace"
  showsPrec _ StencilOpIncrementAndClamp = showString "StencilOpIncrementAndClamp"
  showsPrec _ StencilOpDecrementAndClamp = showString "StencilOpDecrementAndClamp"
  showsPrec _ StencilOpInvert = showString "StencilOpInvert"
  showsPrec _ StencilOpIncrementAndWrap = showString "StencilOpIncrementAndWrap"
  showsPrec _ StencilOpDecrementAndWrap = showString "StencilOpDecrementAndWrap"
  showsPrec p (StencilOp x) = showParen (p >= 11) (showString "StencilOp " . showsPrec 11 x)

instance Read StencilOp where
  readPrec = parens ( choose [ ("StencilOpKeep", pure StencilOpKeep)
                             , ("StencilOpZero", pure StencilOpZero)
                             , ("StencilOpReplace", pure StencilOpReplace)
                             , ("StencilOpIncrementAndClamp", pure StencilOpIncrementAndClamp)
                             , ("StencilOpDecrementAndClamp", pure StencilOpDecrementAndClamp)
                             , ("StencilOpInvert", pure StencilOpInvert)
                             , ("StencilOpIncrementAndWrap", pure StencilOpIncrementAndWrap)
                             , ("StencilOpDecrementAndWrap", pure StencilOpDecrementAndWrap)
                             ] +++
                      prec 10 (do
                        expectP (Ident "StencilOp")
                        v <- step readPrec
                        pure (StencilOp v)
                        )
                    )


pattern StencilOpKeep = StencilOp 0

pattern StencilOpZero = StencilOp 1

pattern StencilOpReplace = StencilOp 2

pattern StencilOpIncrementAndClamp = StencilOp 3

pattern StencilOpDecrementAndClamp = StencilOp 4

pattern StencilOpInvert = StencilOp 5

pattern StencilOpIncrementAndWrap = StencilOp 6

pattern StencilOpDecrementAndWrap = StencilOp 7


data SpecializationInfo =
  SpecializationInfo{ mapEntryCount :: Word32 
                    , pMapEntries :: Ptr SpecializationMapEntry 
                    , dataSize :: CSize 
                    , pData :: Ptr Void 
                    }
  deriving (Eq, Ord)

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
  deriving (Eq, Ord, Storable)

newtype Pipeline = Pipeline Word64
  deriving (Eq, Ord, Storable)

-- ** PipelineInputAssemblyStateCreateFlags
-- | Opaque flag
newtype PipelineInputAssemblyStateCreateFlags = PipelineInputAssemblyStateCreateFlags Flags
  deriving (Eq, Ord, Storable)

-- ** createGraphicsPipelines
foreign import ccall "vkCreateGraphicsPipelines" createGraphicsPipelines ::
  Device ->
  PipelineCache ->
    Word32 ->
      Ptr GraphicsPipelineCreateInfo ->
        Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- ** FrontFace

newtype FrontFace = FrontFace Int32
  deriving (Eq, Ord, Storable)

instance Show FrontFace where
  showsPrec _ FrontFaceCounterClockwise = showString "FrontFaceCounterClockwise"
  showsPrec _ FrontFaceClockwise = showString "FrontFaceClockwise"
  showsPrec p (FrontFace x) = showParen (p >= 11) (showString "FrontFace " . showsPrec 11 x)

instance Read FrontFace where
  readPrec = parens ( choose [ ("FrontFaceCounterClockwise", pure FrontFaceCounterClockwise)
                             , ("FrontFaceClockwise", pure FrontFaceClockwise)
                             ] +++
                      prec 10 (do
                        expectP (Ident "FrontFace")
                        v <- step readPrec
                        pure (FrontFace v)
                        )
                    )


pattern FrontFaceCounterClockwise = FrontFace 0

pattern FrontFaceClockwise = FrontFace 1

-- ** PolygonMode

newtype PolygonMode = PolygonMode Int32
  deriving (Eq, Ord, Storable)

instance Show PolygonMode where
  showsPrec _ PolygonModeFill = showString "PolygonModeFill"
  showsPrec _ PolygonModeLine = showString "PolygonModeLine"
  showsPrec _ PolygonModePoint = showString "PolygonModePoint"
  showsPrec p (PolygonMode x) = showParen (p >= 11) (showString "PolygonMode " . showsPrec 11 x)

instance Read PolygonMode where
  readPrec = parens ( choose [ ("PolygonModeFill", pure PolygonModeFill)
                             , ("PolygonModeLine", pure PolygonModeLine)
                             , ("PolygonModePoint", pure PolygonModePoint)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PolygonMode")
                        v <- step readPrec
                        pure (PolygonMode v)
                        )
                    )


pattern PolygonModeFill = PolygonMode 0

pattern PolygonModeLine = PolygonMode 1

pattern PolygonModePoint = PolygonMode 2

-- ** PipelineViewportStateCreateFlags
-- | Opaque flag
newtype PipelineViewportStateCreateFlags = PipelineViewportStateCreateFlags Flags
  deriving (Eq, Ord, Storable)

-- ** LogicOp

newtype LogicOp = LogicOp Int32
  deriving (Eq, Ord, Storable)

instance Show LogicOp where
  showsPrec _ LogicOpClear = showString "LogicOpClear"
  showsPrec _ LogicOpAnd = showString "LogicOpAnd"
  showsPrec _ LogicOpAndReverse = showString "LogicOpAndReverse"
  showsPrec _ LogicOpCopy = showString "LogicOpCopy"
  showsPrec _ LogicOpAndInverted = showString "LogicOpAndInverted"
  showsPrec _ LogicOpNoOp = showString "LogicOpNoOp"
  showsPrec _ LogicOpXor = showString "LogicOpXor"
  showsPrec _ LogicOpOr = showString "LogicOpOr"
  showsPrec _ LogicOpNor = showString "LogicOpNor"
  showsPrec _ LogicOpEquivalent = showString "LogicOpEquivalent"
  showsPrec _ LogicOpInvert = showString "LogicOpInvert"
  showsPrec _ LogicOpOrReverse = showString "LogicOpOrReverse"
  showsPrec _ LogicOpCopyInverted = showString "LogicOpCopyInverted"
  showsPrec _ LogicOpOrInverted = showString "LogicOpOrInverted"
  showsPrec _ LogicOpNand = showString "LogicOpNand"
  showsPrec _ LogicOpSet = showString "LogicOpSet"
  showsPrec p (LogicOp x) = showParen (p >= 11) (showString "LogicOp " . showsPrec 11 x)

instance Read LogicOp where
  readPrec = parens ( choose [ ("LogicOpClear", pure LogicOpClear)
                             , ("LogicOpAnd", pure LogicOpAnd)
                             , ("LogicOpAndReverse", pure LogicOpAndReverse)
                             , ("LogicOpCopy", pure LogicOpCopy)
                             , ("LogicOpAndInverted", pure LogicOpAndInverted)
                             , ("LogicOpNoOp", pure LogicOpNoOp)
                             , ("LogicOpXor", pure LogicOpXor)
                             , ("LogicOpOr", pure LogicOpOr)
                             , ("LogicOpNor", pure LogicOpNor)
                             , ("LogicOpEquivalent", pure LogicOpEquivalent)
                             , ("LogicOpInvert", pure LogicOpInvert)
                             , ("LogicOpOrReverse", pure LogicOpOrReverse)
                             , ("LogicOpCopyInverted", pure LogicOpCopyInverted)
                             , ("LogicOpOrInverted", pure LogicOpOrInverted)
                             , ("LogicOpNand", pure LogicOpNand)
                             , ("LogicOpSet", pure LogicOpSet)
                             ] +++
                      prec 10 (do
                        expectP (Ident "LogicOp")
                        v <- step readPrec
                        pure (LogicOp v)
                        )
                    )


pattern LogicOpClear = LogicOp 0

pattern LogicOpAnd = LogicOp 1

pattern LogicOpAndReverse = LogicOp 2

pattern LogicOpCopy = LogicOp 3

pattern LogicOpAndInverted = LogicOp 4

pattern LogicOpNoOp = LogicOp 5

pattern LogicOpXor = LogicOp 6

pattern LogicOpOr = LogicOp 7

pattern LogicOpNor = LogicOp 8

pattern LogicOpEquivalent = LogicOp 9

pattern LogicOpInvert = LogicOp 10

pattern LogicOpOrReverse = LogicOp 11

pattern LogicOpCopyInverted = LogicOp 12

pattern LogicOpOrInverted = LogicOp 13

pattern LogicOpNand = LogicOp 14

pattern LogicOpSet = LogicOp 15

-- ** PipelineCreateFlags

newtype PipelineCreateFlags = PipelineCreateFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show PipelineCreateFlags where
  showsPrec _ PipelineCreateDisableOptimizationBit = showString "PipelineCreateDisableOptimizationBit"
  showsPrec _ PipelineCreateAllowDerivativesBit = showString "PipelineCreateAllowDerivativesBit"
  showsPrec _ PipelineCreateDerivativeBit = showString "PipelineCreateDerivativeBit"
  
  showsPrec p (PipelineCreateFlags x) = showParen (p >= 11) (showString "PipelineCreateFlags " . showsPrec 11 x)

instance Read PipelineCreateFlags where
  readPrec = parens ( choose [ ("PipelineCreateDisableOptimizationBit", pure PipelineCreateDisableOptimizationBit)
                             , ("PipelineCreateAllowDerivativesBit", pure PipelineCreateAllowDerivativesBit)
                             , ("PipelineCreateDerivativeBit", pure PipelineCreateDerivativeBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PipelineCreateFlags")
                        v <- step readPrec
                        pure (PipelineCreateFlags v)
                        )
                    )


pattern PipelineCreateDisableOptimizationBit = PipelineCreateFlags 0x1

pattern PipelineCreateAllowDerivativesBit = PipelineCreateFlags 0x2

pattern PipelineCreateDerivativeBit = PipelineCreateFlags 0x4


-- ** PipelineRasterizationStateCreateFlags
-- | Opaque flag
newtype PipelineRasterizationStateCreateFlags = PipelineRasterizationStateCreateFlags Flags
  deriving (Eq, Ord, Storable)

-- ** DynamicState

newtype DynamicState = DynamicState Int32
  deriving (Eq, Ord, Storable)

instance Show DynamicState where
  showsPrec _ DynamicStateViewport = showString "DynamicStateViewport"
  showsPrec _ DynamicStateScissor = showString "DynamicStateScissor"
  showsPrec _ DynamicStateLineWidth = showString "DynamicStateLineWidth"
  showsPrec _ DynamicStateDepthBias = showString "DynamicStateDepthBias"
  showsPrec _ DynamicStateBlendConstants = showString "DynamicStateBlendConstants"
  showsPrec _ DynamicStateDepthBounds = showString "DynamicStateDepthBounds"
  showsPrec _ DynamicStateStencilCompareMask = showString "DynamicStateStencilCompareMask"
  showsPrec _ DynamicStateStencilWriteMask = showString "DynamicStateStencilWriteMask"
  showsPrec _ DynamicStateStencilReference = showString "DynamicStateStencilReference"
  showsPrec p (DynamicState x) = showParen (p >= 11) (showString "DynamicState " . showsPrec 11 x)

instance Read DynamicState where
  readPrec = parens ( choose [ ("DynamicStateViewport", pure DynamicStateViewport)
                             , ("DynamicStateScissor", pure DynamicStateScissor)
                             , ("DynamicStateLineWidth", pure DynamicStateLineWidth)
                             , ("DynamicStateDepthBias", pure DynamicStateDepthBias)
                             , ("DynamicStateBlendConstants", pure DynamicStateBlendConstants)
                             , ("DynamicStateDepthBounds", pure DynamicStateDepthBounds)
                             , ("DynamicStateStencilCompareMask", pure DynamicStateStencilCompareMask)
                             , ("DynamicStateStencilWriteMask", pure DynamicStateStencilWriteMask)
                             , ("DynamicStateStencilReference", pure DynamicStateStencilReference)
                             ] +++
                      prec 10 (do
                        expectP (Ident "DynamicState")
                        v <- step readPrec
                        pure (DynamicState v)
                        )
                    )


pattern DynamicStateViewport = DynamicState 0

pattern DynamicStateScissor = DynamicState 1

pattern DynamicStateLineWidth = DynamicState 2

pattern DynamicStateDepthBias = DynamicState 3

pattern DynamicStateBlendConstants = DynamicState 4

pattern DynamicStateDepthBounds = DynamicState 5

pattern DynamicStateStencilCompareMask = DynamicState 6

pattern DynamicStateStencilWriteMask = DynamicState 7

pattern DynamicStateStencilReference = DynamicState 8

-- ** PipelineBindPoint

newtype PipelineBindPoint = PipelineBindPoint Int32
  deriving (Eq, Ord, Storable)

instance Show PipelineBindPoint where
  showsPrec _ PipelineBindPointGraphics = showString "PipelineBindPointGraphics"
  showsPrec _ PipelineBindPointCompute = showString "PipelineBindPointCompute"
  showsPrec p (PipelineBindPoint x) = showParen (p >= 11) (showString "PipelineBindPoint " . showsPrec 11 x)

instance Read PipelineBindPoint where
  readPrec = parens ( choose [ ("PipelineBindPointGraphics", pure PipelineBindPointGraphics)
                             , ("PipelineBindPointCompute", pure PipelineBindPointCompute)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PipelineBindPoint")
                        v <- step readPrec
                        pure (PipelineBindPoint v)
                        )
                    )


pattern PipelineBindPointGraphics = PipelineBindPoint 0

pattern PipelineBindPointCompute = PipelineBindPoint 1

-- ** PipelineDynamicStateCreateFlags
-- | Opaque flag
newtype PipelineDynamicStateCreateFlags = PipelineDynamicStateCreateFlags Flags
  deriving (Eq, Ord, Storable)


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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord, Storable)

instance Show BlendOp where
  showsPrec _ BlendOpAdd = showString "BlendOpAdd"
  showsPrec _ BlendOpSubtract = showString "BlendOpSubtract"
  showsPrec _ BlendOpReverseSubtract = showString "BlendOpReverseSubtract"
  showsPrec _ BlendOpMin = showString "BlendOpMin"
  showsPrec _ BlendOpMax = showString "BlendOpMax"
  showsPrec p (BlendOp x) = showParen (p >= 11) (showString "BlendOp " . showsPrec 11 x)

instance Read BlendOp where
  readPrec = parens ( choose [ ("BlendOpAdd", pure BlendOpAdd)
                             , ("BlendOpSubtract", pure BlendOpSubtract)
                             , ("BlendOpReverseSubtract", pure BlendOpReverseSubtract)
                             , ("BlendOpMin", pure BlendOpMin)
                             , ("BlendOpMax", pure BlendOpMax)
                             ] +++
                      prec 10 (do
                        expectP (Ident "BlendOp")
                        v <- step readPrec
                        pure (BlendOp v)
                        )
                    )


pattern BlendOpAdd = BlendOp 0

pattern BlendOpSubtract = BlendOp 1

pattern BlendOpReverseSubtract = BlendOp 2

pattern BlendOpMin = BlendOp 3

pattern BlendOpMax = BlendOp 4

-- ** destroyPipeline
foreign import ccall "vkDestroyPipeline" destroyPipeline ::
  Device -> Pipeline -> Ptr AllocationCallbacks -> IO ()

-- ** PipelineShaderStageCreateFlags
-- | Opaque flag
newtype PipelineShaderStageCreateFlags = PipelineShaderStageCreateFlags Flags
  deriving (Eq, Ord, Storable)


data PipelineViewportStateCreateInfo =
  PipelineViewportStateCreateInfo{ sType :: StructureType 
                                 , pNext :: Ptr Void 
                                 , flags :: PipelineViewportStateCreateFlags 
                                 , viewportCount :: Word32 
                                 , pViewports :: Ptr Viewport 
                                 , scissorCount :: Word32 
                                 , pScissors :: Ptr Rect2D 
                                 }
  deriving (Eq, Ord)

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
  deriving (Eq, Ord, Storable)


data PipelineVertexInputStateCreateInfo =
  PipelineVertexInputStateCreateInfo{ sType :: StructureType 
                                    , pNext :: Ptr Void 
                                    , flags :: PipelineVertexInputStateCreateFlags 
                                    , vertexBindingDescriptionCount :: Word32 
                                    , pVertexBindingDescriptions :: Ptr VertexInputBindingDescription 
                                    , vertexAttributeDescriptionCount :: Word32 
                                    , pVertexAttributeDescriptions :: Ptr VertexInputAttributeDescription 
                                    }
  deriving (Eq, Ord)

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
  deriving (Eq, Ord, Storable)

instance Show PrimitiveTopology where
  showsPrec _ PrimitiveTopologyPointList = showString "PrimitiveTopologyPointList"
  showsPrec _ PrimitiveTopologyLineList = showString "PrimitiveTopologyLineList"
  showsPrec _ PrimitiveTopologyLineStrip = showString "PrimitiveTopologyLineStrip"
  showsPrec _ PrimitiveTopologyTriangleList = showString "PrimitiveTopologyTriangleList"
  showsPrec _ PrimitiveTopologyTriangleStrip = showString "PrimitiveTopologyTriangleStrip"
  showsPrec _ PrimitiveTopologyTriangleFan = showString "PrimitiveTopologyTriangleFan"
  showsPrec _ PrimitiveTopologyLineListWithAdjacency = showString "PrimitiveTopologyLineListWithAdjacency"
  showsPrec _ PrimitiveTopologyLineStripWithAdjacency = showString "PrimitiveTopologyLineStripWithAdjacency"
  showsPrec _ PrimitiveTopologyTriangleListWithAdjacency = showString "PrimitiveTopologyTriangleListWithAdjacency"
  showsPrec _ PrimitiveTopologyTriangleStripWithAdjacency = showString "PrimitiveTopologyTriangleStripWithAdjacency"
  showsPrec _ PrimitiveTopologyPatchList = showString "PrimitiveTopologyPatchList"
  showsPrec p (PrimitiveTopology x) = showParen (p >= 11) (showString "PrimitiveTopology " . showsPrec 11 x)

instance Read PrimitiveTopology where
  readPrec = parens ( choose [ ("PrimitiveTopologyPointList", pure PrimitiveTopologyPointList)
                             , ("PrimitiveTopologyLineList", pure PrimitiveTopologyLineList)
                             , ("PrimitiveTopologyLineStrip", pure PrimitiveTopologyLineStrip)
                             , ("PrimitiveTopologyTriangleList", pure PrimitiveTopologyTriangleList)
                             , ("PrimitiveTopologyTriangleStrip", pure PrimitiveTopologyTriangleStrip)
                             , ("PrimitiveTopologyTriangleFan", pure PrimitiveTopologyTriangleFan)
                             , ("PrimitiveTopologyLineListWithAdjacency", pure PrimitiveTopologyLineListWithAdjacency)
                             , ("PrimitiveTopologyLineStripWithAdjacency", pure PrimitiveTopologyLineStripWithAdjacency)
                             , ("PrimitiveTopologyTriangleListWithAdjacency", pure PrimitiveTopologyTriangleListWithAdjacency)
                             , ("PrimitiveTopologyTriangleStripWithAdjacency", pure PrimitiveTopologyTriangleStripWithAdjacency)
                             , ("PrimitiveTopologyPatchList", pure PrimitiveTopologyPatchList)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PrimitiveTopology")
                        v <- step readPrec
                        pure (PrimitiveTopology v)
                        )
                    )


pattern PrimitiveTopologyPointList = PrimitiveTopology 0

pattern PrimitiveTopologyLineList = PrimitiveTopology 1

pattern PrimitiveTopologyLineStrip = PrimitiveTopology 2

pattern PrimitiveTopologyTriangleList = PrimitiveTopology 3

pattern PrimitiveTopologyTriangleStrip = PrimitiveTopology 4

pattern PrimitiveTopologyTriangleFan = PrimitiveTopology 5

pattern PrimitiveTopologyLineListWithAdjacency = PrimitiveTopology 6

pattern PrimitiveTopologyLineStripWithAdjacency = PrimitiveTopology 7

pattern PrimitiveTopologyTriangleListWithAdjacency = PrimitiveTopology 8

pattern PrimitiveTopologyTriangleStripWithAdjacency = PrimitiveTopology 9

pattern PrimitiveTopologyPatchList = PrimitiveTopology 10


data PipelineInputAssemblyStateCreateInfo =
  PipelineInputAssemblyStateCreateInfo{ sType :: StructureType 
                                      , pNext :: Ptr Void 
                                      , flags :: PipelineInputAssemblyStateCreateFlags 
                                      , topology :: PrimitiveTopology 
                                      , primitiveRestartEnable :: Bool32 
                                      }
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord, Storable)

-- ** VertexInputRate

newtype VertexInputRate = VertexInputRate Int32
  deriving (Eq, Ord, Storable)

instance Show VertexInputRate where
  showsPrec _ VertexInputRateVertex = showString "VertexInputRateVertex"
  showsPrec _ VertexInputRateInstance = showString "VertexInputRateInstance"
  showsPrec p (VertexInputRate x) = showParen (p >= 11) (showString "VertexInputRate " . showsPrec 11 x)

instance Read VertexInputRate where
  readPrec = parens ( choose [ ("VertexInputRateVertex", pure VertexInputRateVertex)
                             , ("VertexInputRateInstance", pure VertexInputRateInstance)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VertexInputRate")
                        v <- step readPrec
                        pure (VertexInputRate v)
                        )
                    )


pattern VertexInputRateVertex = VertexInputRate 0

pattern VertexInputRateInstance = VertexInputRate 1

-- ** PipelineStageFlags

newtype PipelineStageFlags = PipelineStageFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show PipelineStageFlags where
  showsPrec _ PipelineStageTopOfPipeBit = showString "PipelineStageTopOfPipeBit"
  showsPrec _ PipelineStageDrawIndirectBit = showString "PipelineStageDrawIndirectBit"
  showsPrec _ PipelineStageVertexInputBit = showString "PipelineStageVertexInputBit"
  showsPrec _ PipelineStageVertexShaderBit = showString "PipelineStageVertexShaderBit"
  showsPrec _ PipelineStageTessellationControlShaderBit = showString "PipelineStageTessellationControlShaderBit"
  showsPrec _ PipelineStageTessellationEvaluationShaderBit = showString "PipelineStageTessellationEvaluationShaderBit"
  showsPrec _ PipelineStageGeometryShaderBit = showString "PipelineStageGeometryShaderBit"
  showsPrec _ PipelineStageFragmentShaderBit = showString "PipelineStageFragmentShaderBit"
  showsPrec _ PipelineStageEarlyFragmentTestsBit = showString "PipelineStageEarlyFragmentTestsBit"
  showsPrec _ PipelineStageLateFragmentTestsBit = showString "PipelineStageLateFragmentTestsBit"
  showsPrec _ PipelineStageColorAttachmentOutputBit = showString "PipelineStageColorAttachmentOutputBit"
  showsPrec _ PipelineStageComputeShaderBit = showString "PipelineStageComputeShaderBit"
  showsPrec _ PipelineStageTransferBit = showString "PipelineStageTransferBit"
  showsPrec _ PipelineStageBottomOfPipeBit = showString "PipelineStageBottomOfPipeBit"
  showsPrec _ PipelineStageHostBit = showString "PipelineStageHostBit"
  showsPrec _ PipelineStageAllGraphicsBit = showString "PipelineStageAllGraphicsBit"
  showsPrec _ PipelineStageAllCommandsBit = showString "PipelineStageAllCommandsBit"
  
  showsPrec p (PipelineStageFlags x) = showParen (p >= 11) (showString "PipelineStageFlags " . showsPrec 11 x)

instance Read PipelineStageFlags where
  readPrec = parens ( choose [ ("PipelineStageTopOfPipeBit", pure PipelineStageTopOfPipeBit)
                             , ("PipelineStageDrawIndirectBit", pure PipelineStageDrawIndirectBit)
                             , ("PipelineStageVertexInputBit", pure PipelineStageVertexInputBit)
                             , ("PipelineStageVertexShaderBit", pure PipelineStageVertexShaderBit)
                             , ("PipelineStageTessellationControlShaderBit", pure PipelineStageTessellationControlShaderBit)
                             , ("PipelineStageTessellationEvaluationShaderBit", pure PipelineStageTessellationEvaluationShaderBit)
                             , ("PipelineStageGeometryShaderBit", pure PipelineStageGeometryShaderBit)
                             , ("PipelineStageFragmentShaderBit", pure PipelineStageFragmentShaderBit)
                             , ("PipelineStageEarlyFragmentTestsBit", pure PipelineStageEarlyFragmentTestsBit)
                             , ("PipelineStageLateFragmentTestsBit", pure PipelineStageLateFragmentTestsBit)
                             , ("PipelineStageColorAttachmentOutputBit", pure PipelineStageColorAttachmentOutputBit)
                             , ("PipelineStageComputeShaderBit", pure PipelineStageComputeShaderBit)
                             , ("PipelineStageTransferBit", pure PipelineStageTransferBit)
                             , ("PipelineStageBottomOfPipeBit", pure PipelineStageBottomOfPipeBit)
                             , ("PipelineStageHostBit", pure PipelineStageHostBit)
                             , ("PipelineStageAllGraphicsBit", pure PipelineStageAllGraphicsBit)
                             , ("PipelineStageAllCommandsBit", pure PipelineStageAllCommandsBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PipelineStageFlags")
                        v <- step readPrec
                        pure (PipelineStageFlags v)
                        )
                    )

-- | Before subsequent commands are processed
pattern PipelineStageTopOfPipeBit = PipelineStageFlags 0x1
-- | Draw/DispatchIndirect command fetch
pattern PipelineStageDrawIndirectBit = PipelineStageFlags 0x2
-- | Vertex/index fetch
pattern PipelineStageVertexInputBit = PipelineStageFlags 0x4
-- | Vertex shading
pattern PipelineStageVertexShaderBit = PipelineStageFlags 0x8
-- | Tessellation control shading
pattern PipelineStageTessellationControlShaderBit = PipelineStageFlags 0x10
-- | Tessellation evaluation shading
pattern PipelineStageTessellationEvaluationShaderBit = PipelineStageFlags 0x20
-- | Geometry shading
pattern PipelineStageGeometryShaderBit = PipelineStageFlags 0x40
-- | Fragment shading
pattern PipelineStageFragmentShaderBit = PipelineStageFlags 0x80
-- | Early fragment (depth and stencil) tests
pattern PipelineStageEarlyFragmentTestsBit = PipelineStageFlags 0x100
-- | Late fragment (depth and stencil) tests
pattern PipelineStageLateFragmentTestsBit = PipelineStageFlags 0x200
-- | Color attachment writes
pattern PipelineStageColorAttachmentOutputBit = PipelineStageFlags 0x400
-- | Compute shading
pattern PipelineStageComputeShaderBit = PipelineStageFlags 0x800
-- | Transfer/copy operations
pattern PipelineStageTransferBit = PipelineStageFlags 0x1000
-- | After previous commands have completed
pattern PipelineStageBottomOfPipeBit = PipelineStageFlags 0x2000
-- | Indicates host (CPU) is a source/sink of the dependency
pattern PipelineStageHostBit = PipelineStageFlags 0x4000
-- | All stages of the graphics pipeline
pattern PipelineStageAllGraphicsBit = PipelineStageFlags 0x8000
-- | All stages supported on the queue
pattern PipelineStageAllCommandsBit = PipelineStageFlags 0x10000



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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord, Storable)

instance Show BlendFactor where
  showsPrec _ BlendFactorZero = showString "BlendFactorZero"
  showsPrec _ BlendFactorOne = showString "BlendFactorOne"
  showsPrec _ BlendFactorSrcColor = showString "BlendFactorSrcColor"
  showsPrec _ BlendFactorOneMinusSrcColor = showString "BlendFactorOneMinusSrcColor"
  showsPrec _ BlendFactorDstColor = showString "BlendFactorDstColor"
  showsPrec _ BlendFactorOneMinusDstColor = showString "BlendFactorOneMinusDstColor"
  showsPrec _ BlendFactorSrcAlpha = showString "BlendFactorSrcAlpha"
  showsPrec _ BlendFactorOneMinusSrcAlpha = showString "BlendFactorOneMinusSrcAlpha"
  showsPrec _ BlendFactorDstAlpha = showString "BlendFactorDstAlpha"
  showsPrec _ BlendFactorOneMinusDstAlpha = showString "BlendFactorOneMinusDstAlpha"
  showsPrec _ BlendFactorConstantColor = showString "BlendFactorConstantColor"
  showsPrec _ BlendFactorOneMinusConstantColor = showString "BlendFactorOneMinusConstantColor"
  showsPrec _ BlendFactorConstantAlpha = showString "BlendFactorConstantAlpha"
  showsPrec _ BlendFactorOneMinusConstantAlpha = showString "BlendFactorOneMinusConstantAlpha"
  showsPrec _ BlendFactorSrcAlphaSaturate = showString "BlendFactorSrcAlphaSaturate"
  showsPrec _ BlendFactorSrc1Color = showString "BlendFactorSrc1Color"
  showsPrec _ BlendFactorOneMinusSrc1Color = showString "BlendFactorOneMinusSrc1Color"
  showsPrec _ BlendFactorSrc1Alpha = showString "BlendFactorSrc1Alpha"
  showsPrec _ BlendFactorOneMinusSrc1Alpha = showString "BlendFactorOneMinusSrc1Alpha"
  showsPrec p (BlendFactor x) = showParen (p >= 11) (showString "BlendFactor " . showsPrec 11 x)

instance Read BlendFactor where
  readPrec = parens ( choose [ ("BlendFactorZero", pure BlendFactorZero)
                             , ("BlendFactorOne", pure BlendFactorOne)
                             , ("BlendFactorSrcColor", pure BlendFactorSrcColor)
                             , ("BlendFactorOneMinusSrcColor", pure BlendFactorOneMinusSrcColor)
                             , ("BlendFactorDstColor", pure BlendFactorDstColor)
                             , ("BlendFactorOneMinusDstColor", pure BlendFactorOneMinusDstColor)
                             , ("BlendFactorSrcAlpha", pure BlendFactorSrcAlpha)
                             , ("BlendFactorOneMinusSrcAlpha", pure BlendFactorOneMinusSrcAlpha)
                             , ("BlendFactorDstAlpha", pure BlendFactorDstAlpha)
                             , ("BlendFactorOneMinusDstAlpha", pure BlendFactorOneMinusDstAlpha)
                             , ("BlendFactorConstantColor", pure BlendFactorConstantColor)
                             , ("BlendFactorOneMinusConstantColor", pure BlendFactorOneMinusConstantColor)
                             , ("BlendFactorConstantAlpha", pure BlendFactorConstantAlpha)
                             , ("BlendFactorOneMinusConstantAlpha", pure BlendFactorOneMinusConstantAlpha)
                             , ("BlendFactorSrcAlphaSaturate", pure BlendFactorSrcAlphaSaturate)
                             , ("BlendFactorSrc1Color", pure BlendFactorSrc1Color)
                             , ("BlendFactorOneMinusSrc1Color", pure BlendFactorOneMinusSrc1Color)
                             , ("BlendFactorSrc1Alpha", pure BlendFactorSrc1Alpha)
                             , ("BlendFactorOneMinusSrc1Alpha", pure BlendFactorOneMinusSrc1Alpha)
                             ] +++
                      prec 10 (do
                        expectP (Ident "BlendFactor")
                        v <- step readPrec
                        pure (BlendFactor v)
                        )
                    )


pattern BlendFactorZero = BlendFactor 0

pattern BlendFactorOne = BlendFactor 1

pattern BlendFactorSrcColor = BlendFactor 2

pattern BlendFactorOneMinusSrcColor = BlendFactor 3

pattern BlendFactorDstColor = BlendFactor 4

pattern BlendFactorOneMinusDstColor = BlendFactor 5

pattern BlendFactorSrcAlpha = BlendFactor 6

pattern BlendFactorOneMinusSrcAlpha = BlendFactor 7

pattern BlendFactorDstAlpha = BlendFactor 8

pattern BlendFactorOneMinusDstAlpha = BlendFactor 9

pattern BlendFactorConstantColor = BlendFactor 10

pattern BlendFactorOneMinusConstantColor = BlendFactor 11

pattern BlendFactorConstantAlpha = BlendFactor 12

pattern BlendFactorOneMinusConstantAlpha = BlendFactor 13

pattern BlendFactorSrcAlphaSaturate = BlendFactor 14

pattern BlendFactorSrc1Color = BlendFactor 15

pattern BlendFactorOneMinusSrc1Color = BlendFactor 16

pattern BlendFactorSrc1Alpha = BlendFactor 17

pattern BlendFactorOneMinusSrc1Alpha = BlendFactor 18

newtype SampleMask = SampleMask Word32
  deriving (Eq, Ord, Storable)

-- ** PipelineMultisampleStateCreateFlags
-- | Opaque flag
newtype PipelineMultisampleStateCreateFlags = PipelineMultisampleStateCreateFlags Flags
  deriving (Eq, Ord, Storable)


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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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


-- ** createComputePipelines
foreign import ccall "vkCreateComputePipelines" createComputePipelines ::
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
  deriving (Eq, Ord)

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


