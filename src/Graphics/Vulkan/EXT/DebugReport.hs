{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.EXT.DebugReport where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64(..)
                )
import Foreign.Ptr( Ptr(..)
                  , FunPtr(..)
                  , plusPtr
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
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.DeviceInitialization( Instance(..)
                                           )
import Graphics.Vulkan.Core( Bool32(..)
                           , StructureType(..)
                           , Result(..)
                           , Flags(..)
                           )
import Foreign.C.Types( CChar(..)
                      , CSize(..)
                      )

-- ** debugReportMessage
foreign import ccall "vkDebugReportMessageEXT" debugReportMessage ::
  Instance ->
  DebugReportFlags ->
    DebugReportObjectType ->
      Word64 -> CSize -> Int32 -> Ptr CChar -> Ptr CChar -> IO ()

newtype DebugReportCallback = DebugReportCallback Word64
  deriving (Eq, Ord, Storable)

-- ** DebugReportObjectType

newtype DebugReportObjectType = DebugReportObjectType Int32
  deriving (Eq, Ord, Storable)

instance Show DebugReportObjectType where
  showsPrec _ DebugReportObjectTypeUnknown = showString "DebugReportObjectTypeUnknown"
  showsPrec _ DebugReportObjectTypeInstance = showString "DebugReportObjectTypeInstance"
  showsPrec _ DebugReportObjectTypePhysicalDevice = showString "DebugReportObjectTypePhysicalDevice"
  showsPrec _ DebugReportObjectTypeDevice = showString "DebugReportObjectTypeDevice"
  showsPrec _ DebugReportObjectTypeQueue = showString "DebugReportObjectTypeQueue"
  showsPrec _ DebugReportObjectTypeSemaphore = showString "DebugReportObjectTypeSemaphore"
  showsPrec _ DebugReportObjectTypeCommandBuffer = showString "DebugReportObjectTypeCommandBuffer"
  showsPrec _ DebugReportObjectTypeFence = showString "DebugReportObjectTypeFence"
  showsPrec _ DebugReportObjectTypeDeviceMemory = showString "DebugReportObjectTypeDeviceMemory"
  showsPrec _ DebugReportObjectTypeBuffer = showString "DebugReportObjectTypeBuffer"
  showsPrec _ DebugReportObjectTypeImage = showString "DebugReportObjectTypeImage"
  showsPrec _ DebugReportObjectTypeEvent = showString "DebugReportObjectTypeEvent"
  showsPrec _ DebugReportObjectTypeQueryPool = showString "DebugReportObjectTypeQueryPool"
  showsPrec _ DebugReportObjectTypeBufferView = showString "DebugReportObjectTypeBufferView"
  showsPrec _ DebugReportObjectTypeImageView = showString "DebugReportObjectTypeImageView"
  showsPrec _ DebugReportObjectTypeShaderModule = showString "DebugReportObjectTypeShaderModule"
  showsPrec _ DebugReportObjectTypePipelineCache = showString "DebugReportObjectTypePipelineCache"
  showsPrec _ DebugReportObjectTypePipelineLayout = showString "DebugReportObjectTypePipelineLayout"
  showsPrec _ DebugReportObjectTypeRenderPass = showString "DebugReportObjectTypeRenderPass"
  showsPrec _ DebugReportObjectTypePipeline = showString "DebugReportObjectTypePipeline"
  showsPrec _ DebugReportObjectTypeDescriptorSetLayout = showString "DebugReportObjectTypeDescriptorSetLayout"
  showsPrec _ DebugReportObjectTypeSampler = showString "DebugReportObjectTypeSampler"
  showsPrec _ DebugReportObjectTypeDescriptorPool = showString "DebugReportObjectTypeDescriptorPool"
  showsPrec _ DebugReportObjectTypeDescriptorSet = showString "DebugReportObjectTypeDescriptorSet"
  showsPrec _ DebugReportObjectTypeFramebuffer = showString "DebugReportObjectTypeFramebuffer"
  showsPrec _ DebugReportObjectTypeCommandPool = showString "DebugReportObjectTypeCommandPool"
  showsPrec _ DebugReportObjectTypeSurfaceKhr = showString "DebugReportObjectTypeSurfaceKhr"
  showsPrec _ DebugReportObjectTypeSwapchainKhr = showString "DebugReportObjectTypeSwapchainKhr"
  showsPrec _ DebugReportObjectTypeDebugReport = showString "DebugReportObjectTypeDebugReport"
  showsPrec p (DebugReportObjectType x) = showParen (p >= 11) (showString "DebugReportObjectType " . showsPrec 11 x)

instance Read DebugReportObjectType where
  readPrec = parens ( choose [ ("DebugReportObjectTypeUnknown", pure DebugReportObjectTypeUnknown)
                             , ("DebugReportObjectTypeInstance", pure DebugReportObjectTypeInstance)
                             , ("DebugReportObjectTypePhysicalDevice", pure DebugReportObjectTypePhysicalDevice)
                             , ("DebugReportObjectTypeDevice", pure DebugReportObjectTypeDevice)
                             , ("DebugReportObjectTypeQueue", pure DebugReportObjectTypeQueue)
                             , ("DebugReportObjectTypeSemaphore", pure DebugReportObjectTypeSemaphore)
                             , ("DebugReportObjectTypeCommandBuffer", pure DebugReportObjectTypeCommandBuffer)
                             , ("DebugReportObjectTypeFence", pure DebugReportObjectTypeFence)
                             , ("DebugReportObjectTypeDeviceMemory", pure DebugReportObjectTypeDeviceMemory)
                             , ("DebugReportObjectTypeBuffer", pure DebugReportObjectTypeBuffer)
                             , ("DebugReportObjectTypeImage", pure DebugReportObjectTypeImage)
                             , ("DebugReportObjectTypeEvent", pure DebugReportObjectTypeEvent)
                             , ("DebugReportObjectTypeQueryPool", pure DebugReportObjectTypeQueryPool)
                             , ("DebugReportObjectTypeBufferView", pure DebugReportObjectTypeBufferView)
                             , ("DebugReportObjectTypeImageView", pure DebugReportObjectTypeImageView)
                             , ("DebugReportObjectTypeShaderModule", pure DebugReportObjectTypeShaderModule)
                             , ("DebugReportObjectTypePipelineCache", pure DebugReportObjectTypePipelineCache)
                             , ("DebugReportObjectTypePipelineLayout", pure DebugReportObjectTypePipelineLayout)
                             , ("DebugReportObjectTypeRenderPass", pure DebugReportObjectTypeRenderPass)
                             , ("DebugReportObjectTypePipeline", pure DebugReportObjectTypePipeline)
                             , ("DebugReportObjectTypeDescriptorSetLayout", pure DebugReportObjectTypeDescriptorSetLayout)
                             , ("DebugReportObjectTypeSampler", pure DebugReportObjectTypeSampler)
                             , ("DebugReportObjectTypeDescriptorPool", pure DebugReportObjectTypeDescriptorPool)
                             , ("DebugReportObjectTypeDescriptorSet", pure DebugReportObjectTypeDescriptorSet)
                             , ("DebugReportObjectTypeFramebuffer", pure DebugReportObjectTypeFramebuffer)
                             , ("DebugReportObjectTypeCommandPool", pure DebugReportObjectTypeCommandPool)
                             , ("DebugReportObjectTypeSurfaceKhr", pure DebugReportObjectTypeSurfaceKhr)
                             , ("DebugReportObjectTypeSwapchainKhr", pure DebugReportObjectTypeSwapchainKhr)
                             , ("DebugReportObjectTypeDebugReport", pure DebugReportObjectTypeDebugReport)
                             ] +++
                      prec 10 (do
                        expectP (Ident "DebugReportObjectType")
                        v <- step readPrec
                        pure (DebugReportObjectType v)
                        )
                    )


pattern DebugReportObjectTypeUnknown = DebugReportObjectType 0

pattern DebugReportObjectTypeInstance = DebugReportObjectType 1

pattern DebugReportObjectTypePhysicalDevice = DebugReportObjectType 2

pattern DebugReportObjectTypeDevice = DebugReportObjectType 3

pattern DebugReportObjectTypeQueue = DebugReportObjectType 4

pattern DebugReportObjectTypeSemaphore = DebugReportObjectType 5

pattern DebugReportObjectTypeCommandBuffer = DebugReportObjectType 6

pattern DebugReportObjectTypeFence = DebugReportObjectType 7

pattern DebugReportObjectTypeDeviceMemory = DebugReportObjectType 8

pattern DebugReportObjectTypeBuffer = DebugReportObjectType 9

pattern DebugReportObjectTypeImage = DebugReportObjectType 10

pattern DebugReportObjectTypeEvent = DebugReportObjectType 11

pattern DebugReportObjectTypeQueryPool = DebugReportObjectType 12

pattern DebugReportObjectTypeBufferView = DebugReportObjectType 13

pattern DebugReportObjectTypeImageView = DebugReportObjectType 14

pattern DebugReportObjectTypeShaderModule = DebugReportObjectType 15

pattern DebugReportObjectTypePipelineCache = DebugReportObjectType 16

pattern DebugReportObjectTypePipelineLayout = DebugReportObjectType 17

pattern DebugReportObjectTypeRenderPass = DebugReportObjectType 18

pattern DebugReportObjectTypePipeline = DebugReportObjectType 19

pattern DebugReportObjectTypeDescriptorSetLayout = DebugReportObjectType 20

pattern DebugReportObjectTypeSampler = DebugReportObjectType 21

pattern DebugReportObjectTypeDescriptorPool = DebugReportObjectType 22

pattern DebugReportObjectTypeDescriptorSet = DebugReportObjectType 23

pattern DebugReportObjectTypeFramebuffer = DebugReportObjectType 24

pattern DebugReportObjectTypeCommandPool = DebugReportObjectType 25

pattern DebugReportObjectTypeSurfaceKhr = DebugReportObjectType 26

pattern DebugReportObjectTypeSwapchainKhr = DebugReportObjectType 27

pattern DebugReportObjectTypeDebugReport = DebugReportObjectType 28

-- ** DebugReportError

newtype DebugReportError = DebugReportError Int32
  deriving (Eq, Ord, Storable)

instance Show DebugReportError where
  showsPrec _ DebugReportErrorNone = showString "DebugReportErrorNone"
  showsPrec _ DebugReportErrorCallbackRef = showString "DebugReportErrorCallbackRef"
  showsPrec p (DebugReportError x) = showParen (p >= 11) (showString "DebugReportError " . showsPrec 11 x)

instance Read DebugReportError where
  readPrec = parens ( choose [ ("DebugReportErrorNone", pure DebugReportErrorNone)
                             , ("DebugReportErrorCallbackRef", pure DebugReportErrorCallbackRef)
                             ] +++
                      prec 10 (do
                        expectP (Ident "DebugReportError")
                        v <- step readPrec
                        pure (DebugReportError v)
                        )
                    )


pattern DebugReportErrorNone = DebugReportError 0

pattern DebugReportErrorCallbackRef = DebugReportError 1


data DebugReportCallbackCreateInfo =
  DebugReportCallbackCreateInfo{ sType :: StructureType 
                               , pNext :: Ptr Void 
                               , flags :: DebugReportFlags 
                               , pfnCallback :: PFN_vkDebugReportCallbackEXT 
                               , pUserData :: Ptr Void 
                               }
  deriving (Eq, Ord)

instance Storable DebugReportCallbackCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = DebugReportCallbackCreateInfo <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: DebugReportCallbackCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: DebugReportCallbackCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: DebugReportCallbackCreateInfo))
                *> poke (ptr `plusPtr` 24) (pfnCallback (poked :: DebugReportCallbackCreateInfo))
                *> poke (ptr `plusPtr` 32) (pUserData (poked :: DebugReportCallbackCreateInfo))


-- ** destroyDebugReportCallback
foreign import ccall "vkDestroyDebugReportCallbackEXT" destroyDebugReportCallback ::
  Instance -> DebugReportCallback -> Ptr AllocationCallbacks -> IO ()

-- ** DebugReportFlags

newtype DebugReportFlags = DebugReportFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show DebugReportFlags where
  showsPrec _ DebugReportInformationBit = showString "DebugReportInformationBit"
  showsPrec _ DebugReportWarningBit = showString "DebugReportWarningBit"
  showsPrec _ DebugReportPerformanceWarningBit = showString "DebugReportPerformanceWarningBit"
  showsPrec _ DebugReportErrorBit = showString "DebugReportErrorBit"
  showsPrec _ DebugReportDebugBit = showString "DebugReportDebugBit"
  
  showsPrec p (DebugReportFlags x) = showParen (p >= 11) (showString "DebugReportFlags " . showsPrec 11 x)

instance Read DebugReportFlags where
  readPrec = parens ( choose [ ("DebugReportInformationBit", pure DebugReportInformationBit)
                             , ("DebugReportWarningBit", pure DebugReportWarningBit)
                             , ("DebugReportPerformanceWarningBit", pure DebugReportPerformanceWarningBit)
                             , ("DebugReportErrorBit", pure DebugReportErrorBit)
                             , ("DebugReportDebugBit", pure DebugReportDebugBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "DebugReportFlags")
                        v <- step readPrec
                        pure (DebugReportFlags v)
                        )
                    )


pattern DebugReportInformationBit = DebugReportFlags 0x1

pattern DebugReportWarningBit = DebugReportFlags 0x2

pattern DebugReportPerformanceWarningBit = DebugReportFlags 0x4

pattern DebugReportErrorBit = DebugReportFlags 0x8

pattern DebugReportDebugBit = DebugReportFlags 0x10


type PFN_vkDebugReportCallbackEXT = FunPtr
  (DebugReportFlags ->
     DebugReportObjectType ->
       Word64 ->
         CSize -> Int32 -> Ptr CChar -> Ptr CChar -> Ptr Void -> IO Bool32)

-- ** createDebugReportCallback
foreign import ccall "vkCreateDebugReportCallbackEXT" createDebugReportCallback ::
  Instance ->
  Ptr DebugReportCallbackCreateInfo ->
    Ptr AllocationCallbacks -> Ptr DebugReportCallback -> IO Result

