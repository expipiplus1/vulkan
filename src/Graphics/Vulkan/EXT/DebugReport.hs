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

-- ** debugReportMessageEXT
foreign import ccall "vkDebugReportMessageEXT" debugReportMessageEXT ::
  Instance ->
  DebugReportFlagsEXT ->
    DebugReportObjectTypeEXT ->
      Word64 -> CSize -> Int32 -> Ptr CChar -> Ptr CChar -> IO ()

newtype DebugReportCallbackEXT = DebugReportCallbackEXT Word64
  deriving (Eq, Storable)

-- ** DebugReportObjectTypeEXT

newtype DebugReportObjectTypeEXT = DebugReportObjectTypeEXT Int32
  deriving (Eq, Storable)

instance Show DebugReportObjectTypeEXT where
  showsPrec _ DebugReportObjectTypeUnknownExt = showString "DebugReportObjectTypeUnknownExt"
  showsPrec _ DebugReportObjectTypeInstanceExt = showString "DebugReportObjectTypeInstanceExt"
  showsPrec _ DebugReportObjectTypePhysicalDeviceExt = showString "DebugReportObjectTypePhysicalDeviceExt"
  showsPrec _ DebugReportObjectTypeDeviceExt = showString "DebugReportObjectTypeDeviceExt"
  showsPrec _ DebugReportObjectTypeQueueExt = showString "DebugReportObjectTypeQueueExt"
  showsPrec _ DebugReportObjectTypeSemaphoreExt = showString "DebugReportObjectTypeSemaphoreExt"
  showsPrec _ DebugReportObjectTypeCommandBufferExt = showString "DebugReportObjectTypeCommandBufferExt"
  showsPrec _ DebugReportObjectTypeFenceExt = showString "DebugReportObjectTypeFenceExt"
  showsPrec _ DebugReportObjectTypeDeviceMemoryExt = showString "DebugReportObjectTypeDeviceMemoryExt"
  showsPrec _ DebugReportObjectTypeBufferExt = showString "DebugReportObjectTypeBufferExt"
  showsPrec _ DebugReportObjectTypeImageExt = showString "DebugReportObjectTypeImageExt"
  showsPrec _ DebugReportObjectTypeEventExt = showString "DebugReportObjectTypeEventExt"
  showsPrec _ DebugReportObjectTypeQueryPoolExt = showString "DebugReportObjectTypeQueryPoolExt"
  showsPrec _ DebugReportObjectTypeBufferViewExt = showString "DebugReportObjectTypeBufferViewExt"
  showsPrec _ DebugReportObjectTypeImageViewExt = showString "DebugReportObjectTypeImageViewExt"
  showsPrec _ DebugReportObjectTypeShaderModuleExt = showString "DebugReportObjectTypeShaderModuleExt"
  showsPrec _ DebugReportObjectTypePipelineCacheExt = showString "DebugReportObjectTypePipelineCacheExt"
  showsPrec _ DebugReportObjectTypePipelineLayoutExt = showString "DebugReportObjectTypePipelineLayoutExt"
  showsPrec _ DebugReportObjectTypeRenderPassExt = showString "DebugReportObjectTypeRenderPassExt"
  showsPrec _ DebugReportObjectTypePipelineExt = showString "DebugReportObjectTypePipelineExt"
  showsPrec _ DebugReportObjectTypeDescriptorSetLayoutExt = showString "DebugReportObjectTypeDescriptorSetLayoutExt"
  showsPrec _ DebugReportObjectTypeSamplerExt = showString "DebugReportObjectTypeSamplerExt"
  showsPrec _ DebugReportObjectTypeDescriptorPoolExt = showString "DebugReportObjectTypeDescriptorPoolExt"
  showsPrec _ DebugReportObjectTypeDescriptorSetExt = showString "DebugReportObjectTypeDescriptorSetExt"
  showsPrec _ DebugReportObjectTypeFramebufferExt = showString "DebugReportObjectTypeFramebufferExt"
  showsPrec _ DebugReportObjectTypeCommandPoolExt = showString "DebugReportObjectTypeCommandPoolExt"
  showsPrec _ DebugReportObjectTypeSurfaceKhrExt = showString "DebugReportObjectTypeSurfaceKhrExt"
  showsPrec _ DebugReportObjectTypeSwapchainKhrExt = showString "DebugReportObjectTypeSwapchainKhrExt"
  showsPrec _ DebugReportObjectTypeDebugReportExt = showString "DebugReportObjectTypeDebugReportExt"
  showsPrec p (DebugReportObjectTypeEXT x) = showParen (p >= 11) (showString "DebugReportObjectTypeEXT " . showsPrec 11 x)

instance Read DebugReportObjectTypeEXT where
  readPrec = parens ( choose [ ("DebugReportObjectTypeUnknownExt", pure DebugReportObjectTypeUnknownExt)
                             , ("DebugReportObjectTypeInstanceExt", pure DebugReportObjectTypeInstanceExt)
                             , ("DebugReportObjectTypePhysicalDeviceExt", pure DebugReportObjectTypePhysicalDeviceExt)
                             , ("DebugReportObjectTypeDeviceExt", pure DebugReportObjectTypeDeviceExt)
                             , ("DebugReportObjectTypeQueueExt", pure DebugReportObjectTypeQueueExt)
                             , ("DebugReportObjectTypeSemaphoreExt", pure DebugReportObjectTypeSemaphoreExt)
                             , ("DebugReportObjectTypeCommandBufferExt", pure DebugReportObjectTypeCommandBufferExt)
                             , ("DebugReportObjectTypeFenceExt", pure DebugReportObjectTypeFenceExt)
                             , ("DebugReportObjectTypeDeviceMemoryExt", pure DebugReportObjectTypeDeviceMemoryExt)
                             , ("DebugReportObjectTypeBufferExt", pure DebugReportObjectTypeBufferExt)
                             , ("DebugReportObjectTypeImageExt", pure DebugReportObjectTypeImageExt)
                             , ("DebugReportObjectTypeEventExt", pure DebugReportObjectTypeEventExt)
                             , ("DebugReportObjectTypeQueryPoolExt", pure DebugReportObjectTypeQueryPoolExt)
                             , ("DebugReportObjectTypeBufferViewExt", pure DebugReportObjectTypeBufferViewExt)
                             , ("DebugReportObjectTypeImageViewExt", pure DebugReportObjectTypeImageViewExt)
                             , ("DebugReportObjectTypeShaderModuleExt", pure DebugReportObjectTypeShaderModuleExt)
                             , ("DebugReportObjectTypePipelineCacheExt", pure DebugReportObjectTypePipelineCacheExt)
                             , ("DebugReportObjectTypePipelineLayoutExt", pure DebugReportObjectTypePipelineLayoutExt)
                             , ("DebugReportObjectTypeRenderPassExt", pure DebugReportObjectTypeRenderPassExt)
                             , ("DebugReportObjectTypePipelineExt", pure DebugReportObjectTypePipelineExt)
                             , ("DebugReportObjectTypeDescriptorSetLayoutExt", pure DebugReportObjectTypeDescriptorSetLayoutExt)
                             , ("DebugReportObjectTypeSamplerExt", pure DebugReportObjectTypeSamplerExt)
                             , ("DebugReportObjectTypeDescriptorPoolExt", pure DebugReportObjectTypeDescriptorPoolExt)
                             , ("DebugReportObjectTypeDescriptorSetExt", pure DebugReportObjectTypeDescriptorSetExt)
                             , ("DebugReportObjectTypeFramebufferExt", pure DebugReportObjectTypeFramebufferExt)
                             , ("DebugReportObjectTypeCommandPoolExt", pure DebugReportObjectTypeCommandPoolExt)
                             , ("DebugReportObjectTypeSurfaceKhrExt", pure DebugReportObjectTypeSurfaceKhrExt)
                             , ("DebugReportObjectTypeSwapchainKhrExt", pure DebugReportObjectTypeSwapchainKhrExt)
                             , ("DebugReportObjectTypeDebugReportExt", pure DebugReportObjectTypeDebugReportExt)
                             ] +++
                      prec 10 (do
                        expectP (Ident "DebugReportObjectTypeEXT")
                        v <- step readPrec
                        pure (DebugReportObjectTypeEXT v)
                        )
                    )


pattern DebugReportObjectTypeUnknownExt = DebugReportObjectTypeEXT 0

pattern DebugReportObjectTypeInstanceExt = DebugReportObjectTypeEXT 1

pattern DebugReportObjectTypePhysicalDeviceExt = DebugReportObjectTypeEXT 2

pattern DebugReportObjectTypeDeviceExt = DebugReportObjectTypeEXT 3

pattern DebugReportObjectTypeQueueExt = DebugReportObjectTypeEXT 4

pattern DebugReportObjectTypeSemaphoreExt = DebugReportObjectTypeEXT 5

pattern DebugReportObjectTypeCommandBufferExt = DebugReportObjectTypeEXT 6

pattern DebugReportObjectTypeFenceExt = DebugReportObjectTypeEXT 7

pattern DebugReportObjectTypeDeviceMemoryExt = DebugReportObjectTypeEXT 8

pattern DebugReportObjectTypeBufferExt = DebugReportObjectTypeEXT 9

pattern DebugReportObjectTypeImageExt = DebugReportObjectTypeEXT 10

pattern DebugReportObjectTypeEventExt = DebugReportObjectTypeEXT 11

pattern DebugReportObjectTypeQueryPoolExt = DebugReportObjectTypeEXT 12

pattern DebugReportObjectTypeBufferViewExt = DebugReportObjectTypeEXT 13

pattern DebugReportObjectTypeImageViewExt = DebugReportObjectTypeEXT 14

pattern DebugReportObjectTypeShaderModuleExt = DebugReportObjectTypeEXT 15

pattern DebugReportObjectTypePipelineCacheExt = DebugReportObjectTypeEXT 16

pattern DebugReportObjectTypePipelineLayoutExt = DebugReportObjectTypeEXT 17

pattern DebugReportObjectTypeRenderPassExt = DebugReportObjectTypeEXT 18

pattern DebugReportObjectTypePipelineExt = DebugReportObjectTypeEXT 19

pattern DebugReportObjectTypeDescriptorSetLayoutExt = DebugReportObjectTypeEXT 20

pattern DebugReportObjectTypeSamplerExt = DebugReportObjectTypeEXT 21

pattern DebugReportObjectTypeDescriptorPoolExt = DebugReportObjectTypeEXT 22

pattern DebugReportObjectTypeDescriptorSetExt = DebugReportObjectTypeEXT 23

pattern DebugReportObjectTypeFramebufferExt = DebugReportObjectTypeEXT 24

pattern DebugReportObjectTypeCommandPoolExt = DebugReportObjectTypeEXT 25

pattern DebugReportObjectTypeSurfaceKhrExt = DebugReportObjectTypeEXT 26

pattern DebugReportObjectTypeSwapchainKhrExt = DebugReportObjectTypeEXT 27

pattern DebugReportObjectTypeDebugReportExt = DebugReportObjectTypeEXT 28

-- ** DebugReportErrorEXT

newtype DebugReportErrorEXT = DebugReportErrorEXT Int32
  deriving (Eq, Storable)

instance Show DebugReportErrorEXT where
  showsPrec _ DebugReportErrorNoneExt = showString "DebugReportErrorNoneExt"
  showsPrec _ DebugReportErrorCallbackRefExt = showString "DebugReportErrorCallbackRefExt"
  showsPrec p (DebugReportErrorEXT x) = showParen (p >= 11) (showString "DebugReportErrorEXT " . showsPrec 11 x)

instance Read DebugReportErrorEXT where
  readPrec = parens ( choose [ ("DebugReportErrorNoneExt", pure DebugReportErrorNoneExt)
                             , ("DebugReportErrorCallbackRefExt", pure DebugReportErrorCallbackRefExt)
                             ] +++
                      prec 10 (do
                        expectP (Ident "DebugReportErrorEXT")
                        v <- step readPrec
                        pure (DebugReportErrorEXT v)
                        )
                    )


pattern DebugReportErrorNoneExt = DebugReportErrorEXT 0

pattern DebugReportErrorCallbackRefExt = DebugReportErrorEXT 1


data DebugReportCallbackCreateInfoEXT =
  DebugReportCallbackCreateInfoEXT{ sType :: StructureType 
                                  , pNext :: Ptr Void 
                                  , flags :: DebugReportFlagsEXT 
                                  , pfnCallback :: PFN_vkDebugReportCallbackEXT 
                                  , pUserData :: Ptr Void 
                                  }
  deriving (Eq)

instance Storable DebugReportCallbackCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = DebugReportCallbackCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 24)
                                              <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: DebugReportCallbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: DebugReportCallbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (flags (poked :: DebugReportCallbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (pfnCallback (poked :: DebugReportCallbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (pUserData (poked :: DebugReportCallbackCreateInfoEXT))


-- ** destroyDebugReportCallbackEXT
foreign import ccall "vkDestroyDebugReportCallbackEXT" destroyDebugReportCallbackEXT ::
  Instance ->
  DebugReportCallbackEXT -> Ptr AllocationCallbacks -> IO ()

-- ** DebugReportFlagsEXT

newtype DebugReportFlagsEXT = DebugReportFlagsEXT Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show DebugReportFlagsEXT where
  showsPrec _ VK_DEBUG_REPORT_INFORMATION_BIT_EXT = showString "VK_DEBUG_REPORT_INFORMATION_BIT_EXT"
  showsPrec _ VK_DEBUG_REPORT_WARNING_BIT_EXT = showString "VK_DEBUG_REPORT_WARNING_BIT_EXT"
  showsPrec _ VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = showString "VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT"
  showsPrec _ VK_DEBUG_REPORT_ERROR_BIT_EXT = showString "VK_DEBUG_REPORT_ERROR_BIT_EXT"
  showsPrec _ VK_DEBUG_REPORT_DEBUG_BIT_EXT = showString "VK_DEBUG_REPORT_DEBUG_BIT_EXT"
  
  showsPrec p (DebugReportFlagsEXT x) = showParen (p >= 11) (showString "DebugReportFlagsEXT " . showsPrec 11 x)

instance Read DebugReportFlagsEXT where
  readPrec = parens ( choose [ ("VK_DEBUG_REPORT_INFORMATION_BIT_EXT", pure VK_DEBUG_REPORT_INFORMATION_BIT_EXT)
                             , ("VK_DEBUG_REPORT_WARNING_BIT_EXT", pure VK_DEBUG_REPORT_WARNING_BIT_EXT)
                             , ("VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT", pure VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT)
                             , ("VK_DEBUG_REPORT_ERROR_BIT_EXT", pure VK_DEBUG_REPORT_ERROR_BIT_EXT)
                             , ("VK_DEBUG_REPORT_DEBUG_BIT_EXT", pure VK_DEBUG_REPORT_DEBUG_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "DebugReportFlagsEXT")
                        v <- step readPrec
                        pure (DebugReportFlagsEXT v)
                        )
                    )


pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT = DebugReportFlagsEXT 0x1

pattern VK_DEBUG_REPORT_WARNING_BIT_EXT = DebugReportFlagsEXT 0x2

pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = DebugReportFlagsEXT 0x4

pattern VK_DEBUG_REPORT_ERROR_BIT_EXT = DebugReportFlagsEXT 0x8

pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT = DebugReportFlagsEXT 0x10


type PFN_vkDebugReportCallbackEXT = FunPtr
  (DebugReportFlagsEXT ->
     DebugReportObjectTypeEXT ->
       Word64 ->
         CSize -> Int32 -> Ptr CChar -> Ptr CChar -> Ptr Void -> IO Bool32)

-- ** createDebugReportCallbackEXT
foreign import ccall "vkCreateDebugReportCallbackEXT" createDebugReportCallbackEXT ::
  Instance ->
  Ptr DebugReportCallbackCreateInfoEXT ->
    Ptr AllocationCallbacks -> Ptr DebugReportCallbackEXT -> IO Result

