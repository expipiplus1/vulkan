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
import Graphics.Vulkan.Core( VkFlags(..)
                           , StructureType(..)
                           , VkBool32(..)
                           , Result(..)
                           )
import Foreign.C.Types( CChar(..)
                      , CSize(..)
                      )

-- ** vkDebugReportMessageEXT
foreign import ccall "vkDebugReportMessageEXT" vkDebugReportMessageEXT ::
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
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT"
  showsPrec _ VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT = showString "VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT"
  showsPrec p (DebugReportObjectTypeEXT x) = showParen (p >= 11) (showString "DebugReportObjectTypeEXT " . showsPrec 11 x)

instance Read DebugReportObjectTypeEXT where
  readPrec = parens ( choose [ ("VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT)
                             , ("VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT", pure VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "DebugReportObjectTypeEXT")
                        v <- step readPrec
                        pure (DebugReportObjectTypeEXT v)
                        )
                    )


pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT = DebugReportObjectTypeEXT 0

pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT = DebugReportObjectTypeEXT 1

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT = DebugReportObjectTypeEXT 2

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT = DebugReportObjectTypeEXT 3

pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT = DebugReportObjectTypeEXT 4

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT = DebugReportObjectTypeEXT 5

pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT = DebugReportObjectTypeEXT 6

pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT = DebugReportObjectTypeEXT 7

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT = DebugReportObjectTypeEXT 8

pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT = DebugReportObjectTypeEXT 9

pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT = DebugReportObjectTypeEXT 10

pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT = DebugReportObjectTypeEXT 11

pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT = DebugReportObjectTypeEXT 12

pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT = DebugReportObjectTypeEXT 13

pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT = DebugReportObjectTypeEXT 14

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT = DebugReportObjectTypeEXT 15

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT = DebugReportObjectTypeEXT 16

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT = DebugReportObjectTypeEXT 17

pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT = DebugReportObjectTypeEXT 18

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT = DebugReportObjectTypeEXT 19

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT = DebugReportObjectTypeEXT 20

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT = DebugReportObjectTypeEXT 21

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT = DebugReportObjectTypeEXT 22

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT = DebugReportObjectTypeEXT 23

pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT = DebugReportObjectTypeEXT 24

pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT = DebugReportObjectTypeEXT 25

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT = DebugReportObjectTypeEXT 26

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT = DebugReportObjectTypeEXT 27

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT = DebugReportObjectTypeEXT 28

-- ** DebugReportErrorEXT

newtype DebugReportErrorEXT = DebugReportErrorEXT Int32
  deriving (Eq, Storable)

instance Show DebugReportErrorEXT where
  showsPrec _ VK_DEBUG_REPORT_ERROR_NONE_EXT = showString "VK_DEBUG_REPORT_ERROR_NONE_EXT"
  showsPrec _ VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT = showString "VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT"
  showsPrec p (DebugReportErrorEXT x) = showParen (p >= 11) (showString "DebugReportErrorEXT " . showsPrec 11 x)

instance Read DebugReportErrorEXT where
  readPrec = parens ( choose [ ("VK_DEBUG_REPORT_ERROR_NONE_EXT", pure VK_DEBUG_REPORT_ERROR_NONE_EXT)
                             , ("VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT", pure VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "DebugReportErrorEXT")
                        v <- step readPrec
                        pure (DebugReportErrorEXT v)
                        )
                    )


pattern VK_DEBUG_REPORT_ERROR_NONE_EXT = DebugReportErrorEXT 0

pattern VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT = DebugReportErrorEXT 1


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


-- ** vkDestroyDebugReportCallbackEXT
foreign import ccall "vkDestroyDebugReportCallbackEXT" vkDestroyDebugReportCallbackEXT ::
  Instance ->
  DebugReportCallbackEXT -> Ptr AllocationCallbacks -> IO ()

-- ** VkDebugReportFlagsEXT

newtype DebugReportFlagsEXT = DebugReportFlagsEXT VkFlags
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
         CSize ->
           Int32 -> Ptr CChar -> Ptr CChar -> Ptr Void -> IO VkBool32)

-- ** vkCreateDebugReportCallbackEXT
foreign import ccall "vkCreateDebugReportCallbackEXT" vkCreateDebugReportCallbackEXT ::
  Instance ->
  Ptr DebugReportCallbackCreateInfoEXT ->
    Ptr AllocationCallbacks -> Ptr DebugReportCallbackEXT -> IO Result

