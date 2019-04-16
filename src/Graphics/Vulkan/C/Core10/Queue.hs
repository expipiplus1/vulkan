{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  , VkFence
  , VkPipelineStageFlagBits(..)
  , pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT
  , pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT
  , pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
  , pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
  , pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
  , pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_TRANSFER_BIT
  , pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
  , pattern VK_PIPELINE_STAGE_HOST_BIT
  , pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT
  , pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT
  , VkPipelineStageFlags
  , VkQueue
  , VkSemaphore
  , VkSubmitInfo(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDeviceWaitIdle
#endif
  , FN_vkDeviceWaitIdle
  , PFN_vkDeviceWaitIdle
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkGetDeviceQueue
#endif
  , FN_vkGetDeviceQueue
  , PFN_vkGetDeviceQueue
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkQueueSubmit
#endif
  , FN_vkQueueSubmit
  , PFN_vkQueueSubmit
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkQueueWaitIdle
#endif
  , FN_vkQueueWaitIdle
  , PFN_vkQueueWaitIdle
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkCommandBuffer_T
-- No documentation found for TopLevel "VkCommandBuffer"
type VkCommandBuffer = Ptr VkCommandBuffer_T
-- | Dummy data to tag the 'Ptr' with
data VkFence_T
-- No documentation found for TopLevel "VkFence"
type VkFence = Ptr VkFence_T
-- ** VkPipelineStageFlagBits

-- No documentation found for TopLevel "VkPipelineStageFlagBits"
newtype VkPipelineStageFlagBits = VkPipelineStageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkPipelineStageFlagBits 0x08000000) = showString "VK_PIPELINE_STAGE_RESERVED_27_BIT_KHR"
  showsPrec _ (VkPipelineStageFlagBits 0x04000000) = showString "VK_PIPELINE_STAGE_RESERVED_26_BIT_KHR"
  showsPrec _ (VkPipelineStageFlagBits 0x01000000) = showString "VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT"
  showsPrec _ (VkPipelineStageFlagBits 0x00040000) = showString "VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT"
  showsPrec _ (VkPipelineStageFlagBits 0x00020000) = showString "VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX"
  showsPrec _ (VkPipelineStageFlagBits 0x00400000) = showString "VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV"
  showsPrec _ (VkPipelineStageFlagBits 0x00200000) = showString "VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV"
  showsPrec _ (VkPipelineStageFlagBits 0x02000000) = showString "VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV"
  showsPrec _ (VkPipelineStageFlagBits 0x00080000) = showString "VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV"
  showsPrec _ (VkPipelineStageFlagBits 0x00100000) = showString "VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV"
  showsPrec _ (VkPipelineStageFlagBits 0x00800000) = showString "VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT"
  showsPrec p (VkPipelineStageFlagBits x) = showParen (p >= 11) (showString "VkPipelineStageFlagBits " . showsPrec 11 x)

instance Read VkPipelineStageFlagBits where
  readPrec = parens ( choose [ ("VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT",                    pure VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT)
                             , ("VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT",                  pure VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT)
                             , ("VK_PIPELINE_STAGE_VERTEX_INPUT_BIT",                   pure VK_PIPELINE_STAGE_VERTEX_INPUT_BIT)
                             , ("VK_PIPELINE_STAGE_VERTEX_SHADER_BIT",                  pure VK_PIPELINE_STAGE_VERTEX_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT",    pure VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT", pure VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT",                pure VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT",                pure VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT",           pure VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT)
                             , ("VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT",            pure VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT)
                             , ("VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT",        pure VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
                             , ("VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT",                 pure VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_TRANSFER_BIT",                       pure VK_PIPELINE_STAGE_TRANSFER_BIT)
                             , ("VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT",                 pure VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT)
                             , ("VK_PIPELINE_STAGE_HOST_BIT",                           pure VK_PIPELINE_STAGE_HOST_BIT)
                             , ("VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT",                   pure VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT)
                             , ("VK_PIPELINE_STAGE_ALL_COMMANDS_BIT",                   pure VK_PIPELINE_STAGE_ALL_COMMANDS_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_PIPELINE_STAGE_RESERVED_27_BIT_KHR",                 pure (VkPipelineStageFlagBits 0x08000000))
                             , ("VK_PIPELINE_STAGE_RESERVED_26_BIT_KHR",                 pure (VkPipelineStageFlagBits 0x04000000))
                             , ("VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT",          pure (VkPipelineStageFlagBits 0x01000000))
                             , ("VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT",       pure (VkPipelineStageFlagBits 0x00040000))
                             , ("VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX",             pure (VkPipelineStageFlagBits 0x00020000))
                             , ("VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV",           pure (VkPipelineStageFlagBits 0x00400000))
                             , ("VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV",           pure (VkPipelineStageFlagBits 0x00200000))
                             , ("VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV", pure (VkPipelineStageFlagBits 0x02000000))
                             , ("VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV",                  pure (VkPipelineStageFlagBits 0x00080000))
                             , ("VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV",                  pure (VkPipelineStageFlagBits 0x00100000))
                             , ("VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT",    pure (VkPipelineStageFlagBits 0x00800000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineStageFlagBits")
                        v <- step readPrec
                        pure (VkPipelineStageFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT"
pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT = VkPipelineStageFlagBits 0x00000001

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT"
pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT = VkPipelineStageFlagBits 0x00000002

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_VERTEX_INPUT_BIT"
pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT = VkPipelineStageFlagBits 0x00000004

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_VERTEX_SHADER_BIT"
pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT = VkPipelineStageFlagBits 0x00000008

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT"
pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT = VkPipelineStageFlagBits 0x00000010

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT"
pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = VkPipelineStageFlagBits 0x00000020

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT"
pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT = VkPipelineStageFlagBits 0x00000040

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT"
pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT = VkPipelineStageFlagBits 0x00000080

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT"
pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = VkPipelineStageFlagBits 0x00000100

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT"
pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = VkPipelineStageFlagBits 0x00000200

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT"
pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = VkPipelineStageFlagBits 0x00000400

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT"
pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT = VkPipelineStageFlagBits 0x00000800

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TRANSFER_BIT"
pattern VK_PIPELINE_STAGE_TRANSFER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_TRANSFER_BIT = VkPipelineStageFlagBits 0x00001000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT"
pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = VkPipelineStageFlagBits 0x00002000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_HOST_BIT"
pattern VK_PIPELINE_STAGE_HOST_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_HOST_BIT = VkPipelineStageFlagBits 0x00004000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT"
pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT = VkPipelineStageFlagBits 0x00008000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_ALL_COMMANDS_BIT"
pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT = VkPipelineStageFlagBits 0x00010000
-- No documentation found for TopLevel "VkPipelineStageFlags"
type VkPipelineStageFlags = VkPipelineStageFlagBits
-- | Dummy data to tag the 'Ptr' with
data VkQueue_T
-- No documentation found for TopLevel "VkQueue"
type VkQueue = Ptr VkQueue_T
-- | Dummy data to tag the 'Ptr' with
data VkSemaphore_T
-- No documentation found for TopLevel "VkSemaphore"
type VkSemaphore = Ptr VkSemaphore_T
-- No documentation found for TopLevel "VkSubmitInfo"
data VkSubmitInfo = VkSubmitInfo
  { -- No documentation found for Nested "VkSubmitInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSubmitInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSubmitInfo" "waitSemaphoreCount"
  vkWaitSemaphoreCount :: Word32
  , -- No documentation found for Nested "VkSubmitInfo" "pWaitSemaphores"
  vkPWaitSemaphores :: Ptr VkSemaphore
  , -- No documentation found for Nested "VkSubmitInfo" "pWaitDstStageMask"
  vkPWaitDstStageMask :: Ptr VkPipelineStageFlags
  , -- No documentation found for Nested "VkSubmitInfo" "commandBufferCount"
  vkCommandBufferCount :: Word32
  , -- No documentation found for Nested "VkSubmitInfo" "pCommandBuffers"
  vkPCommandBuffers :: Ptr VkCommandBuffer
  , -- No documentation found for Nested "VkSubmitInfo" "signalSemaphoreCount"
  vkSignalSemaphoreCount :: Word32
  , -- No documentation found for Nested "VkSubmitInfo" "pSignalSemaphores"
  vkPSignalSemaphores :: Ptr VkSemaphore
  }
  deriving (Eq, Show)

instance Storable VkSubmitInfo where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkSubmitInfo <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
                          <*> peek (ptr `plusPtr` 16)
                          <*> peek (ptr `plusPtr` 24)
                          <*> peek (ptr `plusPtr` 32)
                          <*> peek (ptr `plusPtr` 40)
                          <*> peek (ptr `plusPtr` 48)
                          <*> peek (ptr `plusPtr` 56)
                          <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphores (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 32) (vkPWaitDstStageMask (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 40) (vkCommandBufferCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 48) (vkPCommandBuffers (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 56) (vkSignalSemaphoreCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 64) (vkPSignalSemaphores (poked :: VkSubmitInfo))
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkDeviceWaitIdle"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDeviceWaitIdle" vkDeviceWaitIdle :: ("device" ::: VkDevice) -> IO VkResult

#endif
type FN_vkDeviceWaitIdle = ("device" ::: VkDevice) -> IO VkResult
type PFN_vkDeviceWaitIdle = FunPtr FN_vkDeviceWaitIdle
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkGetDeviceQueue"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceQueue" vkGetDeviceQueue :: ("device" ::: VkDevice) -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr VkQueue) -> IO ()

#endif
type FN_vkGetDeviceQueue = ("device" ::: VkDevice) -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr VkQueue) -> IO ()
type PFN_vkGetDeviceQueue = FunPtr FN_vkGetDeviceQueue
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkQueueSubmit"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkQueueSubmit" vkQueueSubmit :: ("queue" ::: VkQueue) -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr VkSubmitInfo) -> ("fence" ::: VkFence) -> IO VkResult

#endif
type FN_vkQueueSubmit = ("queue" ::: VkQueue) -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr VkSubmitInfo) -> ("fence" ::: VkFence) -> IO VkResult
type PFN_vkQueueSubmit = FunPtr FN_vkQueueSubmit
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkQueueWaitIdle"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkQueueWaitIdle" vkQueueWaitIdle :: ("queue" ::: VkQueue) -> IO VkResult

#endif
type FN_vkQueueWaitIdle = ("queue" ::: VkQueue) -> IO VkResult
type PFN_vkQueueWaitIdle = FunPtr FN_vkQueueWaitIdle
