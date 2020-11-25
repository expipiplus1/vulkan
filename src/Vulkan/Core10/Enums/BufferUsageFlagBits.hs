{-# language CPP #-}
-- No documentation found for Chapter "BufferUsageFlagBits"
module Vulkan.Core10.Enums.BufferUsageFlagBits  ( BufferUsageFlags
                                                , BufferUsageFlagBits( BUFFER_USAGE_TRANSFER_SRC_BIT
                                                                     , BUFFER_USAGE_TRANSFER_DST_BIT
                                                                     , BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
                                                                     , BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
                                                                     , BUFFER_USAGE_UNIFORM_BUFFER_BIT
                                                                     , BUFFER_USAGE_STORAGE_BUFFER_BIT
                                                                     , BUFFER_USAGE_INDEX_BUFFER_BIT
                                                                     , BUFFER_USAGE_VERTEX_BUFFER_BIT
                                                                     , BUFFER_USAGE_INDIRECT_BUFFER_BIT
                                                                     , BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR
                                                                     , BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR
                                                                     , BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
                                                                     , BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
                                                                     , BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
                                                                     , BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
                                                                     , BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
                                                                     , ..
                                                                     )
                                                ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type BufferUsageFlags = BufferUsageFlagBits

-- No documentation found for TopLevel "VkBufferUsageFlagBits"
newtype BufferUsageFlagBits = BufferUsageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TRANSFER_SRC_BIT"
pattern BUFFER_USAGE_TRANSFER_SRC_BIT                          = BufferUsageFlagBits 0x00000001
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TRANSFER_DST_BIT"
pattern BUFFER_USAGE_TRANSFER_DST_BIT                          = BufferUsageFlagBits 0x00000002
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT"
pattern BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT                  = BufferUsageFlagBits 0x00000004
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT"
pattern BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT                  = BufferUsageFlagBits 0x00000008
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT"
pattern BUFFER_USAGE_UNIFORM_BUFFER_BIT                        = BufferUsageFlagBits 0x00000010
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_STORAGE_BUFFER_BIT"
pattern BUFFER_USAGE_STORAGE_BUFFER_BIT                        = BufferUsageFlagBits 0x00000020
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_INDEX_BUFFER_BIT"
pattern BUFFER_USAGE_INDEX_BUFFER_BIT                          = BufferUsageFlagBits 0x00000040
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_VERTEX_BUFFER_BIT"
pattern BUFFER_USAGE_VERTEX_BUFFER_BIT                         = BufferUsageFlagBits 0x00000080
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT"
pattern BUFFER_USAGE_INDIRECT_BUFFER_BIT                       = BufferUsageFlagBits 0x00000100
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR"
pattern BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR              = BufferUsageFlagBits 0x00000400
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR"
pattern BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR    = BufferUsageFlagBits 0x00100000
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR"
pattern BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR = BufferUsageFlagBits 0x00080000
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT"
pattern BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT             = BufferUsageFlagBits 0x00000200
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT = BufferUsageFlagBits 0x00001000
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT         = BufferUsageFlagBits 0x00000800
-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT"
pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT                 = BufferUsageFlagBits 0x00020000

conNameBufferUsageFlagBits :: String
conNameBufferUsageFlagBits = "BufferUsageFlagBits"

enumPrefixBufferUsageFlagBits :: String
enumPrefixBufferUsageFlagBits = "BUFFER_USAGE_"

showTableBufferUsageFlagBits :: [(BufferUsageFlagBits, String)]
showTableBufferUsageFlagBits =
  [ (BUFFER_USAGE_TRANSFER_SRC_BIT                      , "TRANSFER_SRC_BIT")
  , (BUFFER_USAGE_TRANSFER_DST_BIT                      , "TRANSFER_DST_BIT")
  , (BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT              , "UNIFORM_TEXEL_BUFFER_BIT")
  , (BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT              , "STORAGE_TEXEL_BUFFER_BIT")
  , (BUFFER_USAGE_UNIFORM_BUFFER_BIT                    , "UNIFORM_BUFFER_BIT")
  , (BUFFER_USAGE_STORAGE_BUFFER_BIT                    , "STORAGE_BUFFER_BIT")
  , (BUFFER_USAGE_INDEX_BUFFER_BIT                      , "INDEX_BUFFER_BIT")
  , (BUFFER_USAGE_VERTEX_BUFFER_BIT                     , "VERTEX_BUFFER_BIT")
  , (BUFFER_USAGE_INDIRECT_BUFFER_BIT                   , "INDIRECT_BUFFER_BIT")
  , (BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR          , "SHADER_BINDING_TABLE_BIT_KHR")
  , (BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR, "ACCELERATION_STRUCTURE_STORAGE_BIT_KHR")
  , ( BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
    , "ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR"
    )
  , (BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT            , "CONDITIONAL_RENDERING_BIT_EXT")
  , (BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT, "TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT")
  , (BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT        , "TRANSFORM_FEEDBACK_BUFFER_BIT_EXT")
  , (BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT                , "SHADER_DEVICE_ADDRESS_BIT")
  ]


instance Show BufferUsageFlagBits where
showsPrec = enumShowsPrec enumPrefixBufferUsageFlagBits
                          showTableBufferUsageFlagBits
                          conNameBufferUsageFlagBits
                          (\(BufferUsageFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read BufferUsageFlagBits where
  readPrec = enumReadPrec enumPrefixBufferUsageFlagBits
                          showTableBufferUsageFlagBits
                          conNameBufferUsageFlagBits
                          BufferUsageFlagBits

