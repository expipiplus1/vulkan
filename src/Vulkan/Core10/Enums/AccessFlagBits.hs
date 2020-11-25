{-# language CPP #-}
-- No documentation found for Chapter "AccessFlagBits"
module Vulkan.Core10.Enums.AccessFlagBits  ( AccessFlags
                                           , AccessFlagBits( ACCESS_INDIRECT_COMMAND_READ_BIT
                                                           , ACCESS_INDEX_READ_BIT
                                                           , ACCESS_VERTEX_ATTRIBUTE_READ_BIT
                                                           , ACCESS_UNIFORM_READ_BIT
                                                           , ACCESS_INPUT_ATTACHMENT_READ_BIT
                                                           , ACCESS_SHADER_READ_BIT
                                                           , ACCESS_SHADER_WRITE_BIT
                                                           , ACCESS_COLOR_ATTACHMENT_READ_BIT
                                                           , ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                                                           , ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
                                                           , ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
                                                           , ACCESS_TRANSFER_READ_BIT
                                                           , ACCESS_TRANSFER_WRITE_BIT
                                                           , ACCESS_HOST_READ_BIT
                                                           , ACCESS_HOST_WRITE_BIT
                                                           , ACCESS_MEMORY_READ_BIT
                                                           , ACCESS_MEMORY_WRITE_BIT
                                                           , ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV
                                                           , ACCESS_COMMAND_PREPROCESS_READ_BIT_NV
                                                           , ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
                                                           , ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV
                                                           , ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR
                                                           , ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR
                                                           , ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
                                                           , ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT
                                                           , ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT
                                                           , ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT
                                                           , ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT
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
type AccessFlags = AccessFlagBits

-- No documentation found for TopLevel "VkAccessFlagBits"
newtype AccessFlagBits = AccessFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_INDIRECT_COMMAND_READ_BIT"
pattern ACCESS_INDIRECT_COMMAND_READ_BIT                 = AccessFlagBits 0x00000001
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_INDEX_READ_BIT"
pattern ACCESS_INDEX_READ_BIT                            = AccessFlagBits 0x00000002
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT"
pattern ACCESS_VERTEX_ATTRIBUTE_READ_BIT                 = AccessFlagBits 0x00000004
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_UNIFORM_READ_BIT"
pattern ACCESS_UNIFORM_READ_BIT                          = AccessFlagBits 0x00000008
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_INPUT_ATTACHMENT_READ_BIT"
pattern ACCESS_INPUT_ATTACHMENT_READ_BIT                 = AccessFlagBits 0x00000010
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_SHADER_READ_BIT"
pattern ACCESS_SHADER_READ_BIT                           = AccessFlagBits 0x00000020
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_SHADER_WRITE_BIT"
pattern ACCESS_SHADER_WRITE_BIT                          = AccessFlagBits 0x00000040
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COLOR_ATTACHMENT_READ_BIT"
pattern ACCESS_COLOR_ATTACHMENT_READ_BIT                 = AccessFlagBits 0x00000080
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT"
pattern ACCESS_COLOR_ATTACHMENT_WRITE_BIT                = AccessFlagBits 0x00000100
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT"
pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT         = AccessFlagBits 0x00000200
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT"
pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT        = AccessFlagBits 0x00000400
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_TRANSFER_READ_BIT"
pattern ACCESS_TRANSFER_READ_BIT                         = AccessFlagBits 0x00000800
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_TRANSFER_WRITE_BIT"
pattern ACCESS_TRANSFER_WRITE_BIT                        = AccessFlagBits 0x00001000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_HOST_READ_BIT"
pattern ACCESS_HOST_READ_BIT                             = AccessFlagBits 0x00002000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_HOST_WRITE_BIT"
pattern ACCESS_HOST_WRITE_BIT                            = AccessFlagBits 0x00004000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_MEMORY_READ_BIT"
pattern ACCESS_MEMORY_READ_BIT                           = AccessFlagBits 0x00008000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_MEMORY_WRITE_BIT"
pattern ACCESS_MEMORY_WRITE_BIT                          = AccessFlagBits 0x00010000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV"
pattern ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV           = AccessFlagBits 0x00040000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COMMAND_PREPROCESS_READ_BIT_NV"
pattern ACCESS_COMMAND_PREPROCESS_READ_BIT_NV            = AccessFlagBits 0x00020000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT"
pattern ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT         = AccessFlagBits 0x01000000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV"
pattern ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV            = AccessFlagBits 0x00800000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR"
pattern ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR      = AccessFlagBits 0x00400000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR"
pattern ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR       = AccessFlagBits 0x00200000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT"
pattern ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT = AccessFlagBits 0x00080000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT"
pattern ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT        = AccessFlagBits 0x00100000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT"
pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT  = AccessFlagBits 0x08000000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT"
pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT   = AccessFlagBits 0x04000000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT"
pattern ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT          = AccessFlagBits 0x02000000

conNameAccessFlagBits :: String
conNameAccessFlagBits = "AccessFlagBits"

enumPrefixAccessFlagBits :: String
enumPrefixAccessFlagBits = "ACCESS_"

showTableAccessFlagBits :: [(AccessFlagBits, String)]
showTableAccessFlagBits =
  [ (ACCESS_INDIRECT_COMMAND_READ_BIT                , "INDIRECT_COMMAND_READ_BIT")
  , (ACCESS_INDEX_READ_BIT                           , "INDEX_READ_BIT")
  , (ACCESS_VERTEX_ATTRIBUTE_READ_BIT                , "VERTEX_ATTRIBUTE_READ_BIT")
  , (ACCESS_UNIFORM_READ_BIT                         , "UNIFORM_READ_BIT")
  , (ACCESS_INPUT_ATTACHMENT_READ_BIT                , "INPUT_ATTACHMENT_READ_BIT")
  , (ACCESS_SHADER_READ_BIT                          , "SHADER_READ_BIT")
  , (ACCESS_SHADER_WRITE_BIT                         , "SHADER_WRITE_BIT")
  , (ACCESS_COLOR_ATTACHMENT_READ_BIT                , "COLOR_ATTACHMENT_READ_BIT")
  , (ACCESS_COLOR_ATTACHMENT_WRITE_BIT               , "COLOR_ATTACHMENT_WRITE_BIT")
  , (ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT        , "DEPTH_STENCIL_ATTACHMENT_READ_BIT")
  , (ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT       , "DEPTH_STENCIL_ATTACHMENT_WRITE_BIT")
  , (ACCESS_TRANSFER_READ_BIT                        , "TRANSFER_READ_BIT")
  , (ACCESS_TRANSFER_WRITE_BIT                       , "TRANSFER_WRITE_BIT")
  , (ACCESS_HOST_READ_BIT                            , "HOST_READ_BIT")
  , (ACCESS_HOST_WRITE_BIT                           , "HOST_WRITE_BIT")
  , (ACCESS_MEMORY_READ_BIT                          , "MEMORY_READ_BIT")
  , (ACCESS_MEMORY_WRITE_BIT                         , "MEMORY_WRITE_BIT")
  , (ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV          , "COMMAND_PREPROCESS_WRITE_BIT_NV")
  , (ACCESS_COMMAND_PREPROCESS_READ_BIT_NV           , "COMMAND_PREPROCESS_READ_BIT_NV")
  , (ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT        , "FRAGMENT_DENSITY_MAP_READ_BIT_EXT")
  , (ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV           , "SHADING_RATE_IMAGE_READ_BIT_NV")
  , (ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR     , "ACCELERATION_STRUCTURE_WRITE_BIT_KHR")
  , (ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR      , "ACCELERATION_STRUCTURE_READ_BIT_KHR")
  , (ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT, "COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT")
  , (ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT       , "CONDITIONAL_RENDERING_READ_BIT_EXT")
  , (ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT , "TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT")
  , (ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT  , "TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT")
  , (ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT         , "TRANSFORM_FEEDBACK_WRITE_BIT_EXT")
  ]


instance Show AccessFlagBits where
showsPrec = enumShowsPrec enumPrefixAccessFlagBits
                          showTableAccessFlagBits
                          conNameAccessFlagBits
                          (\(AccessFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read AccessFlagBits where
  readPrec = enumReadPrec enumPrefixAccessFlagBits showTableAccessFlagBits conNameAccessFlagBits AccessFlagBits

