{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}

module Graphics.Vulkan.C.Core10.Core
  ( VkBaseInStructure(..)
  , VkBaseOutStructure(..)
  , VkBool32(..)
  , pattern VK_FALSE
  , pattern VK_TRUE
  , VkFlags
  , VkFormat(..)
  , pattern VK_FORMAT_UNDEFINED
  , pattern VK_FORMAT_R4G4_UNORM_PACK8
  , pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16
  , pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16
  , pattern VK_FORMAT_R5G6B5_UNORM_PACK16
  , pattern VK_FORMAT_B5G6R5_UNORM_PACK16
  , pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16
  , pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16
  , pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16
  , pattern VK_FORMAT_R8_UNORM
  , pattern VK_FORMAT_R8_SNORM
  , pattern VK_FORMAT_R8_USCALED
  , pattern VK_FORMAT_R8_SSCALED
  , pattern VK_FORMAT_R8_UINT
  , pattern VK_FORMAT_R8_SINT
  , pattern VK_FORMAT_R8_SRGB
  , pattern VK_FORMAT_R8G8_UNORM
  , pattern VK_FORMAT_R8G8_SNORM
  , pattern VK_FORMAT_R8G8_USCALED
  , pattern VK_FORMAT_R8G8_SSCALED
  , pattern VK_FORMAT_R8G8_UINT
  , pattern VK_FORMAT_R8G8_SINT
  , pattern VK_FORMAT_R8G8_SRGB
  , pattern VK_FORMAT_R8G8B8_UNORM
  , pattern VK_FORMAT_R8G8B8_SNORM
  , pattern VK_FORMAT_R8G8B8_USCALED
  , pattern VK_FORMAT_R8G8B8_SSCALED
  , pattern VK_FORMAT_R8G8B8_UINT
  , pattern VK_FORMAT_R8G8B8_SINT
  , pattern VK_FORMAT_R8G8B8_SRGB
  , pattern VK_FORMAT_B8G8R8_UNORM
  , pattern VK_FORMAT_B8G8R8_SNORM
  , pattern VK_FORMAT_B8G8R8_USCALED
  , pattern VK_FORMAT_B8G8R8_SSCALED
  , pattern VK_FORMAT_B8G8R8_UINT
  , pattern VK_FORMAT_B8G8R8_SINT
  , pattern VK_FORMAT_B8G8R8_SRGB
  , pattern VK_FORMAT_R8G8B8A8_UNORM
  , pattern VK_FORMAT_R8G8B8A8_SNORM
  , pattern VK_FORMAT_R8G8B8A8_USCALED
  , pattern VK_FORMAT_R8G8B8A8_SSCALED
  , pattern VK_FORMAT_R8G8B8A8_UINT
  , pattern VK_FORMAT_R8G8B8A8_SINT
  , pattern VK_FORMAT_R8G8B8A8_SRGB
  , pattern VK_FORMAT_B8G8R8A8_UNORM
  , pattern VK_FORMAT_B8G8R8A8_SNORM
  , pattern VK_FORMAT_B8G8R8A8_USCALED
  , pattern VK_FORMAT_B8G8R8A8_SSCALED
  , pattern VK_FORMAT_B8G8R8A8_UINT
  , pattern VK_FORMAT_B8G8R8A8_SINT
  , pattern VK_FORMAT_B8G8R8A8_SRGB
  , pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32
  , pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32
  , pattern VK_FORMAT_A8B8G8R8_UINT_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SINT_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32
  , pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32
  , pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32
  , pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32
  , pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32
  , pattern VK_FORMAT_A2R10G10B10_UINT_PACK32
  , pattern VK_FORMAT_A2R10G10B10_SINT_PACK32
  , pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32
  , pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32
  , pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32
  , pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32
  , pattern VK_FORMAT_A2B10G10R10_UINT_PACK32
  , pattern VK_FORMAT_A2B10G10R10_SINT_PACK32
  , pattern VK_FORMAT_R16_UNORM
  , pattern VK_FORMAT_R16_SNORM
  , pattern VK_FORMAT_R16_USCALED
  , pattern VK_FORMAT_R16_SSCALED
  , pattern VK_FORMAT_R16_UINT
  , pattern VK_FORMAT_R16_SINT
  , pattern VK_FORMAT_R16_SFLOAT
  , pattern VK_FORMAT_R16G16_UNORM
  , pattern VK_FORMAT_R16G16_SNORM
  , pattern VK_FORMAT_R16G16_USCALED
  , pattern VK_FORMAT_R16G16_SSCALED
  , pattern VK_FORMAT_R16G16_UINT
  , pattern VK_FORMAT_R16G16_SINT
  , pattern VK_FORMAT_R16G16_SFLOAT
  , pattern VK_FORMAT_R16G16B16_UNORM
  , pattern VK_FORMAT_R16G16B16_SNORM
  , pattern VK_FORMAT_R16G16B16_USCALED
  , pattern VK_FORMAT_R16G16B16_SSCALED
  , pattern VK_FORMAT_R16G16B16_UINT
  , pattern VK_FORMAT_R16G16B16_SINT
  , pattern VK_FORMAT_R16G16B16_SFLOAT
  , pattern VK_FORMAT_R16G16B16A16_UNORM
  , pattern VK_FORMAT_R16G16B16A16_SNORM
  , pattern VK_FORMAT_R16G16B16A16_USCALED
  , pattern VK_FORMAT_R16G16B16A16_SSCALED
  , pattern VK_FORMAT_R16G16B16A16_UINT
  , pattern VK_FORMAT_R16G16B16A16_SINT
  , pattern VK_FORMAT_R16G16B16A16_SFLOAT
  , pattern VK_FORMAT_R32_UINT
  , pattern VK_FORMAT_R32_SINT
  , pattern VK_FORMAT_R32_SFLOAT
  , pattern VK_FORMAT_R32G32_UINT
  , pattern VK_FORMAT_R32G32_SINT
  , pattern VK_FORMAT_R32G32_SFLOAT
  , pattern VK_FORMAT_R32G32B32_UINT
  , pattern VK_FORMAT_R32G32B32_SINT
  , pattern VK_FORMAT_R32G32B32_SFLOAT
  , pattern VK_FORMAT_R32G32B32A32_UINT
  , pattern VK_FORMAT_R32G32B32A32_SINT
  , pattern VK_FORMAT_R32G32B32A32_SFLOAT
  , pattern VK_FORMAT_R64_UINT
  , pattern VK_FORMAT_R64_SINT
  , pattern VK_FORMAT_R64_SFLOAT
  , pattern VK_FORMAT_R64G64_UINT
  , pattern VK_FORMAT_R64G64_SINT
  , pattern VK_FORMAT_R64G64_SFLOAT
  , pattern VK_FORMAT_R64G64B64_UINT
  , pattern VK_FORMAT_R64G64B64_SINT
  , pattern VK_FORMAT_R64G64B64_SFLOAT
  , pattern VK_FORMAT_R64G64B64A64_UINT
  , pattern VK_FORMAT_R64G64B64A64_SINT
  , pattern VK_FORMAT_R64G64B64A64_SFLOAT
  , pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32
  , pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32
  , pattern VK_FORMAT_D16_UNORM
  , pattern VK_FORMAT_X8_D24_UNORM_PACK32
  , pattern VK_FORMAT_D32_SFLOAT
  , pattern VK_FORMAT_S8_UINT
  , pattern VK_FORMAT_D16_UNORM_S8_UINT
  , pattern VK_FORMAT_D24_UNORM_S8_UINT
  , pattern VK_FORMAT_D32_SFLOAT_S8_UINT
  , pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK
  , pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK
  , pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK
  , pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK
  , pattern VK_FORMAT_BC2_UNORM_BLOCK
  , pattern VK_FORMAT_BC2_SRGB_BLOCK
  , pattern VK_FORMAT_BC3_UNORM_BLOCK
  , pattern VK_FORMAT_BC3_SRGB_BLOCK
  , pattern VK_FORMAT_BC4_UNORM_BLOCK
  , pattern VK_FORMAT_BC4_SNORM_BLOCK
  , pattern VK_FORMAT_BC5_UNORM_BLOCK
  , pattern VK_FORMAT_BC5_SNORM_BLOCK
  , pattern VK_FORMAT_BC6H_UFLOAT_BLOCK
  , pattern VK_FORMAT_BC6H_SFLOAT_BLOCK
  , pattern VK_FORMAT_BC7_UNORM_BLOCK
  , pattern VK_FORMAT_BC7_SRGB_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK
  , pattern VK_FORMAT_EAC_R11_UNORM_BLOCK
  , pattern VK_FORMAT_EAC_R11_SNORM_BLOCK
  , pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK
  , pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK
  , pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK
  , VkObjectType(..)
  , pattern VK_OBJECT_TYPE_UNKNOWN
  , pattern VK_OBJECT_TYPE_INSTANCE
  , pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE
  , pattern VK_OBJECT_TYPE_DEVICE
  , pattern VK_OBJECT_TYPE_QUEUE
  , pattern VK_OBJECT_TYPE_SEMAPHORE
  , pattern VK_OBJECT_TYPE_COMMAND_BUFFER
  , pattern VK_OBJECT_TYPE_FENCE
  , pattern VK_OBJECT_TYPE_DEVICE_MEMORY
  , pattern VK_OBJECT_TYPE_BUFFER
  , pattern VK_OBJECT_TYPE_IMAGE
  , pattern VK_OBJECT_TYPE_EVENT
  , pattern VK_OBJECT_TYPE_QUERY_POOL
  , pattern VK_OBJECT_TYPE_BUFFER_VIEW
  , pattern VK_OBJECT_TYPE_IMAGE_VIEW
  , pattern VK_OBJECT_TYPE_SHADER_MODULE
  , pattern VK_OBJECT_TYPE_PIPELINE_CACHE
  , pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT
  , pattern VK_OBJECT_TYPE_RENDER_PASS
  , pattern VK_OBJECT_TYPE_PIPELINE
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT
  , pattern VK_OBJECT_TYPE_SAMPLER
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_SET
  , pattern VK_OBJECT_TYPE_FRAMEBUFFER
  , pattern VK_OBJECT_TYPE_COMMAND_POOL
  , VkResult(..)
  , pattern VK_SUCCESS
  , pattern VK_NOT_READY
  , pattern VK_TIMEOUT
  , pattern VK_EVENT_SET
  , pattern VK_EVENT_RESET
  , pattern VK_INCOMPLETE
  , pattern VK_ERROR_OUT_OF_HOST_MEMORY
  , pattern VK_ERROR_OUT_OF_DEVICE_MEMORY
  , pattern VK_ERROR_INITIALIZATION_FAILED
  , pattern VK_ERROR_DEVICE_LOST
  , pattern VK_ERROR_MEMORY_MAP_FAILED
  , pattern VK_ERROR_LAYER_NOT_PRESENT
  , pattern VK_ERROR_EXTENSION_NOT_PRESENT
  , pattern VK_ERROR_FEATURE_NOT_PRESENT
  , pattern VK_ERROR_INCOMPATIBLE_DRIVER
  , pattern VK_ERROR_TOO_MANY_OBJECTS
  , pattern VK_ERROR_FORMAT_NOT_SUPPORTED
  , pattern VK_ERROR_FRAGMENTED_POOL
  , VkStructureType(..)
  , pattern VK_STRUCTURE_TYPE_APPLICATION_INFO
  , pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
  , pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO
  , pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  , pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
  , pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO
  , VkVendorId(..)
  , pattern VK_VENDOR_ID_VIV
  , pattern VK_VENDOR_ID_VSI
  , pattern VK_VENDOR_ID_KAZAN
  , Zero(..)
  ) where

import Data.Int
  ( Int16
  , Int32
  , Int64
  , Int8
  )
import qualified Data.Vector.Storable.Sized
  ( Vector
  , replicate
  )
import Data.Word
  ( Word16
  , Word32
  , Word64
  , Word8
  )
import Foreign.C.Types
  ( CChar
  , CFloat
  , CInt
  , CSize
  )
import Foreign.Ptr
  ( Ptr
  , nullPtr
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
import GHC.TypeNats
  ( KnownNat
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





-- No documentation found for TopLevel "VkBaseInStructure"
data VkBaseInStructure = VkBaseInStructure
  { -- No documentation found for Nested "VkBaseInStructure" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBaseInStructure" "pNext"
  vkPNext :: Ptr VkBaseInStructure
  }
  deriving (Eq, Show)

instance Storable VkBaseInStructure where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkBaseInStructure <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBaseInStructure))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBaseInStructure))

instance Zero VkBaseInStructure where
  zero = VkBaseInStructure zero
                           zero

-- No documentation found for TopLevel "VkBaseOutStructure"
data VkBaseOutStructure = VkBaseOutStructure
  { -- No documentation found for Nested "VkBaseOutStructure" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBaseOutStructure" "pNext"
  vkPNext :: Ptr VkBaseOutStructure
  }
  deriving (Eq, Show)

instance Storable VkBaseOutStructure where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkBaseOutStructure <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBaseOutStructure))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBaseOutStructure))

instance Zero VkBaseOutStructure where
  zero = VkBaseOutStructure zero
                            zero

-- ** VkBool32

-- No documentation found for TopLevel "VkBool32"
newtype VkBool32 = VkBool32 Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkBool32 where
  showsPrec _ VK_FALSE = showString "VK_FALSE"
  showsPrec _ VK_TRUE = showString "VK_TRUE"
  showsPrec p (VkBool32 x) = showParen (p >= 11) (showString "VkBool32 " . showsPrec 11 x)

instance Read VkBool32 where
  readPrec = parens ( choose [ ("VK_FALSE", pure VK_FALSE)
                             , ("VK_TRUE",  pure VK_TRUE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBool32")
                        v <- step readPrec
                        pure (VkBool32 v)
                        )
                    )

-- No documentation found for Nested "VkBool32" "VK_FALSE"
pattern VK_FALSE :: VkBool32
pattern VK_FALSE = VkBool32 0

-- No documentation found for Nested "VkBool32" "VK_TRUE"
pattern VK_TRUE :: VkBool32
pattern VK_TRUE = VkBool32 1

-- No documentation found for TopLevel "VkFlags"
type VkFlags = Word32

-- ** VkFormat

-- No documentation found for TopLevel "VkFormat"
newtype VkFormat = VkFormat Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkFormat where
  showsPrec _ VK_FORMAT_UNDEFINED = showString "VK_FORMAT_UNDEFINED"
  showsPrec _ VK_FORMAT_R4G4_UNORM_PACK8 = showString "VK_FORMAT_R4G4_UNORM_PACK8"
  showsPrec _ VK_FORMAT_R4G4B4A4_UNORM_PACK16 = showString "VK_FORMAT_R4G4B4A4_UNORM_PACK16"
  showsPrec _ VK_FORMAT_B4G4R4A4_UNORM_PACK16 = showString "VK_FORMAT_B4G4R4A4_UNORM_PACK16"
  showsPrec _ VK_FORMAT_R5G6B5_UNORM_PACK16 = showString "VK_FORMAT_R5G6B5_UNORM_PACK16"
  showsPrec _ VK_FORMAT_B5G6R5_UNORM_PACK16 = showString "VK_FORMAT_B5G6R5_UNORM_PACK16"
  showsPrec _ VK_FORMAT_R5G5B5A1_UNORM_PACK16 = showString "VK_FORMAT_R5G5B5A1_UNORM_PACK16"
  showsPrec _ VK_FORMAT_B5G5R5A1_UNORM_PACK16 = showString "VK_FORMAT_B5G5R5A1_UNORM_PACK16"
  showsPrec _ VK_FORMAT_A1R5G5B5_UNORM_PACK16 = showString "VK_FORMAT_A1R5G5B5_UNORM_PACK16"
  showsPrec _ VK_FORMAT_R8_UNORM = showString "VK_FORMAT_R8_UNORM"
  showsPrec _ VK_FORMAT_R8_SNORM = showString "VK_FORMAT_R8_SNORM"
  showsPrec _ VK_FORMAT_R8_USCALED = showString "VK_FORMAT_R8_USCALED"
  showsPrec _ VK_FORMAT_R8_SSCALED = showString "VK_FORMAT_R8_SSCALED"
  showsPrec _ VK_FORMAT_R8_UINT = showString "VK_FORMAT_R8_UINT"
  showsPrec _ VK_FORMAT_R8_SINT = showString "VK_FORMAT_R8_SINT"
  showsPrec _ VK_FORMAT_R8_SRGB = showString "VK_FORMAT_R8_SRGB"
  showsPrec _ VK_FORMAT_R8G8_UNORM = showString "VK_FORMAT_R8G8_UNORM"
  showsPrec _ VK_FORMAT_R8G8_SNORM = showString "VK_FORMAT_R8G8_SNORM"
  showsPrec _ VK_FORMAT_R8G8_USCALED = showString "VK_FORMAT_R8G8_USCALED"
  showsPrec _ VK_FORMAT_R8G8_SSCALED = showString "VK_FORMAT_R8G8_SSCALED"
  showsPrec _ VK_FORMAT_R8G8_UINT = showString "VK_FORMAT_R8G8_UINT"
  showsPrec _ VK_FORMAT_R8G8_SINT = showString "VK_FORMAT_R8G8_SINT"
  showsPrec _ VK_FORMAT_R8G8_SRGB = showString "VK_FORMAT_R8G8_SRGB"
  showsPrec _ VK_FORMAT_R8G8B8_UNORM = showString "VK_FORMAT_R8G8B8_UNORM"
  showsPrec _ VK_FORMAT_R8G8B8_SNORM = showString "VK_FORMAT_R8G8B8_SNORM"
  showsPrec _ VK_FORMAT_R8G8B8_USCALED = showString "VK_FORMAT_R8G8B8_USCALED"
  showsPrec _ VK_FORMAT_R8G8B8_SSCALED = showString "VK_FORMAT_R8G8B8_SSCALED"
  showsPrec _ VK_FORMAT_R8G8B8_UINT = showString "VK_FORMAT_R8G8B8_UINT"
  showsPrec _ VK_FORMAT_R8G8B8_SINT = showString "VK_FORMAT_R8G8B8_SINT"
  showsPrec _ VK_FORMAT_R8G8B8_SRGB = showString "VK_FORMAT_R8G8B8_SRGB"
  showsPrec _ VK_FORMAT_B8G8R8_UNORM = showString "VK_FORMAT_B8G8R8_UNORM"
  showsPrec _ VK_FORMAT_B8G8R8_SNORM = showString "VK_FORMAT_B8G8R8_SNORM"
  showsPrec _ VK_FORMAT_B8G8R8_USCALED = showString "VK_FORMAT_B8G8R8_USCALED"
  showsPrec _ VK_FORMAT_B8G8R8_SSCALED = showString "VK_FORMAT_B8G8R8_SSCALED"
  showsPrec _ VK_FORMAT_B8G8R8_UINT = showString "VK_FORMAT_B8G8R8_UINT"
  showsPrec _ VK_FORMAT_B8G8R8_SINT = showString "VK_FORMAT_B8G8R8_SINT"
  showsPrec _ VK_FORMAT_B8G8R8_SRGB = showString "VK_FORMAT_B8G8R8_SRGB"
  showsPrec _ VK_FORMAT_R8G8B8A8_UNORM = showString "VK_FORMAT_R8G8B8A8_UNORM"
  showsPrec _ VK_FORMAT_R8G8B8A8_SNORM = showString "VK_FORMAT_R8G8B8A8_SNORM"
  showsPrec _ VK_FORMAT_R8G8B8A8_USCALED = showString "VK_FORMAT_R8G8B8A8_USCALED"
  showsPrec _ VK_FORMAT_R8G8B8A8_SSCALED = showString "VK_FORMAT_R8G8B8A8_SSCALED"
  showsPrec _ VK_FORMAT_R8G8B8A8_UINT = showString "VK_FORMAT_R8G8B8A8_UINT"
  showsPrec _ VK_FORMAT_R8G8B8A8_SINT = showString "VK_FORMAT_R8G8B8A8_SINT"
  showsPrec _ VK_FORMAT_R8G8B8A8_SRGB = showString "VK_FORMAT_R8G8B8A8_SRGB"
  showsPrec _ VK_FORMAT_B8G8R8A8_UNORM = showString "VK_FORMAT_B8G8R8A8_UNORM"
  showsPrec _ VK_FORMAT_B8G8R8A8_SNORM = showString "VK_FORMAT_B8G8R8A8_SNORM"
  showsPrec _ VK_FORMAT_B8G8R8A8_USCALED = showString "VK_FORMAT_B8G8R8A8_USCALED"
  showsPrec _ VK_FORMAT_B8G8R8A8_SSCALED = showString "VK_FORMAT_B8G8R8A8_SSCALED"
  showsPrec _ VK_FORMAT_B8G8R8A8_UINT = showString "VK_FORMAT_B8G8R8A8_UINT"
  showsPrec _ VK_FORMAT_B8G8R8A8_SINT = showString "VK_FORMAT_B8G8R8A8_SINT"
  showsPrec _ VK_FORMAT_B8G8R8A8_SRGB = showString "VK_FORMAT_B8G8R8A8_SRGB"
  showsPrec _ VK_FORMAT_A8B8G8R8_UNORM_PACK32 = showString "VK_FORMAT_A8B8G8R8_UNORM_PACK32"
  showsPrec _ VK_FORMAT_A8B8G8R8_SNORM_PACK32 = showString "VK_FORMAT_A8B8G8R8_SNORM_PACK32"
  showsPrec _ VK_FORMAT_A8B8G8R8_USCALED_PACK32 = showString "VK_FORMAT_A8B8G8R8_USCALED_PACK32"
  showsPrec _ VK_FORMAT_A8B8G8R8_SSCALED_PACK32 = showString "VK_FORMAT_A8B8G8R8_SSCALED_PACK32"
  showsPrec _ VK_FORMAT_A8B8G8R8_UINT_PACK32 = showString "VK_FORMAT_A8B8G8R8_UINT_PACK32"
  showsPrec _ VK_FORMAT_A8B8G8R8_SINT_PACK32 = showString "VK_FORMAT_A8B8G8R8_SINT_PACK32"
  showsPrec _ VK_FORMAT_A8B8G8R8_SRGB_PACK32 = showString "VK_FORMAT_A8B8G8R8_SRGB_PACK32"
  showsPrec _ VK_FORMAT_A2R10G10B10_UNORM_PACK32 = showString "VK_FORMAT_A2R10G10B10_UNORM_PACK32"
  showsPrec _ VK_FORMAT_A2R10G10B10_SNORM_PACK32 = showString "VK_FORMAT_A2R10G10B10_SNORM_PACK32"
  showsPrec _ VK_FORMAT_A2R10G10B10_USCALED_PACK32 = showString "VK_FORMAT_A2R10G10B10_USCALED_PACK32"
  showsPrec _ VK_FORMAT_A2R10G10B10_SSCALED_PACK32 = showString "VK_FORMAT_A2R10G10B10_SSCALED_PACK32"
  showsPrec _ VK_FORMAT_A2R10G10B10_UINT_PACK32 = showString "VK_FORMAT_A2R10G10B10_UINT_PACK32"
  showsPrec _ VK_FORMAT_A2R10G10B10_SINT_PACK32 = showString "VK_FORMAT_A2R10G10B10_SINT_PACK32"
  showsPrec _ VK_FORMAT_A2B10G10R10_UNORM_PACK32 = showString "VK_FORMAT_A2B10G10R10_UNORM_PACK32"
  showsPrec _ VK_FORMAT_A2B10G10R10_SNORM_PACK32 = showString "VK_FORMAT_A2B10G10R10_SNORM_PACK32"
  showsPrec _ VK_FORMAT_A2B10G10R10_USCALED_PACK32 = showString "VK_FORMAT_A2B10G10R10_USCALED_PACK32"
  showsPrec _ VK_FORMAT_A2B10G10R10_SSCALED_PACK32 = showString "VK_FORMAT_A2B10G10R10_SSCALED_PACK32"
  showsPrec _ VK_FORMAT_A2B10G10R10_UINT_PACK32 = showString "VK_FORMAT_A2B10G10R10_UINT_PACK32"
  showsPrec _ VK_FORMAT_A2B10G10R10_SINT_PACK32 = showString "VK_FORMAT_A2B10G10R10_SINT_PACK32"
  showsPrec _ VK_FORMAT_R16_UNORM = showString "VK_FORMAT_R16_UNORM"
  showsPrec _ VK_FORMAT_R16_SNORM = showString "VK_FORMAT_R16_SNORM"
  showsPrec _ VK_FORMAT_R16_USCALED = showString "VK_FORMAT_R16_USCALED"
  showsPrec _ VK_FORMAT_R16_SSCALED = showString "VK_FORMAT_R16_SSCALED"
  showsPrec _ VK_FORMAT_R16_UINT = showString "VK_FORMAT_R16_UINT"
  showsPrec _ VK_FORMAT_R16_SINT = showString "VK_FORMAT_R16_SINT"
  showsPrec _ VK_FORMAT_R16_SFLOAT = showString "VK_FORMAT_R16_SFLOAT"
  showsPrec _ VK_FORMAT_R16G16_UNORM = showString "VK_FORMAT_R16G16_UNORM"
  showsPrec _ VK_FORMAT_R16G16_SNORM = showString "VK_FORMAT_R16G16_SNORM"
  showsPrec _ VK_FORMAT_R16G16_USCALED = showString "VK_FORMAT_R16G16_USCALED"
  showsPrec _ VK_FORMAT_R16G16_SSCALED = showString "VK_FORMAT_R16G16_SSCALED"
  showsPrec _ VK_FORMAT_R16G16_UINT = showString "VK_FORMAT_R16G16_UINT"
  showsPrec _ VK_FORMAT_R16G16_SINT = showString "VK_FORMAT_R16G16_SINT"
  showsPrec _ VK_FORMAT_R16G16_SFLOAT = showString "VK_FORMAT_R16G16_SFLOAT"
  showsPrec _ VK_FORMAT_R16G16B16_UNORM = showString "VK_FORMAT_R16G16B16_UNORM"
  showsPrec _ VK_FORMAT_R16G16B16_SNORM = showString "VK_FORMAT_R16G16B16_SNORM"
  showsPrec _ VK_FORMAT_R16G16B16_USCALED = showString "VK_FORMAT_R16G16B16_USCALED"
  showsPrec _ VK_FORMAT_R16G16B16_SSCALED = showString "VK_FORMAT_R16G16B16_SSCALED"
  showsPrec _ VK_FORMAT_R16G16B16_UINT = showString "VK_FORMAT_R16G16B16_UINT"
  showsPrec _ VK_FORMAT_R16G16B16_SINT = showString "VK_FORMAT_R16G16B16_SINT"
  showsPrec _ VK_FORMAT_R16G16B16_SFLOAT = showString "VK_FORMAT_R16G16B16_SFLOAT"
  showsPrec _ VK_FORMAT_R16G16B16A16_UNORM = showString "VK_FORMAT_R16G16B16A16_UNORM"
  showsPrec _ VK_FORMAT_R16G16B16A16_SNORM = showString "VK_FORMAT_R16G16B16A16_SNORM"
  showsPrec _ VK_FORMAT_R16G16B16A16_USCALED = showString "VK_FORMAT_R16G16B16A16_USCALED"
  showsPrec _ VK_FORMAT_R16G16B16A16_SSCALED = showString "VK_FORMAT_R16G16B16A16_SSCALED"
  showsPrec _ VK_FORMAT_R16G16B16A16_UINT = showString "VK_FORMAT_R16G16B16A16_UINT"
  showsPrec _ VK_FORMAT_R16G16B16A16_SINT = showString "VK_FORMAT_R16G16B16A16_SINT"
  showsPrec _ VK_FORMAT_R16G16B16A16_SFLOAT = showString "VK_FORMAT_R16G16B16A16_SFLOAT"
  showsPrec _ VK_FORMAT_R32_UINT = showString "VK_FORMAT_R32_UINT"
  showsPrec _ VK_FORMAT_R32_SINT = showString "VK_FORMAT_R32_SINT"
  showsPrec _ VK_FORMAT_R32_SFLOAT = showString "VK_FORMAT_R32_SFLOAT"
  showsPrec _ VK_FORMAT_R32G32_UINT = showString "VK_FORMAT_R32G32_UINT"
  showsPrec _ VK_FORMAT_R32G32_SINT = showString "VK_FORMAT_R32G32_SINT"
  showsPrec _ VK_FORMAT_R32G32_SFLOAT = showString "VK_FORMAT_R32G32_SFLOAT"
  showsPrec _ VK_FORMAT_R32G32B32_UINT = showString "VK_FORMAT_R32G32B32_UINT"
  showsPrec _ VK_FORMAT_R32G32B32_SINT = showString "VK_FORMAT_R32G32B32_SINT"
  showsPrec _ VK_FORMAT_R32G32B32_SFLOAT = showString "VK_FORMAT_R32G32B32_SFLOAT"
  showsPrec _ VK_FORMAT_R32G32B32A32_UINT = showString "VK_FORMAT_R32G32B32A32_UINT"
  showsPrec _ VK_FORMAT_R32G32B32A32_SINT = showString "VK_FORMAT_R32G32B32A32_SINT"
  showsPrec _ VK_FORMAT_R32G32B32A32_SFLOAT = showString "VK_FORMAT_R32G32B32A32_SFLOAT"
  showsPrec _ VK_FORMAT_R64_UINT = showString "VK_FORMAT_R64_UINT"
  showsPrec _ VK_FORMAT_R64_SINT = showString "VK_FORMAT_R64_SINT"
  showsPrec _ VK_FORMAT_R64_SFLOAT = showString "VK_FORMAT_R64_SFLOAT"
  showsPrec _ VK_FORMAT_R64G64_UINT = showString "VK_FORMAT_R64G64_UINT"
  showsPrec _ VK_FORMAT_R64G64_SINT = showString "VK_FORMAT_R64G64_SINT"
  showsPrec _ VK_FORMAT_R64G64_SFLOAT = showString "VK_FORMAT_R64G64_SFLOAT"
  showsPrec _ VK_FORMAT_R64G64B64_UINT = showString "VK_FORMAT_R64G64B64_UINT"
  showsPrec _ VK_FORMAT_R64G64B64_SINT = showString "VK_FORMAT_R64G64B64_SINT"
  showsPrec _ VK_FORMAT_R64G64B64_SFLOAT = showString "VK_FORMAT_R64G64B64_SFLOAT"
  showsPrec _ VK_FORMAT_R64G64B64A64_UINT = showString "VK_FORMAT_R64G64B64A64_UINT"
  showsPrec _ VK_FORMAT_R64G64B64A64_SINT = showString "VK_FORMAT_R64G64B64A64_SINT"
  showsPrec _ VK_FORMAT_R64G64B64A64_SFLOAT = showString "VK_FORMAT_R64G64B64A64_SFLOAT"
  showsPrec _ VK_FORMAT_B10G11R11_UFLOAT_PACK32 = showString "VK_FORMAT_B10G11R11_UFLOAT_PACK32"
  showsPrec _ VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 = showString "VK_FORMAT_E5B9G9R9_UFLOAT_PACK32"
  showsPrec _ VK_FORMAT_D16_UNORM = showString "VK_FORMAT_D16_UNORM"
  showsPrec _ VK_FORMAT_X8_D24_UNORM_PACK32 = showString "VK_FORMAT_X8_D24_UNORM_PACK32"
  showsPrec _ VK_FORMAT_D32_SFLOAT = showString "VK_FORMAT_D32_SFLOAT"
  showsPrec _ VK_FORMAT_S8_UINT = showString "VK_FORMAT_S8_UINT"
  showsPrec _ VK_FORMAT_D16_UNORM_S8_UINT = showString "VK_FORMAT_D16_UNORM_S8_UINT"
  showsPrec _ VK_FORMAT_D24_UNORM_S8_UINT = showString "VK_FORMAT_D24_UNORM_S8_UINT"
  showsPrec _ VK_FORMAT_D32_SFLOAT_S8_UINT = showString "VK_FORMAT_D32_SFLOAT_S8_UINT"
  showsPrec _ VK_FORMAT_BC1_RGB_UNORM_BLOCK = showString "VK_FORMAT_BC1_RGB_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC1_RGB_SRGB_BLOCK = showString "VK_FORMAT_BC1_RGB_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_BC1_RGBA_UNORM_BLOCK = showString "VK_FORMAT_BC1_RGBA_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC1_RGBA_SRGB_BLOCK = showString "VK_FORMAT_BC1_RGBA_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_BC2_UNORM_BLOCK = showString "VK_FORMAT_BC2_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC2_SRGB_BLOCK = showString "VK_FORMAT_BC2_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_BC3_UNORM_BLOCK = showString "VK_FORMAT_BC3_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC3_SRGB_BLOCK = showString "VK_FORMAT_BC3_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_BC4_UNORM_BLOCK = showString "VK_FORMAT_BC4_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC4_SNORM_BLOCK = showString "VK_FORMAT_BC4_SNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC5_UNORM_BLOCK = showString "VK_FORMAT_BC5_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC5_SNORM_BLOCK = showString "VK_FORMAT_BC5_SNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC6H_UFLOAT_BLOCK = showString "VK_FORMAT_BC6H_UFLOAT_BLOCK"
  showsPrec _ VK_FORMAT_BC6H_SFLOAT_BLOCK = showString "VK_FORMAT_BC6H_SFLOAT_BLOCK"
  showsPrec _ VK_FORMAT_BC7_UNORM_BLOCK = showString "VK_FORMAT_BC7_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC7_SRGB_BLOCK = showString "VK_FORMAT_BC7_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK = showString "VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK = showString "VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK = showString "VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK = showString "VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = showString "VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK = showString "VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_EAC_R11_UNORM_BLOCK = showString "VK_FORMAT_EAC_R11_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_EAC_R11_SNORM_BLOCK = showString "VK_FORMAT_EAC_R11_SNORM_BLOCK"
  showsPrec _ VK_FORMAT_EAC_R11G11_UNORM_BLOCK = showString "VK_FORMAT_EAC_R11G11_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_EAC_R11G11_SNORM_BLOCK = showString "VK_FORMAT_EAC_R11G11_SNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_4x4_UNORM_BLOCK = showString "VK_FORMAT_ASTC_4x4_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_4x4_SRGB_BLOCK = showString "VK_FORMAT_ASTC_4x4_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_5x4_UNORM_BLOCK = showString "VK_FORMAT_ASTC_5x4_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_5x4_SRGB_BLOCK = showString "VK_FORMAT_ASTC_5x4_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_5x5_UNORM_BLOCK = showString "VK_FORMAT_ASTC_5x5_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_5x5_SRGB_BLOCK = showString "VK_FORMAT_ASTC_5x5_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_6x5_UNORM_BLOCK = showString "VK_FORMAT_ASTC_6x5_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_6x5_SRGB_BLOCK = showString "VK_FORMAT_ASTC_6x5_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_6x6_UNORM_BLOCK = showString "VK_FORMAT_ASTC_6x6_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_6x6_SRGB_BLOCK = showString "VK_FORMAT_ASTC_6x6_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_8x5_UNORM_BLOCK = showString "VK_FORMAT_ASTC_8x5_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_8x5_SRGB_BLOCK = showString "VK_FORMAT_ASTC_8x5_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_8x6_UNORM_BLOCK = showString "VK_FORMAT_ASTC_8x6_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_8x6_SRGB_BLOCK = showString "VK_FORMAT_ASTC_8x6_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_8x8_UNORM_BLOCK = showString "VK_FORMAT_ASTC_8x8_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_8x8_SRGB_BLOCK = showString "VK_FORMAT_ASTC_8x8_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x5_UNORM_BLOCK = showString "VK_FORMAT_ASTC_10x5_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x5_SRGB_BLOCK = showString "VK_FORMAT_ASTC_10x5_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x6_UNORM_BLOCK = showString "VK_FORMAT_ASTC_10x6_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x6_SRGB_BLOCK = showString "VK_FORMAT_ASTC_10x6_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x8_UNORM_BLOCK = showString "VK_FORMAT_ASTC_10x8_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x8_SRGB_BLOCK = showString "VK_FORMAT_ASTC_10x8_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x10_UNORM_BLOCK = showString "VK_FORMAT_ASTC_10x10_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x10_SRGB_BLOCK = showString "VK_FORMAT_ASTC_10x10_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_12x10_UNORM_BLOCK = showString "VK_FORMAT_ASTC_12x10_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_12x10_SRGB_BLOCK = showString "VK_FORMAT_ASTC_12x10_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_12x12_UNORM_BLOCK = showString "VK_FORMAT_ASTC_12x12_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_12x12_SRGB_BLOCK = showString "VK_FORMAT_ASTC_12x12_SRGB_BLOCK"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkFormat 1000156000) = showString "VK_FORMAT_G8B8G8R8_422_UNORM"
  showsPrec _ (VkFormat 1000156001) = showString "VK_FORMAT_B8G8R8G8_422_UNORM"
  showsPrec _ (VkFormat 1000156002) = showString "VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM"
  showsPrec _ (VkFormat 1000156003) = showString "VK_FORMAT_G8_B8R8_2PLANE_420_UNORM"
  showsPrec _ (VkFormat 1000156004) = showString "VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM"
  showsPrec _ (VkFormat 1000156005) = showString "VK_FORMAT_G8_B8R8_2PLANE_422_UNORM"
  showsPrec _ (VkFormat 1000156006) = showString "VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM"
  showsPrec _ (VkFormat 1000156007) = showString "VK_FORMAT_R10X6_UNORM_PACK16"
  showsPrec _ (VkFormat 1000156008) = showString "VK_FORMAT_R10X6G10X6_UNORM_2PACK16"
  showsPrec _ (VkFormat 1000156009) = showString "VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16"
  showsPrec _ (VkFormat 1000156010) = showString "VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16"
  showsPrec _ (VkFormat 1000156011) = showString "VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16"
  showsPrec _ (VkFormat 1000156012) = showString "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156013) = showString "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156014) = showString "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156015) = showString "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156016) = showString "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156017) = showString "VK_FORMAT_R12X4_UNORM_PACK16"
  showsPrec _ (VkFormat 1000156018) = showString "VK_FORMAT_R12X4G12X4_UNORM_2PACK16"
  showsPrec _ (VkFormat 1000156019) = showString "VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16"
  showsPrec _ (VkFormat 1000156020) = showString "VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16"
  showsPrec _ (VkFormat 1000156021) = showString "VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16"
  showsPrec _ (VkFormat 1000156022) = showString "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156023) = showString "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156024) = showString "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156025) = showString "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156026) = showString "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156027) = showString "VK_FORMAT_G16B16G16R16_422_UNORM"
  showsPrec _ (VkFormat 1000156028) = showString "VK_FORMAT_B16G16R16G16_422_UNORM"
  showsPrec _ (VkFormat 1000156029) = showString "VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM"
  showsPrec _ (VkFormat 1000156030) = showString "VK_FORMAT_G16_B16R16_2PLANE_420_UNORM"
  showsPrec _ (VkFormat 1000156031) = showString "VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM"
  showsPrec _ (VkFormat 1000156032) = showString "VK_FORMAT_G16_B16R16_2PLANE_422_UNORM"
  showsPrec _ (VkFormat 1000156033) = showString "VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM"
  showsPrec _ (VkFormat 1000054000) = showString "VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054001) = showString "VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054002) = showString "VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054003) = showString "VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054004) = showString "VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054005) = showString "VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054006) = showString "VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054007) = showString "VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG"
  showsPrec p (VkFormat x) = showParen (p >= 11) (showString "VkFormat " . showsPrec 11 x)

instance Read VkFormat where
  readPrec = parens ( choose [ ("VK_FORMAT_UNDEFINED",                  pure VK_FORMAT_UNDEFINED)
                             , ("VK_FORMAT_R4G4_UNORM_PACK8",           pure VK_FORMAT_R4G4_UNORM_PACK8)
                             , ("VK_FORMAT_R4G4B4A4_UNORM_PACK16",      pure VK_FORMAT_R4G4B4A4_UNORM_PACK16)
                             , ("VK_FORMAT_B4G4R4A4_UNORM_PACK16",      pure VK_FORMAT_B4G4R4A4_UNORM_PACK16)
                             , ("VK_FORMAT_R5G6B5_UNORM_PACK16",        pure VK_FORMAT_R5G6B5_UNORM_PACK16)
                             , ("VK_FORMAT_B5G6R5_UNORM_PACK16",        pure VK_FORMAT_B5G6R5_UNORM_PACK16)
                             , ("VK_FORMAT_R5G5B5A1_UNORM_PACK16",      pure VK_FORMAT_R5G5B5A1_UNORM_PACK16)
                             , ("VK_FORMAT_B5G5R5A1_UNORM_PACK16",      pure VK_FORMAT_B5G5R5A1_UNORM_PACK16)
                             , ("VK_FORMAT_A1R5G5B5_UNORM_PACK16",      pure VK_FORMAT_A1R5G5B5_UNORM_PACK16)
                             , ("VK_FORMAT_R8_UNORM",                   pure VK_FORMAT_R8_UNORM)
                             , ("VK_FORMAT_R8_SNORM",                   pure VK_FORMAT_R8_SNORM)
                             , ("VK_FORMAT_R8_USCALED",                 pure VK_FORMAT_R8_USCALED)
                             , ("VK_FORMAT_R8_SSCALED",                 pure VK_FORMAT_R8_SSCALED)
                             , ("VK_FORMAT_R8_UINT",                    pure VK_FORMAT_R8_UINT)
                             , ("VK_FORMAT_R8_SINT",                    pure VK_FORMAT_R8_SINT)
                             , ("VK_FORMAT_R8_SRGB",                    pure VK_FORMAT_R8_SRGB)
                             , ("VK_FORMAT_R8G8_UNORM",                 pure VK_FORMAT_R8G8_UNORM)
                             , ("VK_FORMAT_R8G8_SNORM",                 pure VK_FORMAT_R8G8_SNORM)
                             , ("VK_FORMAT_R8G8_USCALED",               pure VK_FORMAT_R8G8_USCALED)
                             , ("VK_FORMAT_R8G8_SSCALED",               pure VK_FORMAT_R8G8_SSCALED)
                             , ("VK_FORMAT_R8G8_UINT",                  pure VK_FORMAT_R8G8_UINT)
                             , ("VK_FORMAT_R8G8_SINT",                  pure VK_FORMAT_R8G8_SINT)
                             , ("VK_FORMAT_R8G8_SRGB",                  pure VK_FORMAT_R8G8_SRGB)
                             , ("VK_FORMAT_R8G8B8_UNORM",               pure VK_FORMAT_R8G8B8_UNORM)
                             , ("VK_FORMAT_R8G8B8_SNORM",               pure VK_FORMAT_R8G8B8_SNORM)
                             , ("VK_FORMAT_R8G8B8_USCALED",             pure VK_FORMAT_R8G8B8_USCALED)
                             , ("VK_FORMAT_R8G8B8_SSCALED",             pure VK_FORMAT_R8G8B8_SSCALED)
                             , ("VK_FORMAT_R8G8B8_UINT",                pure VK_FORMAT_R8G8B8_UINT)
                             , ("VK_FORMAT_R8G8B8_SINT",                pure VK_FORMAT_R8G8B8_SINT)
                             , ("VK_FORMAT_R8G8B8_SRGB",                pure VK_FORMAT_R8G8B8_SRGB)
                             , ("VK_FORMAT_B8G8R8_UNORM",               pure VK_FORMAT_B8G8R8_UNORM)
                             , ("VK_FORMAT_B8G8R8_SNORM",               pure VK_FORMAT_B8G8R8_SNORM)
                             , ("VK_FORMAT_B8G8R8_USCALED",             pure VK_FORMAT_B8G8R8_USCALED)
                             , ("VK_FORMAT_B8G8R8_SSCALED",             pure VK_FORMAT_B8G8R8_SSCALED)
                             , ("VK_FORMAT_B8G8R8_UINT",                pure VK_FORMAT_B8G8R8_UINT)
                             , ("VK_FORMAT_B8G8R8_SINT",                pure VK_FORMAT_B8G8R8_SINT)
                             , ("VK_FORMAT_B8G8R8_SRGB",                pure VK_FORMAT_B8G8R8_SRGB)
                             , ("VK_FORMAT_R8G8B8A8_UNORM",             pure VK_FORMAT_R8G8B8A8_UNORM)
                             , ("VK_FORMAT_R8G8B8A8_SNORM",             pure VK_FORMAT_R8G8B8A8_SNORM)
                             , ("VK_FORMAT_R8G8B8A8_USCALED",           pure VK_FORMAT_R8G8B8A8_USCALED)
                             , ("VK_FORMAT_R8G8B8A8_SSCALED",           pure VK_FORMAT_R8G8B8A8_SSCALED)
                             , ("VK_FORMAT_R8G8B8A8_UINT",              pure VK_FORMAT_R8G8B8A8_UINT)
                             , ("VK_FORMAT_R8G8B8A8_SINT",              pure VK_FORMAT_R8G8B8A8_SINT)
                             , ("VK_FORMAT_R8G8B8A8_SRGB",              pure VK_FORMAT_R8G8B8A8_SRGB)
                             , ("VK_FORMAT_B8G8R8A8_UNORM",             pure VK_FORMAT_B8G8R8A8_UNORM)
                             , ("VK_FORMAT_B8G8R8A8_SNORM",             pure VK_FORMAT_B8G8R8A8_SNORM)
                             , ("VK_FORMAT_B8G8R8A8_USCALED",           pure VK_FORMAT_B8G8R8A8_USCALED)
                             , ("VK_FORMAT_B8G8R8A8_SSCALED",           pure VK_FORMAT_B8G8R8A8_SSCALED)
                             , ("VK_FORMAT_B8G8R8A8_UINT",              pure VK_FORMAT_B8G8R8A8_UINT)
                             , ("VK_FORMAT_B8G8R8A8_SINT",              pure VK_FORMAT_B8G8R8A8_SINT)
                             , ("VK_FORMAT_B8G8R8A8_SRGB",              pure VK_FORMAT_B8G8R8A8_SRGB)
                             , ("VK_FORMAT_A8B8G8R8_UNORM_PACK32",      pure VK_FORMAT_A8B8G8R8_UNORM_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_SNORM_PACK32",      pure VK_FORMAT_A8B8G8R8_SNORM_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_USCALED_PACK32",    pure VK_FORMAT_A8B8G8R8_USCALED_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_SSCALED_PACK32",    pure VK_FORMAT_A8B8G8R8_SSCALED_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_UINT_PACK32",       pure VK_FORMAT_A8B8G8R8_UINT_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_SINT_PACK32",       pure VK_FORMAT_A8B8G8R8_SINT_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_SRGB_PACK32",       pure VK_FORMAT_A8B8G8R8_SRGB_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_UNORM_PACK32",   pure VK_FORMAT_A2R10G10B10_UNORM_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_SNORM_PACK32",   pure VK_FORMAT_A2R10G10B10_SNORM_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_USCALED_PACK32", pure VK_FORMAT_A2R10G10B10_USCALED_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_SSCALED_PACK32", pure VK_FORMAT_A2R10G10B10_SSCALED_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_UINT_PACK32",    pure VK_FORMAT_A2R10G10B10_UINT_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_SINT_PACK32",    pure VK_FORMAT_A2R10G10B10_SINT_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_UNORM_PACK32",   pure VK_FORMAT_A2B10G10R10_UNORM_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_SNORM_PACK32",   pure VK_FORMAT_A2B10G10R10_SNORM_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_USCALED_PACK32", pure VK_FORMAT_A2B10G10R10_USCALED_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_SSCALED_PACK32", pure VK_FORMAT_A2B10G10R10_SSCALED_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_UINT_PACK32",    pure VK_FORMAT_A2B10G10R10_UINT_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_SINT_PACK32",    pure VK_FORMAT_A2B10G10R10_SINT_PACK32)
                             , ("VK_FORMAT_R16_UNORM",                  pure VK_FORMAT_R16_UNORM)
                             , ("VK_FORMAT_R16_SNORM",                  pure VK_FORMAT_R16_SNORM)
                             , ("VK_FORMAT_R16_USCALED",                pure VK_FORMAT_R16_USCALED)
                             , ("VK_FORMAT_R16_SSCALED",                pure VK_FORMAT_R16_SSCALED)
                             , ("VK_FORMAT_R16_UINT",                   pure VK_FORMAT_R16_UINT)
                             , ("VK_FORMAT_R16_SINT",                   pure VK_FORMAT_R16_SINT)
                             , ("VK_FORMAT_R16_SFLOAT",                 pure VK_FORMAT_R16_SFLOAT)
                             , ("VK_FORMAT_R16G16_UNORM",               pure VK_FORMAT_R16G16_UNORM)
                             , ("VK_FORMAT_R16G16_SNORM",               pure VK_FORMAT_R16G16_SNORM)
                             , ("VK_FORMAT_R16G16_USCALED",             pure VK_FORMAT_R16G16_USCALED)
                             , ("VK_FORMAT_R16G16_SSCALED",             pure VK_FORMAT_R16G16_SSCALED)
                             , ("VK_FORMAT_R16G16_UINT",                pure VK_FORMAT_R16G16_UINT)
                             , ("VK_FORMAT_R16G16_SINT",                pure VK_FORMAT_R16G16_SINT)
                             , ("VK_FORMAT_R16G16_SFLOAT",              pure VK_FORMAT_R16G16_SFLOAT)
                             , ("VK_FORMAT_R16G16B16_UNORM",            pure VK_FORMAT_R16G16B16_UNORM)
                             , ("VK_FORMAT_R16G16B16_SNORM",            pure VK_FORMAT_R16G16B16_SNORM)
                             , ("VK_FORMAT_R16G16B16_USCALED",          pure VK_FORMAT_R16G16B16_USCALED)
                             , ("VK_FORMAT_R16G16B16_SSCALED",          pure VK_FORMAT_R16G16B16_SSCALED)
                             , ("VK_FORMAT_R16G16B16_UINT",             pure VK_FORMAT_R16G16B16_UINT)
                             , ("VK_FORMAT_R16G16B16_SINT",             pure VK_FORMAT_R16G16B16_SINT)
                             , ("VK_FORMAT_R16G16B16_SFLOAT",           pure VK_FORMAT_R16G16B16_SFLOAT)
                             , ("VK_FORMAT_R16G16B16A16_UNORM",         pure VK_FORMAT_R16G16B16A16_UNORM)
                             , ("VK_FORMAT_R16G16B16A16_SNORM",         pure VK_FORMAT_R16G16B16A16_SNORM)
                             , ("VK_FORMAT_R16G16B16A16_USCALED",       pure VK_FORMAT_R16G16B16A16_USCALED)
                             , ("VK_FORMAT_R16G16B16A16_SSCALED",       pure VK_FORMAT_R16G16B16A16_SSCALED)
                             , ("VK_FORMAT_R16G16B16A16_UINT",          pure VK_FORMAT_R16G16B16A16_UINT)
                             , ("VK_FORMAT_R16G16B16A16_SINT",          pure VK_FORMAT_R16G16B16A16_SINT)
                             , ("VK_FORMAT_R16G16B16A16_SFLOAT",        pure VK_FORMAT_R16G16B16A16_SFLOAT)
                             , ("VK_FORMAT_R32_UINT",                   pure VK_FORMAT_R32_UINT)
                             , ("VK_FORMAT_R32_SINT",                   pure VK_FORMAT_R32_SINT)
                             , ("VK_FORMAT_R32_SFLOAT",                 pure VK_FORMAT_R32_SFLOAT)
                             , ("VK_FORMAT_R32G32_UINT",                pure VK_FORMAT_R32G32_UINT)
                             , ("VK_FORMAT_R32G32_SINT",                pure VK_FORMAT_R32G32_SINT)
                             , ("VK_FORMAT_R32G32_SFLOAT",              pure VK_FORMAT_R32G32_SFLOAT)
                             , ("VK_FORMAT_R32G32B32_UINT",             pure VK_FORMAT_R32G32B32_UINT)
                             , ("VK_FORMAT_R32G32B32_SINT",             pure VK_FORMAT_R32G32B32_SINT)
                             , ("VK_FORMAT_R32G32B32_SFLOAT",           pure VK_FORMAT_R32G32B32_SFLOAT)
                             , ("VK_FORMAT_R32G32B32A32_UINT",          pure VK_FORMAT_R32G32B32A32_UINT)
                             , ("VK_FORMAT_R32G32B32A32_SINT",          pure VK_FORMAT_R32G32B32A32_SINT)
                             , ("VK_FORMAT_R32G32B32A32_SFLOAT",        pure VK_FORMAT_R32G32B32A32_SFLOAT)
                             , ("VK_FORMAT_R64_UINT",                   pure VK_FORMAT_R64_UINT)
                             , ("VK_FORMAT_R64_SINT",                   pure VK_FORMAT_R64_SINT)
                             , ("VK_FORMAT_R64_SFLOAT",                 pure VK_FORMAT_R64_SFLOAT)
                             , ("VK_FORMAT_R64G64_UINT",                pure VK_FORMAT_R64G64_UINT)
                             , ("VK_FORMAT_R64G64_SINT",                pure VK_FORMAT_R64G64_SINT)
                             , ("VK_FORMAT_R64G64_SFLOAT",              pure VK_FORMAT_R64G64_SFLOAT)
                             , ("VK_FORMAT_R64G64B64_UINT",             pure VK_FORMAT_R64G64B64_UINT)
                             , ("VK_FORMAT_R64G64B64_SINT",             pure VK_FORMAT_R64G64B64_SINT)
                             , ("VK_FORMAT_R64G64B64_SFLOAT",           pure VK_FORMAT_R64G64B64_SFLOAT)
                             , ("VK_FORMAT_R64G64B64A64_UINT",          pure VK_FORMAT_R64G64B64A64_UINT)
                             , ("VK_FORMAT_R64G64B64A64_SINT",          pure VK_FORMAT_R64G64B64A64_SINT)
                             , ("VK_FORMAT_R64G64B64A64_SFLOAT",        pure VK_FORMAT_R64G64B64A64_SFLOAT)
                             , ("VK_FORMAT_B10G11R11_UFLOAT_PACK32",    pure VK_FORMAT_B10G11R11_UFLOAT_PACK32)
                             , ("VK_FORMAT_E5B9G9R9_UFLOAT_PACK32",     pure VK_FORMAT_E5B9G9R9_UFLOAT_PACK32)
                             , ("VK_FORMAT_D16_UNORM",                  pure VK_FORMAT_D16_UNORM)
                             , ("VK_FORMAT_X8_D24_UNORM_PACK32",        pure VK_FORMAT_X8_D24_UNORM_PACK32)
                             , ("VK_FORMAT_D32_SFLOAT",                 pure VK_FORMAT_D32_SFLOAT)
                             , ("VK_FORMAT_S8_UINT",                    pure VK_FORMAT_S8_UINT)
                             , ("VK_FORMAT_D16_UNORM_S8_UINT",          pure VK_FORMAT_D16_UNORM_S8_UINT)
                             , ("VK_FORMAT_D24_UNORM_S8_UINT",          pure VK_FORMAT_D24_UNORM_S8_UINT)
                             , ("VK_FORMAT_D32_SFLOAT_S8_UINT",         pure VK_FORMAT_D32_SFLOAT_S8_UINT)
                             , ("VK_FORMAT_BC1_RGB_UNORM_BLOCK",        pure VK_FORMAT_BC1_RGB_UNORM_BLOCK)
                             , ("VK_FORMAT_BC1_RGB_SRGB_BLOCK",         pure VK_FORMAT_BC1_RGB_SRGB_BLOCK)
                             , ("VK_FORMAT_BC1_RGBA_UNORM_BLOCK",       pure VK_FORMAT_BC1_RGBA_UNORM_BLOCK)
                             , ("VK_FORMAT_BC1_RGBA_SRGB_BLOCK",        pure VK_FORMAT_BC1_RGBA_SRGB_BLOCK)
                             , ("VK_FORMAT_BC2_UNORM_BLOCK",            pure VK_FORMAT_BC2_UNORM_BLOCK)
                             , ("VK_FORMAT_BC2_SRGB_BLOCK",             pure VK_FORMAT_BC2_SRGB_BLOCK)
                             , ("VK_FORMAT_BC3_UNORM_BLOCK",            pure VK_FORMAT_BC3_UNORM_BLOCK)
                             , ("VK_FORMAT_BC3_SRGB_BLOCK",             pure VK_FORMAT_BC3_SRGB_BLOCK)
                             , ("VK_FORMAT_BC4_UNORM_BLOCK",            pure VK_FORMAT_BC4_UNORM_BLOCK)
                             , ("VK_FORMAT_BC4_SNORM_BLOCK",            pure VK_FORMAT_BC4_SNORM_BLOCK)
                             , ("VK_FORMAT_BC5_UNORM_BLOCK",            pure VK_FORMAT_BC5_UNORM_BLOCK)
                             , ("VK_FORMAT_BC5_SNORM_BLOCK",            pure VK_FORMAT_BC5_SNORM_BLOCK)
                             , ("VK_FORMAT_BC6H_UFLOAT_BLOCK",          pure VK_FORMAT_BC6H_UFLOAT_BLOCK)
                             , ("VK_FORMAT_BC6H_SFLOAT_BLOCK",          pure VK_FORMAT_BC6H_SFLOAT_BLOCK)
                             , ("VK_FORMAT_BC7_UNORM_BLOCK",            pure VK_FORMAT_BC7_UNORM_BLOCK)
                             , ("VK_FORMAT_BC7_SRGB_BLOCK",             pure VK_FORMAT_BC7_SRGB_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK",    pure VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK",     pure VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK",  pure VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK",   pure VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK",  pure VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK",   pure VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK)
                             , ("VK_FORMAT_EAC_R11_UNORM_BLOCK",        pure VK_FORMAT_EAC_R11_UNORM_BLOCK)
                             , ("VK_FORMAT_EAC_R11_SNORM_BLOCK",        pure VK_FORMAT_EAC_R11_SNORM_BLOCK)
                             , ("VK_FORMAT_EAC_R11G11_UNORM_BLOCK",     pure VK_FORMAT_EAC_R11G11_UNORM_BLOCK)
                             , ("VK_FORMAT_EAC_R11G11_SNORM_BLOCK",     pure VK_FORMAT_EAC_R11G11_SNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_4x4_UNORM_BLOCK",       pure VK_FORMAT_ASTC_4x4_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_4x4_SRGB_BLOCK",        pure VK_FORMAT_ASTC_4x4_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_5x4_UNORM_BLOCK",       pure VK_FORMAT_ASTC_5x4_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_5x4_SRGB_BLOCK",        pure VK_FORMAT_ASTC_5x4_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_5x5_UNORM_BLOCK",       pure VK_FORMAT_ASTC_5x5_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_5x5_SRGB_BLOCK",        pure VK_FORMAT_ASTC_5x5_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_6x5_UNORM_BLOCK",       pure VK_FORMAT_ASTC_6x5_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_6x5_SRGB_BLOCK",        pure VK_FORMAT_ASTC_6x5_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_6x6_UNORM_BLOCK",       pure VK_FORMAT_ASTC_6x6_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_6x6_SRGB_BLOCK",        pure VK_FORMAT_ASTC_6x6_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_8x5_UNORM_BLOCK",       pure VK_FORMAT_ASTC_8x5_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_8x5_SRGB_BLOCK",        pure VK_FORMAT_ASTC_8x5_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_8x6_UNORM_BLOCK",       pure VK_FORMAT_ASTC_8x6_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_8x6_SRGB_BLOCK",        pure VK_FORMAT_ASTC_8x6_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_8x8_UNORM_BLOCK",       pure VK_FORMAT_ASTC_8x8_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_8x8_SRGB_BLOCK",        pure VK_FORMAT_ASTC_8x8_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_10x5_UNORM_BLOCK",      pure VK_FORMAT_ASTC_10x5_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_10x5_SRGB_BLOCK",       pure VK_FORMAT_ASTC_10x5_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_10x6_UNORM_BLOCK",      pure VK_FORMAT_ASTC_10x6_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_10x6_SRGB_BLOCK",       pure VK_FORMAT_ASTC_10x6_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_10x8_UNORM_BLOCK",      pure VK_FORMAT_ASTC_10x8_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_10x8_SRGB_BLOCK",       pure VK_FORMAT_ASTC_10x8_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_10x10_UNORM_BLOCK",     pure VK_FORMAT_ASTC_10x10_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_10x10_SRGB_BLOCK",      pure VK_FORMAT_ASTC_10x10_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_12x10_UNORM_BLOCK",     pure VK_FORMAT_ASTC_12x10_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_12x10_SRGB_BLOCK",      pure VK_FORMAT_ASTC_12x10_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_12x12_UNORM_BLOCK",     pure VK_FORMAT_ASTC_12x12_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_12x12_SRGB_BLOCK",      pure VK_FORMAT_ASTC_12x12_SRGB_BLOCK)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_FORMAT_G8B8G8R8_422_UNORM",                         pure (VkFormat 1000156000))
                             , ("VK_FORMAT_B8G8R8G8_422_UNORM",                         pure (VkFormat 1000156001))
                             , ("VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM",                  pure (VkFormat 1000156002))
                             , ("VK_FORMAT_G8_B8R8_2PLANE_420_UNORM",                   pure (VkFormat 1000156003))
                             , ("VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM",                  pure (VkFormat 1000156004))
                             , ("VK_FORMAT_G8_B8R8_2PLANE_422_UNORM",                   pure (VkFormat 1000156005))
                             , ("VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM",                  pure (VkFormat 1000156006))
                             , ("VK_FORMAT_R10X6_UNORM_PACK16",                         pure (VkFormat 1000156007))
                             , ("VK_FORMAT_R10X6G10X6_UNORM_2PACK16",                   pure (VkFormat 1000156008))
                             , ("VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16",         pure (VkFormat 1000156009))
                             , ("VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16",     pure (VkFormat 1000156010))
                             , ("VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16",     pure (VkFormat 1000156011))
                             , ("VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16", pure (VkFormat 1000156012))
                             , ("VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16",  pure (VkFormat 1000156013))
                             , ("VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16", pure (VkFormat 1000156014))
                             , ("VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16",  pure (VkFormat 1000156015))
                             , ("VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16", pure (VkFormat 1000156016))
                             , ("VK_FORMAT_R12X4_UNORM_PACK16",                         pure (VkFormat 1000156017))
                             , ("VK_FORMAT_R12X4G12X4_UNORM_2PACK16",                   pure (VkFormat 1000156018))
                             , ("VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16",         pure (VkFormat 1000156019))
                             , ("VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16",     pure (VkFormat 1000156020))
                             , ("VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16",     pure (VkFormat 1000156021))
                             , ("VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16", pure (VkFormat 1000156022))
                             , ("VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16",  pure (VkFormat 1000156023))
                             , ("VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16", pure (VkFormat 1000156024))
                             , ("VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16",  pure (VkFormat 1000156025))
                             , ("VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16", pure (VkFormat 1000156026))
                             , ("VK_FORMAT_G16B16G16R16_422_UNORM",                     pure (VkFormat 1000156027))
                             , ("VK_FORMAT_B16G16R16G16_422_UNORM",                     pure (VkFormat 1000156028))
                             , ("VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM",               pure (VkFormat 1000156029))
                             , ("VK_FORMAT_G16_B16R16_2PLANE_420_UNORM",                pure (VkFormat 1000156030))
                             , ("VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM",               pure (VkFormat 1000156031))
                             , ("VK_FORMAT_G16_B16R16_2PLANE_422_UNORM",                pure (VkFormat 1000156032))
                             , ("VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM",               pure (VkFormat 1000156033))
                             , ("VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG",                pure (VkFormat 1000054000))
                             , ("VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG",                pure (VkFormat 1000054001))
                             , ("VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG",                pure (VkFormat 1000054002))
                             , ("VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG",                pure (VkFormat 1000054003))
                             , ("VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG",                 pure (VkFormat 1000054004))
                             , ("VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG",                 pure (VkFormat 1000054005))
                             , ("VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG",                 pure (VkFormat 1000054006))
                             , ("VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG",                 pure (VkFormat 1000054007))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFormat")
                        v <- step readPrec
                        pure (VkFormat v)
                        )
                    )

-- No documentation found for Nested "VkFormat" "VK_FORMAT_UNDEFINED"
pattern VK_FORMAT_UNDEFINED :: VkFormat
pattern VK_FORMAT_UNDEFINED = VkFormat 0

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R4G4_UNORM_PACK8"
pattern VK_FORMAT_R4G4_UNORM_PACK8 :: VkFormat
pattern VK_FORMAT_R4G4_UNORM_PACK8 = VkFormat 1

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R4G4B4A4_UNORM_PACK16"
pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16 = VkFormat 2

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B4G4R4A4_UNORM_PACK16"
pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16 = VkFormat 3

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R5G6B5_UNORM_PACK16"
pattern VK_FORMAT_R5G6B5_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R5G6B5_UNORM_PACK16 = VkFormat 4

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B5G6R5_UNORM_PACK16"
pattern VK_FORMAT_B5G6R5_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_B5G6R5_UNORM_PACK16 = VkFormat 5

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R5G5B5A1_UNORM_PACK16"
pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16 = VkFormat 6

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B5G5R5A1_UNORM_PACK16"
pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16 = VkFormat 7

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A1R5G5B5_UNORM_PACK16"
pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16 = VkFormat 8

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_UNORM"
pattern VK_FORMAT_R8_UNORM :: VkFormat
pattern VK_FORMAT_R8_UNORM = VkFormat 9

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_SNORM"
pattern VK_FORMAT_R8_SNORM :: VkFormat
pattern VK_FORMAT_R8_SNORM = VkFormat 10

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_USCALED"
pattern VK_FORMAT_R8_USCALED :: VkFormat
pattern VK_FORMAT_R8_USCALED = VkFormat 11

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_SSCALED"
pattern VK_FORMAT_R8_SSCALED :: VkFormat
pattern VK_FORMAT_R8_SSCALED = VkFormat 12

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_UINT"
pattern VK_FORMAT_R8_UINT :: VkFormat
pattern VK_FORMAT_R8_UINT = VkFormat 13

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_SINT"
pattern VK_FORMAT_R8_SINT :: VkFormat
pattern VK_FORMAT_R8_SINT = VkFormat 14

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_SRGB"
pattern VK_FORMAT_R8_SRGB :: VkFormat
pattern VK_FORMAT_R8_SRGB = VkFormat 15

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_UNORM"
pattern VK_FORMAT_R8G8_UNORM :: VkFormat
pattern VK_FORMAT_R8G8_UNORM = VkFormat 16

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_SNORM"
pattern VK_FORMAT_R8G8_SNORM :: VkFormat
pattern VK_FORMAT_R8G8_SNORM = VkFormat 17

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_USCALED"
pattern VK_FORMAT_R8G8_USCALED :: VkFormat
pattern VK_FORMAT_R8G8_USCALED = VkFormat 18

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_SSCALED"
pattern VK_FORMAT_R8G8_SSCALED :: VkFormat
pattern VK_FORMAT_R8G8_SSCALED = VkFormat 19

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_UINT"
pattern VK_FORMAT_R8G8_UINT :: VkFormat
pattern VK_FORMAT_R8G8_UINT = VkFormat 20

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_SINT"
pattern VK_FORMAT_R8G8_SINT :: VkFormat
pattern VK_FORMAT_R8G8_SINT = VkFormat 21

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_SRGB"
pattern VK_FORMAT_R8G8_SRGB :: VkFormat
pattern VK_FORMAT_R8G8_SRGB = VkFormat 22

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_UNORM"
pattern VK_FORMAT_R8G8B8_UNORM :: VkFormat
pattern VK_FORMAT_R8G8B8_UNORM = VkFormat 23

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_SNORM"
pattern VK_FORMAT_R8G8B8_SNORM :: VkFormat
pattern VK_FORMAT_R8G8B8_SNORM = VkFormat 24

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_USCALED"
pattern VK_FORMAT_R8G8B8_USCALED :: VkFormat
pattern VK_FORMAT_R8G8B8_USCALED = VkFormat 25

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_SSCALED"
pattern VK_FORMAT_R8G8B8_SSCALED :: VkFormat
pattern VK_FORMAT_R8G8B8_SSCALED = VkFormat 26

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_UINT"
pattern VK_FORMAT_R8G8B8_UINT :: VkFormat
pattern VK_FORMAT_R8G8B8_UINT = VkFormat 27

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_SINT"
pattern VK_FORMAT_R8G8B8_SINT :: VkFormat
pattern VK_FORMAT_R8G8B8_SINT = VkFormat 28

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_SRGB"
pattern VK_FORMAT_R8G8B8_SRGB :: VkFormat
pattern VK_FORMAT_R8G8B8_SRGB = VkFormat 29

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_UNORM"
pattern VK_FORMAT_B8G8R8_UNORM :: VkFormat
pattern VK_FORMAT_B8G8R8_UNORM = VkFormat 30

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_SNORM"
pattern VK_FORMAT_B8G8R8_SNORM :: VkFormat
pattern VK_FORMAT_B8G8R8_SNORM = VkFormat 31

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_USCALED"
pattern VK_FORMAT_B8G8R8_USCALED :: VkFormat
pattern VK_FORMAT_B8G8R8_USCALED = VkFormat 32

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_SSCALED"
pattern VK_FORMAT_B8G8R8_SSCALED :: VkFormat
pattern VK_FORMAT_B8G8R8_SSCALED = VkFormat 33

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_UINT"
pattern VK_FORMAT_B8G8R8_UINT :: VkFormat
pattern VK_FORMAT_B8G8R8_UINT = VkFormat 34

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_SINT"
pattern VK_FORMAT_B8G8R8_SINT :: VkFormat
pattern VK_FORMAT_B8G8R8_SINT = VkFormat 35

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_SRGB"
pattern VK_FORMAT_B8G8R8_SRGB :: VkFormat
pattern VK_FORMAT_B8G8R8_SRGB = VkFormat 36

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_UNORM"
pattern VK_FORMAT_R8G8B8A8_UNORM :: VkFormat
pattern VK_FORMAT_R8G8B8A8_UNORM = VkFormat 37

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_SNORM"
pattern VK_FORMAT_R8G8B8A8_SNORM :: VkFormat
pattern VK_FORMAT_R8G8B8A8_SNORM = VkFormat 38

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_USCALED"
pattern VK_FORMAT_R8G8B8A8_USCALED :: VkFormat
pattern VK_FORMAT_R8G8B8A8_USCALED = VkFormat 39

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_SSCALED"
pattern VK_FORMAT_R8G8B8A8_SSCALED :: VkFormat
pattern VK_FORMAT_R8G8B8A8_SSCALED = VkFormat 40

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_UINT"
pattern VK_FORMAT_R8G8B8A8_UINT :: VkFormat
pattern VK_FORMAT_R8G8B8A8_UINT = VkFormat 41

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_SINT"
pattern VK_FORMAT_R8G8B8A8_SINT :: VkFormat
pattern VK_FORMAT_R8G8B8A8_SINT = VkFormat 42

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_SRGB"
pattern VK_FORMAT_R8G8B8A8_SRGB :: VkFormat
pattern VK_FORMAT_R8G8B8A8_SRGB = VkFormat 43

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_UNORM"
pattern VK_FORMAT_B8G8R8A8_UNORM :: VkFormat
pattern VK_FORMAT_B8G8R8A8_UNORM = VkFormat 44

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_SNORM"
pattern VK_FORMAT_B8G8R8A8_SNORM :: VkFormat
pattern VK_FORMAT_B8G8R8A8_SNORM = VkFormat 45

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_USCALED"
pattern VK_FORMAT_B8G8R8A8_USCALED :: VkFormat
pattern VK_FORMAT_B8G8R8A8_USCALED = VkFormat 46

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_SSCALED"
pattern VK_FORMAT_B8G8R8A8_SSCALED :: VkFormat
pattern VK_FORMAT_B8G8R8A8_SSCALED = VkFormat 47

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_UINT"
pattern VK_FORMAT_B8G8R8A8_UINT :: VkFormat
pattern VK_FORMAT_B8G8R8A8_UINT = VkFormat 48

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_SINT"
pattern VK_FORMAT_B8G8R8A8_SINT :: VkFormat
pattern VK_FORMAT_B8G8R8A8_SINT = VkFormat 49

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_SRGB"
pattern VK_FORMAT_B8G8R8A8_SRGB :: VkFormat
pattern VK_FORMAT_B8G8R8A8_SRGB = VkFormat 50

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_UNORM_PACK32"
pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32 = VkFormat 51

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_SNORM_PACK32"
pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32 = VkFormat 52

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_USCALED_PACK32"
pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32 = VkFormat 53

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_SSCALED_PACK32"
pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32 = VkFormat 54

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_UINT_PACK32"
pattern VK_FORMAT_A8B8G8R8_UINT_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_UINT_PACK32 = VkFormat 55

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_SINT_PACK32"
pattern VK_FORMAT_A8B8G8R8_SINT_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_SINT_PACK32 = VkFormat 56

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_SRGB_PACK32"
pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32 = VkFormat 57

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2R10G10B10_UNORM_PACK32"
pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32 :: VkFormat
pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32 = VkFormat 58

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2R10G10B10_SNORM_PACK32"
pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32 :: VkFormat
pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32 = VkFormat 59

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2R10G10B10_USCALED_PACK32"
pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32 :: VkFormat
pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32 = VkFormat 60

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2R10G10B10_SSCALED_PACK32"
pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32 :: VkFormat
pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32 = VkFormat 61

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2R10G10B10_UINT_PACK32"
pattern VK_FORMAT_A2R10G10B10_UINT_PACK32 :: VkFormat
pattern VK_FORMAT_A2R10G10B10_UINT_PACK32 = VkFormat 62

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2R10G10B10_SINT_PACK32"
pattern VK_FORMAT_A2R10G10B10_SINT_PACK32 :: VkFormat
pattern VK_FORMAT_A2R10G10B10_SINT_PACK32 = VkFormat 63

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2B10G10R10_UNORM_PACK32"
pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32 :: VkFormat
pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32 = VkFormat 64

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2B10G10R10_SNORM_PACK32"
pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32 :: VkFormat
pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32 = VkFormat 65

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2B10G10R10_USCALED_PACK32"
pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32 :: VkFormat
pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32 = VkFormat 66

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2B10G10R10_SSCALED_PACK32"
pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32 :: VkFormat
pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32 = VkFormat 67

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2B10G10R10_UINT_PACK32"
pattern VK_FORMAT_A2B10G10R10_UINT_PACK32 :: VkFormat
pattern VK_FORMAT_A2B10G10R10_UINT_PACK32 = VkFormat 68

-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2B10G10R10_SINT_PACK32"
pattern VK_FORMAT_A2B10G10R10_SINT_PACK32 :: VkFormat
pattern VK_FORMAT_A2B10G10R10_SINT_PACK32 = VkFormat 69

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_UNORM"
pattern VK_FORMAT_R16_UNORM :: VkFormat
pattern VK_FORMAT_R16_UNORM = VkFormat 70

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_SNORM"
pattern VK_FORMAT_R16_SNORM :: VkFormat
pattern VK_FORMAT_R16_SNORM = VkFormat 71

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_USCALED"
pattern VK_FORMAT_R16_USCALED :: VkFormat
pattern VK_FORMAT_R16_USCALED = VkFormat 72

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_SSCALED"
pattern VK_FORMAT_R16_SSCALED :: VkFormat
pattern VK_FORMAT_R16_SSCALED = VkFormat 73

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_UINT"
pattern VK_FORMAT_R16_UINT :: VkFormat
pattern VK_FORMAT_R16_UINT = VkFormat 74

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_SINT"
pattern VK_FORMAT_R16_SINT :: VkFormat
pattern VK_FORMAT_R16_SINT = VkFormat 75

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_SFLOAT"
pattern VK_FORMAT_R16_SFLOAT :: VkFormat
pattern VK_FORMAT_R16_SFLOAT = VkFormat 76

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_UNORM"
pattern VK_FORMAT_R16G16_UNORM :: VkFormat
pattern VK_FORMAT_R16G16_UNORM = VkFormat 77

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_SNORM"
pattern VK_FORMAT_R16G16_SNORM :: VkFormat
pattern VK_FORMAT_R16G16_SNORM = VkFormat 78

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_USCALED"
pattern VK_FORMAT_R16G16_USCALED :: VkFormat
pattern VK_FORMAT_R16G16_USCALED = VkFormat 79

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_SSCALED"
pattern VK_FORMAT_R16G16_SSCALED :: VkFormat
pattern VK_FORMAT_R16G16_SSCALED = VkFormat 80

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_UINT"
pattern VK_FORMAT_R16G16_UINT :: VkFormat
pattern VK_FORMAT_R16G16_UINT = VkFormat 81

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_SINT"
pattern VK_FORMAT_R16G16_SINT :: VkFormat
pattern VK_FORMAT_R16G16_SINT = VkFormat 82

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_SFLOAT"
pattern VK_FORMAT_R16G16_SFLOAT :: VkFormat
pattern VK_FORMAT_R16G16_SFLOAT = VkFormat 83

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_UNORM"
pattern VK_FORMAT_R16G16B16_UNORM :: VkFormat
pattern VK_FORMAT_R16G16B16_UNORM = VkFormat 84

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_SNORM"
pattern VK_FORMAT_R16G16B16_SNORM :: VkFormat
pattern VK_FORMAT_R16G16B16_SNORM = VkFormat 85

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_USCALED"
pattern VK_FORMAT_R16G16B16_USCALED :: VkFormat
pattern VK_FORMAT_R16G16B16_USCALED = VkFormat 86

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_SSCALED"
pattern VK_FORMAT_R16G16B16_SSCALED :: VkFormat
pattern VK_FORMAT_R16G16B16_SSCALED = VkFormat 87

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_UINT"
pattern VK_FORMAT_R16G16B16_UINT :: VkFormat
pattern VK_FORMAT_R16G16B16_UINT = VkFormat 88

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_SINT"
pattern VK_FORMAT_R16G16B16_SINT :: VkFormat
pattern VK_FORMAT_R16G16B16_SINT = VkFormat 89

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_SFLOAT"
pattern VK_FORMAT_R16G16B16_SFLOAT :: VkFormat
pattern VK_FORMAT_R16G16B16_SFLOAT = VkFormat 90

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_UNORM"
pattern VK_FORMAT_R16G16B16A16_UNORM :: VkFormat
pattern VK_FORMAT_R16G16B16A16_UNORM = VkFormat 91

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_SNORM"
pattern VK_FORMAT_R16G16B16A16_SNORM :: VkFormat
pattern VK_FORMAT_R16G16B16A16_SNORM = VkFormat 92

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_USCALED"
pattern VK_FORMAT_R16G16B16A16_USCALED :: VkFormat
pattern VK_FORMAT_R16G16B16A16_USCALED = VkFormat 93

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_SSCALED"
pattern VK_FORMAT_R16G16B16A16_SSCALED :: VkFormat
pattern VK_FORMAT_R16G16B16A16_SSCALED = VkFormat 94

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_UINT"
pattern VK_FORMAT_R16G16B16A16_UINT :: VkFormat
pattern VK_FORMAT_R16G16B16A16_UINT = VkFormat 95

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_SINT"
pattern VK_FORMAT_R16G16B16A16_SINT :: VkFormat
pattern VK_FORMAT_R16G16B16A16_SINT = VkFormat 96

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_SFLOAT"
pattern VK_FORMAT_R16G16B16A16_SFLOAT :: VkFormat
pattern VK_FORMAT_R16G16B16A16_SFLOAT = VkFormat 97

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32_UINT"
pattern VK_FORMAT_R32_UINT :: VkFormat
pattern VK_FORMAT_R32_UINT = VkFormat 98

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32_SINT"
pattern VK_FORMAT_R32_SINT :: VkFormat
pattern VK_FORMAT_R32_SINT = VkFormat 99

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32_SFLOAT"
pattern VK_FORMAT_R32_SFLOAT :: VkFormat
pattern VK_FORMAT_R32_SFLOAT = VkFormat 100

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32_UINT"
pattern VK_FORMAT_R32G32_UINT :: VkFormat
pattern VK_FORMAT_R32G32_UINT = VkFormat 101

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32_SINT"
pattern VK_FORMAT_R32G32_SINT :: VkFormat
pattern VK_FORMAT_R32G32_SINT = VkFormat 102

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32_SFLOAT"
pattern VK_FORMAT_R32G32_SFLOAT :: VkFormat
pattern VK_FORMAT_R32G32_SFLOAT = VkFormat 103

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32B32_UINT"
pattern VK_FORMAT_R32G32B32_UINT :: VkFormat
pattern VK_FORMAT_R32G32B32_UINT = VkFormat 104

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32B32_SINT"
pattern VK_FORMAT_R32G32B32_SINT :: VkFormat
pattern VK_FORMAT_R32G32B32_SINT = VkFormat 105

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32B32_SFLOAT"
pattern VK_FORMAT_R32G32B32_SFLOAT :: VkFormat
pattern VK_FORMAT_R32G32B32_SFLOAT = VkFormat 106

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32B32A32_UINT"
pattern VK_FORMAT_R32G32B32A32_UINT :: VkFormat
pattern VK_FORMAT_R32G32B32A32_UINT = VkFormat 107

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32B32A32_SINT"
pattern VK_FORMAT_R32G32B32A32_SINT :: VkFormat
pattern VK_FORMAT_R32G32B32A32_SINT = VkFormat 108

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32B32A32_SFLOAT"
pattern VK_FORMAT_R32G32B32A32_SFLOAT :: VkFormat
pattern VK_FORMAT_R32G32B32A32_SFLOAT = VkFormat 109

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64_UINT"
pattern VK_FORMAT_R64_UINT :: VkFormat
pattern VK_FORMAT_R64_UINT = VkFormat 110

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64_SINT"
pattern VK_FORMAT_R64_SINT :: VkFormat
pattern VK_FORMAT_R64_SINT = VkFormat 111

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64_SFLOAT"
pattern VK_FORMAT_R64_SFLOAT :: VkFormat
pattern VK_FORMAT_R64_SFLOAT = VkFormat 112

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64_UINT"
pattern VK_FORMAT_R64G64_UINT :: VkFormat
pattern VK_FORMAT_R64G64_UINT = VkFormat 113

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64_SINT"
pattern VK_FORMAT_R64G64_SINT :: VkFormat
pattern VK_FORMAT_R64G64_SINT = VkFormat 114

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64_SFLOAT"
pattern VK_FORMAT_R64G64_SFLOAT :: VkFormat
pattern VK_FORMAT_R64G64_SFLOAT = VkFormat 115

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64B64_UINT"
pattern VK_FORMAT_R64G64B64_UINT :: VkFormat
pattern VK_FORMAT_R64G64B64_UINT = VkFormat 116

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64B64_SINT"
pattern VK_FORMAT_R64G64B64_SINT :: VkFormat
pattern VK_FORMAT_R64G64B64_SINT = VkFormat 117

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64B64_SFLOAT"
pattern VK_FORMAT_R64G64B64_SFLOAT :: VkFormat
pattern VK_FORMAT_R64G64B64_SFLOAT = VkFormat 118

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64B64A64_UINT"
pattern VK_FORMAT_R64G64B64A64_UINT :: VkFormat
pattern VK_FORMAT_R64G64B64A64_UINT = VkFormat 119

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64B64A64_SINT"
pattern VK_FORMAT_R64G64B64A64_SINT :: VkFormat
pattern VK_FORMAT_R64G64B64A64_SINT = VkFormat 120

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64B64A64_SFLOAT"
pattern VK_FORMAT_R64G64B64A64_SFLOAT :: VkFormat
pattern VK_FORMAT_R64G64B64A64_SFLOAT = VkFormat 121

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B10G11R11_UFLOAT_PACK32"
pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32 :: VkFormat
pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32 = VkFormat 122

-- No documentation found for Nested "VkFormat" "VK_FORMAT_E5B9G9R9_UFLOAT_PACK32"
pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 :: VkFormat
pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 = VkFormat 123

-- No documentation found for Nested "VkFormat" "VK_FORMAT_D16_UNORM"
pattern VK_FORMAT_D16_UNORM :: VkFormat
pattern VK_FORMAT_D16_UNORM = VkFormat 124

-- No documentation found for Nested "VkFormat" "VK_FORMAT_X8_D24_UNORM_PACK32"
pattern VK_FORMAT_X8_D24_UNORM_PACK32 :: VkFormat
pattern VK_FORMAT_X8_D24_UNORM_PACK32 = VkFormat 125

-- No documentation found for Nested "VkFormat" "VK_FORMAT_D32_SFLOAT"
pattern VK_FORMAT_D32_SFLOAT :: VkFormat
pattern VK_FORMAT_D32_SFLOAT = VkFormat 126

-- No documentation found for Nested "VkFormat" "VK_FORMAT_S8_UINT"
pattern VK_FORMAT_S8_UINT :: VkFormat
pattern VK_FORMAT_S8_UINT = VkFormat 127

-- No documentation found for Nested "VkFormat" "VK_FORMAT_D16_UNORM_S8_UINT"
pattern VK_FORMAT_D16_UNORM_S8_UINT :: VkFormat
pattern VK_FORMAT_D16_UNORM_S8_UINT = VkFormat 128

-- No documentation found for Nested "VkFormat" "VK_FORMAT_D24_UNORM_S8_UINT"
pattern VK_FORMAT_D24_UNORM_S8_UINT :: VkFormat
pattern VK_FORMAT_D24_UNORM_S8_UINT = VkFormat 129

-- No documentation found for Nested "VkFormat" "VK_FORMAT_D32_SFLOAT_S8_UINT"
pattern VK_FORMAT_D32_SFLOAT_S8_UINT :: VkFormat
pattern VK_FORMAT_D32_SFLOAT_S8_UINT = VkFormat 130

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC1_RGB_UNORM_BLOCK"
pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK = VkFormat 131

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC1_RGB_SRGB_BLOCK"
pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK = VkFormat 132

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC1_RGBA_UNORM_BLOCK"
pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK = VkFormat 133

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC1_RGBA_SRGB_BLOCK"
pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK = VkFormat 134

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC2_UNORM_BLOCK"
pattern VK_FORMAT_BC2_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC2_UNORM_BLOCK = VkFormat 135

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC2_SRGB_BLOCK"
pattern VK_FORMAT_BC2_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_BC2_SRGB_BLOCK = VkFormat 136

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC3_UNORM_BLOCK"
pattern VK_FORMAT_BC3_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC3_UNORM_BLOCK = VkFormat 137

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC3_SRGB_BLOCK"
pattern VK_FORMAT_BC3_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_BC3_SRGB_BLOCK = VkFormat 138

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC4_UNORM_BLOCK"
pattern VK_FORMAT_BC4_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC4_UNORM_BLOCK = VkFormat 139

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC4_SNORM_BLOCK"
pattern VK_FORMAT_BC4_SNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC4_SNORM_BLOCK = VkFormat 140

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC5_UNORM_BLOCK"
pattern VK_FORMAT_BC5_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC5_UNORM_BLOCK = VkFormat 141

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC5_SNORM_BLOCK"
pattern VK_FORMAT_BC5_SNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC5_SNORM_BLOCK = VkFormat 142

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC6H_UFLOAT_BLOCK"
pattern VK_FORMAT_BC6H_UFLOAT_BLOCK :: VkFormat
pattern VK_FORMAT_BC6H_UFLOAT_BLOCK = VkFormat 143

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC6H_SFLOAT_BLOCK"
pattern VK_FORMAT_BC6H_SFLOAT_BLOCK :: VkFormat
pattern VK_FORMAT_BC6H_SFLOAT_BLOCK = VkFormat 144

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC7_UNORM_BLOCK"
pattern VK_FORMAT_BC7_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC7_UNORM_BLOCK = VkFormat 145

-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC7_SRGB_BLOCK"
pattern VK_FORMAT_BC7_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_BC7_SRGB_BLOCK = VkFormat 146

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK"
pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK = VkFormat 147

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK"
pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK = VkFormat 148

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK"
pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK = VkFormat 149

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK"
pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK = VkFormat 150

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK"
pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = VkFormat 151

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK"
pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK = VkFormat 152

-- No documentation found for Nested "VkFormat" "VK_FORMAT_EAC_R11_UNORM_BLOCK"
pattern VK_FORMAT_EAC_R11_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_EAC_R11_UNORM_BLOCK = VkFormat 153

-- No documentation found for Nested "VkFormat" "VK_FORMAT_EAC_R11_SNORM_BLOCK"
pattern VK_FORMAT_EAC_R11_SNORM_BLOCK :: VkFormat
pattern VK_FORMAT_EAC_R11_SNORM_BLOCK = VkFormat 154

-- No documentation found for Nested "VkFormat" "VK_FORMAT_EAC_R11G11_UNORM_BLOCK"
pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK = VkFormat 155

-- No documentation found for Nested "VkFormat" "VK_FORMAT_EAC_R11G11_SNORM_BLOCK"
pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK :: VkFormat
pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK = VkFormat 156

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_4x4_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK = VkFormat 157

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_4x4_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK = VkFormat 158

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_5x4_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK = VkFormat 159

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_5x4_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK = VkFormat 160

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_5x5_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK = VkFormat 161

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_5x5_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK = VkFormat 162

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_6x5_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK = VkFormat 163

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_6x5_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK = VkFormat 164

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_6x6_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK = VkFormat 165

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_6x6_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK = VkFormat 166

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x5_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK = VkFormat 167

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x5_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK = VkFormat 168

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x6_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK = VkFormat 169

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x6_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK = VkFormat 170

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x8_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK = VkFormat 171

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x8_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK = VkFormat 172

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x5_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK = VkFormat 173

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x5_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK = VkFormat 174

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x6_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK = VkFormat 175

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x6_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK = VkFormat 176

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x8_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK = VkFormat 177

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x8_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK = VkFormat 178

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x10_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK = VkFormat 179

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x10_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK = VkFormat 180

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_12x10_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK = VkFormat 181

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_12x10_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK = VkFormat 182

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_12x12_UNORM_BLOCK"
pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK = VkFormat 183

-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_12x12_SRGB_BLOCK"
pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK = VkFormat 184

-- ** VkObjectType

-- No documentation found for TopLevel "VkObjectType"
newtype VkObjectType = VkObjectType Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkObjectType where
  showsPrec _ VK_OBJECT_TYPE_UNKNOWN = showString "VK_OBJECT_TYPE_UNKNOWN"
  showsPrec _ VK_OBJECT_TYPE_INSTANCE = showString "VK_OBJECT_TYPE_INSTANCE"
  showsPrec _ VK_OBJECT_TYPE_PHYSICAL_DEVICE = showString "VK_OBJECT_TYPE_PHYSICAL_DEVICE"
  showsPrec _ VK_OBJECT_TYPE_DEVICE = showString "VK_OBJECT_TYPE_DEVICE"
  showsPrec _ VK_OBJECT_TYPE_QUEUE = showString "VK_OBJECT_TYPE_QUEUE"
  showsPrec _ VK_OBJECT_TYPE_SEMAPHORE = showString "VK_OBJECT_TYPE_SEMAPHORE"
  showsPrec _ VK_OBJECT_TYPE_COMMAND_BUFFER = showString "VK_OBJECT_TYPE_COMMAND_BUFFER"
  showsPrec _ VK_OBJECT_TYPE_FENCE = showString "VK_OBJECT_TYPE_FENCE"
  showsPrec _ VK_OBJECT_TYPE_DEVICE_MEMORY = showString "VK_OBJECT_TYPE_DEVICE_MEMORY"
  showsPrec _ VK_OBJECT_TYPE_BUFFER = showString "VK_OBJECT_TYPE_BUFFER"
  showsPrec _ VK_OBJECT_TYPE_IMAGE = showString "VK_OBJECT_TYPE_IMAGE"
  showsPrec _ VK_OBJECT_TYPE_EVENT = showString "VK_OBJECT_TYPE_EVENT"
  showsPrec _ VK_OBJECT_TYPE_QUERY_POOL = showString "VK_OBJECT_TYPE_QUERY_POOL"
  showsPrec _ VK_OBJECT_TYPE_BUFFER_VIEW = showString "VK_OBJECT_TYPE_BUFFER_VIEW"
  showsPrec _ VK_OBJECT_TYPE_IMAGE_VIEW = showString "VK_OBJECT_TYPE_IMAGE_VIEW"
  showsPrec _ VK_OBJECT_TYPE_SHADER_MODULE = showString "VK_OBJECT_TYPE_SHADER_MODULE"
  showsPrec _ VK_OBJECT_TYPE_PIPELINE_CACHE = showString "VK_OBJECT_TYPE_PIPELINE_CACHE"
  showsPrec _ VK_OBJECT_TYPE_PIPELINE_LAYOUT = showString "VK_OBJECT_TYPE_PIPELINE_LAYOUT"
  showsPrec _ VK_OBJECT_TYPE_RENDER_PASS = showString "VK_OBJECT_TYPE_RENDER_PASS"
  showsPrec _ VK_OBJECT_TYPE_PIPELINE = showString "VK_OBJECT_TYPE_PIPELINE"
  showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT = showString "VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT"
  showsPrec _ VK_OBJECT_TYPE_SAMPLER = showString "VK_OBJECT_TYPE_SAMPLER"
  showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_POOL = showString "VK_OBJECT_TYPE_DESCRIPTOR_POOL"
  showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_SET = showString "VK_OBJECT_TYPE_DESCRIPTOR_SET"
  showsPrec _ VK_OBJECT_TYPE_FRAMEBUFFER = showString "VK_OBJECT_TYPE_FRAMEBUFFER"
  showsPrec _ VK_OBJECT_TYPE_COMMAND_POOL = showString "VK_OBJECT_TYPE_COMMAND_POOL"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkObjectType 1000156000) = showString "VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION"
  showsPrec _ (VkObjectType 1000085000) = showString "VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE"
  showsPrec _ (VkObjectType 1000000000) = showString "VK_OBJECT_TYPE_SURFACE_KHR"
  showsPrec _ (VkObjectType 1000001000) = showString "VK_OBJECT_TYPE_SWAPCHAIN_KHR"
  showsPrec _ (VkObjectType 1000002000) = showString "VK_OBJECT_TYPE_DISPLAY_KHR"
  showsPrec _ (VkObjectType 1000002001) = showString "VK_OBJECT_TYPE_DISPLAY_MODE_KHR"
  showsPrec _ (VkObjectType 1000011000) = showString "VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT"
  showsPrec _ (VkObjectType 1000086000) = showString "VK_OBJECT_TYPE_OBJECT_TABLE_NVX"
  showsPrec _ (VkObjectType 1000086001) = showString "VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX"
  showsPrec _ (VkObjectType 1000128000) = showString "VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT"
  showsPrec _ (VkObjectType 1000160000) = showString "VK_OBJECT_TYPE_VALIDATION_CACHE_EXT"
  showsPrec _ (VkObjectType 1000165000) = showString "VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV"
  showsPrec p (VkObjectType x) = showParen (p >= 11) (showString "VkObjectType " . showsPrec 11 x)

instance Read VkObjectType where
  readPrec = parens ( choose [ ("VK_OBJECT_TYPE_UNKNOWN",               pure VK_OBJECT_TYPE_UNKNOWN)
                             , ("VK_OBJECT_TYPE_INSTANCE",              pure VK_OBJECT_TYPE_INSTANCE)
                             , ("VK_OBJECT_TYPE_PHYSICAL_DEVICE",       pure VK_OBJECT_TYPE_PHYSICAL_DEVICE)
                             , ("VK_OBJECT_TYPE_DEVICE",                pure VK_OBJECT_TYPE_DEVICE)
                             , ("VK_OBJECT_TYPE_QUEUE",                 pure VK_OBJECT_TYPE_QUEUE)
                             , ("VK_OBJECT_TYPE_SEMAPHORE",             pure VK_OBJECT_TYPE_SEMAPHORE)
                             , ("VK_OBJECT_TYPE_COMMAND_BUFFER",        pure VK_OBJECT_TYPE_COMMAND_BUFFER)
                             , ("VK_OBJECT_TYPE_FENCE",                 pure VK_OBJECT_TYPE_FENCE)
                             , ("VK_OBJECT_TYPE_DEVICE_MEMORY",         pure VK_OBJECT_TYPE_DEVICE_MEMORY)
                             , ("VK_OBJECT_TYPE_BUFFER",                pure VK_OBJECT_TYPE_BUFFER)
                             , ("VK_OBJECT_TYPE_IMAGE",                 pure VK_OBJECT_TYPE_IMAGE)
                             , ("VK_OBJECT_TYPE_EVENT",                 pure VK_OBJECT_TYPE_EVENT)
                             , ("VK_OBJECT_TYPE_QUERY_POOL",            pure VK_OBJECT_TYPE_QUERY_POOL)
                             , ("VK_OBJECT_TYPE_BUFFER_VIEW",           pure VK_OBJECT_TYPE_BUFFER_VIEW)
                             , ("VK_OBJECT_TYPE_IMAGE_VIEW",            pure VK_OBJECT_TYPE_IMAGE_VIEW)
                             , ("VK_OBJECT_TYPE_SHADER_MODULE",         pure VK_OBJECT_TYPE_SHADER_MODULE)
                             , ("VK_OBJECT_TYPE_PIPELINE_CACHE",        pure VK_OBJECT_TYPE_PIPELINE_CACHE)
                             , ("VK_OBJECT_TYPE_PIPELINE_LAYOUT",       pure VK_OBJECT_TYPE_PIPELINE_LAYOUT)
                             , ("VK_OBJECT_TYPE_RENDER_PASS",           pure VK_OBJECT_TYPE_RENDER_PASS)
                             , ("VK_OBJECT_TYPE_PIPELINE",              pure VK_OBJECT_TYPE_PIPELINE)
                             , ("VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT", pure VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT)
                             , ("VK_OBJECT_TYPE_SAMPLER",               pure VK_OBJECT_TYPE_SAMPLER)
                             , ("VK_OBJECT_TYPE_DESCRIPTOR_POOL",       pure VK_OBJECT_TYPE_DESCRIPTOR_POOL)
                             , ("VK_OBJECT_TYPE_DESCRIPTOR_SET",        pure VK_OBJECT_TYPE_DESCRIPTOR_SET)
                             , ("VK_OBJECT_TYPE_FRAMEBUFFER",           pure VK_OBJECT_TYPE_FRAMEBUFFER)
                             , ("VK_OBJECT_TYPE_COMMAND_POOL",          pure VK_OBJECT_TYPE_COMMAND_POOL)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION",     pure (VkObjectType 1000156000))
                             , ("VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE",   pure (VkObjectType 1000085000))
                             , ("VK_OBJECT_TYPE_SURFACE_KHR",                  pure (VkObjectType 1000000000))
                             , ("VK_OBJECT_TYPE_SWAPCHAIN_KHR",                pure (VkObjectType 1000001000))
                             , ("VK_OBJECT_TYPE_DISPLAY_KHR",                  pure (VkObjectType 1000002000))
                             , ("VK_OBJECT_TYPE_DISPLAY_MODE_KHR",             pure (VkObjectType 1000002001))
                             , ("VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT",    pure (VkObjectType 1000011000))
                             , ("VK_OBJECT_TYPE_OBJECT_TABLE_NVX",             pure (VkObjectType 1000086000))
                             , ("VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX", pure (VkObjectType 1000086001))
                             , ("VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT",    pure (VkObjectType 1000128000))
                             , ("VK_OBJECT_TYPE_VALIDATION_CACHE_EXT",         pure (VkObjectType 1000160000))
                             , ("VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV",    pure (VkObjectType 1000165000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkObjectType")
                        v <- step readPrec
                        pure (VkObjectType v)
                        )
                    )

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_UNKNOWN"
pattern VK_OBJECT_TYPE_UNKNOWN :: VkObjectType
pattern VK_OBJECT_TYPE_UNKNOWN = VkObjectType 0

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_INSTANCE"
pattern VK_OBJECT_TYPE_INSTANCE :: VkObjectType
pattern VK_OBJECT_TYPE_INSTANCE = VkObjectType 1

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PHYSICAL_DEVICE"
pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE :: VkObjectType
pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE = VkObjectType 2

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEVICE"
pattern VK_OBJECT_TYPE_DEVICE :: VkObjectType
pattern VK_OBJECT_TYPE_DEVICE = VkObjectType 3

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_QUEUE"
pattern VK_OBJECT_TYPE_QUEUE :: VkObjectType
pattern VK_OBJECT_TYPE_QUEUE = VkObjectType 4

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SEMAPHORE"
pattern VK_OBJECT_TYPE_SEMAPHORE :: VkObjectType
pattern VK_OBJECT_TYPE_SEMAPHORE = VkObjectType 5

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_COMMAND_BUFFER"
pattern VK_OBJECT_TYPE_COMMAND_BUFFER :: VkObjectType
pattern VK_OBJECT_TYPE_COMMAND_BUFFER = VkObjectType 6

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_FENCE"
pattern VK_OBJECT_TYPE_FENCE :: VkObjectType
pattern VK_OBJECT_TYPE_FENCE = VkObjectType 7

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEVICE_MEMORY"
pattern VK_OBJECT_TYPE_DEVICE_MEMORY :: VkObjectType
pattern VK_OBJECT_TYPE_DEVICE_MEMORY = VkObjectType 8

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_BUFFER"
pattern VK_OBJECT_TYPE_BUFFER :: VkObjectType
pattern VK_OBJECT_TYPE_BUFFER = VkObjectType 9

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_IMAGE"
pattern VK_OBJECT_TYPE_IMAGE :: VkObjectType
pattern VK_OBJECT_TYPE_IMAGE = VkObjectType 10

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_EVENT"
pattern VK_OBJECT_TYPE_EVENT :: VkObjectType
pattern VK_OBJECT_TYPE_EVENT = VkObjectType 11

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_QUERY_POOL"
pattern VK_OBJECT_TYPE_QUERY_POOL :: VkObjectType
pattern VK_OBJECT_TYPE_QUERY_POOL = VkObjectType 12

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_BUFFER_VIEW"
pattern VK_OBJECT_TYPE_BUFFER_VIEW :: VkObjectType
pattern VK_OBJECT_TYPE_BUFFER_VIEW = VkObjectType 13

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_IMAGE_VIEW"
pattern VK_OBJECT_TYPE_IMAGE_VIEW :: VkObjectType
pattern VK_OBJECT_TYPE_IMAGE_VIEW = VkObjectType 14

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SHADER_MODULE"
pattern VK_OBJECT_TYPE_SHADER_MODULE :: VkObjectType
pattern VK_OBJECT_TYPE_SHADER_MODULE = VkObjectType 15

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PIPELINE_CACHE"
pattern VK_OBJECT_TYPE_PIPELINE_CACHE :: VkObjectType
pattern VK_OBJECT_TYPE_PIPELINE_CACHE = VkObjectType 16

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PIPELINE_LAYOUT"
pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT :: VkObjectType
pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT = VkObjectType 17

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_RENDER_PASS"
pattern VK_OBJECT_TYPE_RENDER_PASS :: VkObjectType
pattern VK_OBJECT_TYPE_RENDER_PASS = VkObjectType 18

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PIPELINE"
pattern VK_OBJECT_TYPE_PIPELINE :: VkObjectType
pattern VK_OBJECT_TYPE_PIPELINE = VkObjectType 19

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT"
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT :: VkObjectType
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT = VkObjectType 20

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SAMPLER"
pattern VK_OBJECT_TYPE_SAMPLER :: VkObjectType
pattern VK_OBJECT_TYPE_SAMPLER = VkObjectType 21

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_POOL"
pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL :: VkObjectType
pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL = VkObjectType 22

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_SET"
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET :: VkObjectType
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET = VkObjectType 23

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_FRAMEBUFFER"
pattern VK_OBJECT_TYPE_FRAMEBUFFER :: VkObjectType
pattern VK_OBJECT_TYPE_FRAMEBUFFER = VkObjectType 24

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_COMMAND_POOL"
pattern VK_OBJECT_TYPE_COMMAND_POOL :: VkObjectType
pattern VK_OBJECT_TYPE_COMMAND_POOL = VkObjectType 25

-- ** VkResult

-- No documentation found for TopLevel "VkResult"
newtype VkResult = VkResult Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkResult where
  showsPrec _ VK_SUCCESS = showString "VK_SUCCESS"
  showsPrec _ VK_NOT_READY = showString "VK_NOT_READY"
  showsPrec _ VK_TIMEOUT = showString "VK_TIMEOUT"
  showsPrec _ VK_EVENT_SET = showString "VK_EVENT_SET"
  showsPrec _ VK_EVENT_RESET = showString "VK_EVENT_RESET"
  showsPrec _ VK_INCOMPLETE = showString "VK_INCOMPLETE"
  showsPrec _ VK_ERROR_OUT_OF_HOST_MEMORY = showString "VK_ERROR_OUT_OF_HOST_MEMORY"
  showsPrec _ VK_ERROR_OUT_OF_DEVICE_MEMORY = showString "VK_ERROR_OUT_OF_DEVICE_MEMORY"
  showsPrec _ VK_ERROR_INITIALIZATION_FAILED = showString "VK_ERROR_INITIALIZATION_FAILED"
  showsPrec _ VK_ERROR_DEVICE_LOST = showString "VK_ERROR_DEVICE_LOST"
  showsPrec _ VK_ERROR_MEMORY_MAP_FAILED = showString "VK_ERROR_MEMORY_MAP_FAILED"
  showsPrec _ VK_ERROR_LAYER_NOT_PRESENT = showString "VK_ERROR_LAYER_NOT_PRESENT"
  showsPrec _ VK_ERROR_EXTENSION_NOT_PRESENT = showString "VK_ERROR_EXTENSION_NOT_PRESENT"
  showsPrec _ VK_ERROR_FEATURE_NOT_PRESENT = showString "VK_ERROR_FEATURE_NOT_PRESENT"
  showsPrec _ VK_ERROR_INCOMPATIBLE_DRIVER = showString "VK_ERROR_INCOMPATIBLE_DRIVER"
  showsPrec _ VK_ERROR_TOO_MANY_OBJECTS = showString "VK_ERROR_TOO_MANY_OBJECTS"
  showsPrec _ VK_ERROR_FORMAT_NOT_SUPPORTED = showString "VK_ERROR_FORMAT_NOT_SUPPORTED"
  showsPrec _ VK_ERROR_FRAGMENTED_POOL = showString "VK_ERROR_FRAGMENTED_POOL"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkResult (-1000069000)) = showString "VK_ERROR_OUT_OF_POOL_MEMORY"
  showsPrec _ (VkResult (-1000072003)) = showString "VK_ERROR_INVALID_EXTERNAL_HANDLE"
  showsPrec _ (VkResult (-1000000000)) = showString "VK_ERROR_SURFACE_LOST_KHR"
  showsPrec _ (VkResult (-1000000001)) = showString "VK_ERROR_NATIVE_WINDOW_IN_USE_KHR"
  showsPrec _ (VkResult 1000001003) = showString "VK_SUBOPTIMAL_KHR"
  showsPrec _ (VkResult (-1000001004)) = showString "VK_ERROR_OUT_OF_DATE_KHR"
  showsPrec _ (VkResult (-1000003001)) = showString "VK_ERROR_INCOMPATIBLE_DISPLAY_KHR"
  showsPrec _ (VkResult (-1000011001)) = showString "VK_ERROR_VALIDATION_FAILED_EXT"
  showsPrec _ (VkResult (-1000012000)) = showString "VK_ERROR_INVALID_SHADER_NV"
  showsPrec _ (VkResult (-1000158000)) = showString "VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT"
  showsPrec _ (VkResult (-1000161000)) = showString "VK_ERROR_FRAGMENTATION_EXT"
  showsPrec _ (VkResult (-1000174001)) = showString "VK_ERROR_NOT_PERMITTED_EXT"
  showsPrec _ (VkResult (-1000244000)) = showString "VK_ERROR_INVALID_DEVICE_ADDRESS_EXT"
  showsPrec _ (VkResult (-1000255000)) = showString "VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT"
  showsPrec p (VkResult x) = showParen (p >= 11) (showString "VkResult " . showsPrec 11 x)

instance Read VkResult where
  readPrec = parens ( choose [ ("VK_SUCCESS",                     pure VK_SUCCESS)
                             , ("VK_NOT_READY",                   pure VK_NOT_READY)
                             , ("VK_TIMEOUT",                     pure VK_TIMEOUT)
                             , ("VK_EVENT_SET",                   pure VK_EVENT_SET)
                             , ("VK_EVENT_RESET",                 pure VK_EVENT_RESET)
                             , ("VK_INCOMPLETE",                  pure VK_INCOMPLETE)
                             , ("VK_ERROR_OUT_OF_HOST_MEMORY",    pure VK_ERROR_OUT_OF_HOST_MEMORY)
                             , ("VK_ERROR_OUT_OF_DEVICE_MEMORY",  pure VK_ERROR_OUT_OF_DEVICE_MEMORY)
                             , ("VK_ERROR_INITIALIZATION_FAILED", pure VK_ERROR_INITIALIZATION_FAILED)
                             , ("VK_ERROR_DEVICE_LOST",           pure VK_ERROR_DEVICE_LOST)
                             , ("VK_ERROR_MEMORY_MAP_FAILED",     pure VK_ERROR_MEMORY_MAP_FAILED)
                             , ("VK_ERROR_LAYER_NOT_PRESENT",     pure VK_ERROR_LAYER_NOT_PRESENT)
                             , ("VK_ERROR_EXTENSION_NOT_PRESENT", pure VK_ERROR_EXTENSION_NOT_PRESENT)
                             , ("VK_ERROR_FEATURE_NOT_PRESENT",   pure VK_ERROR_FEATURE_NOT_PRESENT)
                             , ("VK_ERROR_INCOMPATIBLE_DRIVER",   pure VK_ERROR_INCOMPATIBLE_DRIVER)
                             , ("VK_ERROR_TOO_MANY_OBJECTS",      pure VK_ERROR_TOO_MANY_OBJECTS)
                             , ("VK_ERROR_FORMAT_NOT_SUPPORTED",  pure VK_ERROR_FORMAT_NOT_SUPPORTED)
                             , ("VK_ERROR_FRAGMENTED_POOL",       pure VK_ERROR_FRAGMENTED_POOL)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_ERROR_OUT_OF_POOL_MEMORY",                           pure (VkResult (-1000069000)))
                             , ("VK_ERROR_INVALID_EXTERNAL_HANDLE",                      pure (VkResult (-1000072003)))
                             , ("VK_ERROR_SURFACE_LOST_KHR",                             pure (VkResult (-1000000000)))
                             , ("VK_ERROR_NATIVE_WINDOW_IN_USE_KHR",                     pure (VkResult (-1000000001)))
                             , ("VK_SUBOPTIMAL_KHR",                                     pure (VkResult 1000001003))
                             , ("VK_ERROR_OUT_OF_DATE_KHR",                              pure (VkResult (-1000001004)))
                             , ("VK_ERROR_INCOMPATIBLE_DISPLAY_KHR",                     pure (VkResult (-1000003001)))
                             , ("VK_ERROR_VALIDATION_FAILED_EXT",                        pure (VkResult (-1000011001)))
                             , ("VK_ERROR_INVALID_SHADER_NV",                            pure (VkResult (-1000012000)))
                             , ("VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT", pure (VkResult (-1000158000)))
                             , ("VK_ERROR_FRAGMENTATION_EXT",                            pure (VkResult (-1000161000)))
                             , ("VK_ERROR_NOT_PERMITTED_EXT",                            pure (VkResult (-1000174001)))
                             , ("VK_ERROR_INVALID_DEVICE_ADDRESS_EXT",                   pure (VkResult (-1000244000)))
                             , ("VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT",          pure (VkResult (-1000255000)))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkResult")
                        v <- step readPrec
                        pure (VkResult v)
                        )
                    )

-- No documentation found for Nested "VkResult" "VK_SUCCESS"
pattern VK_SUCCESS :: VkResult
pattern VK_SUCCESS = VkResult 0

-- No documentation found for Nested "VkResult" "VK_NOT_READY"
pattern VK_NOT_READY :: VkResult
pattern VK_NOT_READY = VkResult 1

-- No documentation found for Nested "VkResult" "VK_TIMEOUT"
pattern VK_TIMEOUT :: VkResult
pattern VK_TIMEOUT = VkResult 2

-- No documentation found for Nested "VkResult" "VK_EVENT_SET"
pattern VK_EVENT_SET :: VkResult
pattern VK_EVENT_SET = VkResult 3

-- No documentation found for Nested "VkResult" "VK_EVENT_RESET"
pattern VK_EVENT_RESET :: VkResult
pattern VK_EVENT_RESET = VkResult 4

-- No documentation found for Nested "VkResult" "VK_INCOMPLETE"
pattern VK_INCOMPLETE :: VkResult
pattern VK_INCOMPLETE = VkResult 5

-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_HOST_MEMORY"
pattern VK_ERROR_OUT_OF_HOST_MEMORY :: VkResult
pattern VK_ERROR_OUT_OF_HOST_MEMORY = VkResult (-1)

-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_DEVICE_MEMORY"
pattern VK_ERROR_OUT_OF_DEVICE_MEMORY :: VkResult
pattern VK_ERROR_OUT_OF_DEVICE_MEMORY = VkResult (-2)

-- No documentation found for Nested "VkResult" "VK_ERROR_INITIALIZATION_FAILED"
pattern VK_ERROR_INITIALIZATION_FAILED :: VkResult
pattern VK_ERROR_INITIALIZATION_FAILED = VkResult (-3)

-- No documentation found for Nested "VkResult" "VK_ERROR_DEVICE_LOST"
pattern VK_ERROR_DEVICE_LOST :: VkResult
pattern VK_ERROR_DEVICE_LOST = VkResult (-4)

-- No documentation found for Nested "VkResult" "VK_ERROR_MEMORY_MAP_FAILED"
pattern VK_ERROR_MEMORY_MAP_FAILED :: VkResult
pattern VK_ERROR_MEMORY_MAP_FAILED = VkResult (-5)

-- No documentation found for Nested "VkResult" "VK_ERROR_LAYER_NOT_PRESENT"
pattern VK_ERROR_LAYER_NOT_PRESENT :: VkResult
pattern VK_ERROR_LAYER_NOT_PRESENT = VkResult (-6)

-- No documentation found for Nested "VkResult" "VK_ERROR_EXTENSION_NOT_PRESENT"
pattern VK_ERROR_EXTENSION_NOT_PRESENT :: VkResult
pattern VK_ERROR_EXTENSION_NOT_PRESENT = VkResult (-7)

-- No documentation found for Nested "VkResult" "VK_ERROR_FEATURE_NOT_PRESENT"
pattern VK_ERROR_FEATURE_NOT_PRESENT :: VkResult
pattern VK_ERROR_FEATURE_NOT_PRESENT = VkResult (-8)

-- No documentation found for Nested "VkResult" "VK_ERROR_INCOMPATIBLE_DRIVER"
pattern VK_ERROR_INCOMPATIBLE_DRIVER :: VkResult
pattern VK_ERROR_INCOMPATIBLE_DRIVER = VkResult (-9)

-- No documentation found for Nested "VkResult" "VK_ERROR_TOO_MANY_OBJECTS"
pattern VK_ERROR_TOO_MANY_OBJECTS :: VkResult
pattern VK_ERROR_TOO_MANY_OBJECTS = VkResult (-10)

-- No documentation found for Nested "VkResult" "VK_ERROR_FORMAT_NOT_SUPPORTED"
pattern VK_ERROR_FORMAT_NOT_SUPPORTED :: VkResult
pattern VK_ERROR_FORMAT_NOT_SUPPORTED = VkResult (-11)

-- No documentation found for Nested "VkResult" "VK_ERROR_FRAGMENTED_POOL"
pattern VK_ERROR_FRAGMENTED_POOL :: VkResult
pattern VK_ERROR_FRAGMENTED_POOL = VkResult (-12)

-- ** VkStructureType

-- No documentation found for TopLevel "VkStructureType"
newtype VkStructureType = VkStructureType Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkStructureType where
  showsPrec _ VK_STRUCTURE_TYPE_APPLICATION_INFO = showString "VK_STRUCTURE_TYPE_APPLICATION_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_SUBMIT_INFO = showString "VK_STRUCTURE_TYPE_SUBMIT_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = showString "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE = showString "VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE"
  showsPrec _ VK_STRUCTURE_TYPE_BIND_SPARSE_INFO = showString "VK_STRUCTURE_TYPE_BIND_SPARSE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_FENCE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_FENCE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_EVENT_CREATE_INFO = showString "VK_STRUCTURE_TYPE_EVENT_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO = showString "VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO = showString "VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO = showString "VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO = showString "VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO = showString "VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = showString "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET"
  showsPrec _ VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET = showString "VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET"
  showsPrec _ VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO = showString "VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO = showString "VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO = showString "VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO = showString "VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO = showString "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO = showString "VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO = showString "VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER = showString "VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER"
  showsPrec _ VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = showString "VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER"
  showsPrec _ VK_STRUCTURE_TYPE_MEMORY_BARRIER = showString "VK_STRUCTURE_TYPE_MEMORY_BARRIER"
  showsPrec _ VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkStructureType 1000094000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES"
  showsPrec _ (VkStructureType 1000157000) = showString "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO"
  showsPrec _ (VkStructureType 1000157001) = showString "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO"
  showsPrec _ (VkStructureType 1000083000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES"
  showsPrec _ (VkStructureType 1000127000) = showString "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS"
  showsPrec _ (VkStructureType 1000127001) = showString "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO"
  showsPrec _ (VkStructureType 1000060000) = showString "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO"
  showsPrec _ (VkStructureType 1000060003) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO"
  showsPrec _ (VkStructureType 1000060004) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO"
  showsPrec _ (VkStructureType 1000060005) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO"
  showsPrec _ (VkStructureType 1000060006) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO"
  showsPrec _ (VkStructureType 1000060013) = showString "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO"
  showsPrec _ (VkStructureType 1000060014) = showString "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO"
  showsPrec _ (VkStructureType 1000070000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES"
  showsPrec _ (VkStructureType 1000070001) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000146000) = showString "VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2"
  showsPrec _ (VkStructureType 1000146001) = showString "VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2"
  showsPrec _ (VkStructureType 1000146002) = showString "VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2"
  showsPrec _ (VkStructureType 1000146003) = showString "VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2"
  showsPrec _ (VkStructureType 1000146004) = showString "VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2"
  showsPrec _ (VkStructureType 1000059000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2"
  showsPrec _ (VkStructureType 1000059001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2"
  showsPrec _ (VkStructureType 1000059002) = showString "VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2"
  showsPrec _ (VkStructureType 1000059003) = showString "VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2"
  showsPrec _ (VkStructureType 1000059004) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2"
  showsPrec _ (VkStructureType 1000059005) = showString "VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2"
  showsPrec _ (VkStructureType 1000059006) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2"
  showsPrec _ (VkStructureType 1000059007) = showString "VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2"
  showsPrec _ (VkStructureType 1000059008) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2"
  showsPrec _ (VkStructureType 1000117000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES"
  showsPrec _ (VkStructureType 1000117001) = showString "VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO"
  showsPrec _ (VkStructureType 1000117002) = showString "VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000117003) = showString "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000053000) = showString "VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO"
  showsPrec _ (VkStructureType 1000053001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES"
  showsPrec _ (VkStructureType 1000053002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES"
  showsPrec _ (VkStructureType 1000120000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES"
  showsPrec _ (VkStructureType 1000145000) = showString "VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO"
  showsPrec _ (VkStructureType 1000145001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES"
  showsPrec _ (VkStructureType 1000145002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES"
  showsPrec _ (VkStructureType 1000145003) = showString "VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2"
  showsPrec _ (VkStructureType 1000156000) = showString "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO"
  showsPrec _ (VkStructureType 1000156001) = showString "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO"
  showsPrec _ (VkStructureType 1000156002) = showString "VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO"
  showsPrec _ (VkStructureType 1000156003) = showString "VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO"
  showsPrec _ (VkStructureType 1000156004) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES"
  showsPrec _ (VkStructureType 1000156005) = showString "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES"
  showsPrec _ (VkStructureType 1000085000) = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000071000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO"
  showsPrec _ (VkStructureType 1000071001) = showString "VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES"
  showsPrec _ (VkStructureType 1000071002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO"
  showsPrec _ (VkStructureType 1000071003) = showString "VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES"
  showsPrec _ (VkStructureType 1000071004) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES"
  showsPrec _ (VkStructureType 1000072000) = showString "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO"
  showsPrec _ (VkStructureType 1000072001) = showString "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000072002) = showString "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO"
  showsPrec _ (VkStructureType 1000112000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO"
  showsPrec _ (VkStructureType 1000112001) = showString "VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES"
  showsPrec _ (VkStructureType 1000113000) = showString "VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000077000) = showString "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000076000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO"
  showsPrec _ (VkStructureType 1000076001) = showString "VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES"
  showsPrec _ (VkStructureType 1000168000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES"
  showsPrec _ (VkStructureType 1000168001) = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT"
  showsPrec _ (VkStructureType 1000063000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES"
  showsPrec _ (VkStructureType 1000001000) = showString "VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000001001) = showString "VK_STRUCTURE_TYPE_PRESENT_INFO_KHR"
  showsPrec _ (VkStructureType 1000060007) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR"
  showsPrec _ (VkStructureType 1000060008) = showString "VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000060009) = showString "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR"
  showsPrec _ (VkStructureType 1000060010) = showString "VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR"
  showsPrec _ (VkStructureType 1000060011) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR"
  showsPrec _ (VkStructureType 1000060012) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000002000) = showString "VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000002001) = showString "VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000003000) = showString "VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR"
  showsPrec _ (VkStructureType 1000004000) = showString "VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000005000) = showString "VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000006000) = showString "VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000008000) = showString "VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000009000) = showString "VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000010000) = showString "VK_STRUCTURE_TYPE_NATIVE_BUFFER_ANDROID"
  showsPrec _ (VkStructureType 1000011000) = showString "VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000018000) = showString "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD"
  showsPrec _ (VkStructureType 1000022000) = showString "VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT"
  showsPrec _ (VkStructureType 1000022001) = showString "VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT"
  showsPrec _ (VkStructureType 1000022002) = showString "VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT"
  showsPrec _ (VkStructureType 1000026000) = showString "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000026001) = showString "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000026002) = showString "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV"
  showsPrec _ (VkStructureType 1000028000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000028001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000028002) = showString "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000030000) = showString "VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX"
  showsPrec _ (VkStructureType 1000041000) = showString "VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD"
  showsPrec _ (VkStructureType 1000049000) = showString "VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP"
  showsPrec _ (VkStructureType 1000050000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV"
  showsPrec _ (VkStructureType 1000056000) = showString "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000056001) = showString "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV"
  showsPrec _ (VkStructureType 1000057000) = showString "VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV"
  showsPrec _ (VkStructureType 1000057001) = showString "VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV"
  showsPrec _ (VkStructureType 1000058000) = showString "VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV"
  showsPrec _ (VkStructureType 1000061000) = showString "VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT"
  showsPrec _ (VkStructureType 1000062000) = showString "VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN"
  showsPrec _ (VkStructureType 1000067000) = showString "VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT"
  showsPrec _ (VkStructureType 1000067001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000073000) = showString "VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000073001) = showString "VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000073002) = showString "VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR"
  showsPrec _ (VkStructureType 1000073003) = showString "VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000074000) = showString "VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR"
  showsPrec _ (VkStructureType 1000074001) = showString "VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR"
  showsPrec _ (VkStructureType 1000074002) = showString "VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR"
  showsPrec _ (VkStructureType 1000075000) = showString "VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR"
  showsPrec _ (VkStructureType 1000078000) = showString "VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000078001) = showString "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000078002) = showString "VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR"
  showsPrec _ (VkStructureType 1000078003) = showString "VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000079000) = showString "VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR"
  showsPrec _ (VkStructureType 1000079001) = showString "VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR"
  showsPrec _ (VkStructureType 1000080000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR"
  showsPrec _ (VkStructureType 1000081000) = showString "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT"
  showsPrec _ (VkStructureType 1000081001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000081002) = showString "VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT"
  showsPrec _ (VkStructureType 1000082000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR"
  showsPrec _ (VkStructureType 1000084000) = showString "VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR"
  showsPrec _ (VkStructureType 1000086000) = showString "VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX"
  showsPrec _ (VkStructureType 1000086001) = showString "VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX"
  showsPrec _ (VkStructureType 1000086002) = showString "VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX"
  showsPrec _ (VkStructureType 1000086003) = showString "VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX"
  showsPrec _ (VkStructureType 1000086004) = showString "VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX"
  showsPrec _ (VkStructureType 1000086005) = showString "VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX"
  showsPrec _ (VkStructureType 1000087000) = showString "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000090000) = showString "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT"
  showsPrec _ (VkStructureType 1000091000) = showString "VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT"
  showsPrec _ (VkStructureType 1000091001) = showString "VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT"
  showsPrec _ (VkStructureType 1000091002) = showString "VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT"
  showsPrec _ (VkStructureType 1000091003) = showString "VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000092000) = showString "VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE"
  showsPrec _ (VkStructureType 1000097000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX"
  showsPrec _ (VkStructureType 1000098000) = showString "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000099000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000099001) = showString "VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000101000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000101001) = showString "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000102000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000102001) = showString "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000105000) = showString "VK_STRUCTURE_TYPE_HDR_METADATA_EXT"
  showsPrec _ (VkStructureType 1000109000) = showString "VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR"
  showsPrec _ (VkStructureType 1000109001) = showString "VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR"
  showsPrec _ (VkStructureType 1000109002) = showString "VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR"
  showsPrec _ (VkStructureType 1000109003) = showString "VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR"
  showsPrec _ (VkStructureType 1000109004) = showString "VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR"
  showsPrec _ (VkStructureType 1000109005) = showString "VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR"
  showsPrec _ (VkStructureType 1000109006) = showString "VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR"
  showsPrec _ (VkStructureType 1000111000) = showString "VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR"
  showsPrec _ (VkStructureType 1000114000) = showString "VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000114001) = showString "VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000114002) = showString "VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000115000) = showString "VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR"
  showsPrec _ (VkStructureType 1000115001) = showString "VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR"
  showsPrec _ (VkStructureType 1000119000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR"
  showsPrec _ (VkStructureType 1000119001) = showString "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR"
  showsPrec _ (VkStructureType 1000119002) = showString "VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR"
  showsPrec _ (VkStructureType 1000121000) = showString "VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR"
  showsPrec _ (VkStructureType 1000121001) = showString "VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR"
  showsPrec _ (VkStructureType 1000121002) = showString "VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR"
  showsPrec _ (VkStructureType 1000121003) = showString "VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR"
  showsPrec _ (VkStructureType 1000121004) = showString "VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR"
  showsPrec _ (VkStructureType 1000122000) = showString "VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK"
  showsPrec _ (VkStructureType 1000123000) = showString "VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK"
  showsPrec _ (VkStructureType 1000128000) = showString "VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT"
  showsPrec _ (VkStructureType 1000128001) = showString "VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT"
  showsPrec _ (VkStructureType 1000128002) = showString "VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT"
  showsPrec _ (VkStructureType 1000128003) = showString "VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT"
  showsPrec _ (VkStructureType 1000128004) = showString "VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000129000) = showString "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID"
  showsPrec _ (VkStructureType 1000129001) = showString "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID"
  showsPrec _ (VkStructureType 1000129002) = showString "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID"
  showsPrec _ (VkStructureType 1000129003) = showString "VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID"
  showsPrec _ (VkStructureType 1000129004) = showString "VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID"
  showsPrec _ (VkStructureType 1000129005) = showString "VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID"
  showsPrec _ (VkStructureType 1000130000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000130001) = showString "VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000138000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000138001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000138002) = showString "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT"
  showsPrec _ (VkStructureType 1000138003) = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000143000) = showString "VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT"
  showsPrec _ (VkStructureType 1000143001) = showString "VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT"
  showsPrec _ (VkStructureType 1000143002) = showString "VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000143003) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000143004) = showString "VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000147000) = showString "VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000148000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000148001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000148002) = showString "VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000149000) = showString "VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000152000) = showString "VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000158000) = showString "VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT"
  showsPrec _ (VkStructureType 1000158001) = showString "VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000158002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT"
  showsPrec _ (VkStructureType 1000158003) = showString "VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000158004) = showString "VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000158005) = showString "VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000160000) = showString "VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000160001) = showString "VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000161000) = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000161001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000161002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000161003) = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000161004) = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT"
  showsPrec _ (VkStructureType 1000164000) = showString "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000164001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV"
  showsPrec _ (VkStructureType 1000164002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV"
  showsPrec _ (VkStructureType 1000164005) = showString "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000165000) = showString "VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000165001) = showString "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000165003) = showString "VK_STRUCTURE_TYPE_GEOMETRY_NV"
  showsPrec _ (VkStructureType 1000165004) = showString "VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV"
  showsPrec _ (VkStructureType 1000165005) = showString "VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV"
  showsPrec _ (VkStructureType 1000165006) = showString "VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV"
  showsPrec _ (VkStructureType 1000165007) = showString "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV"
  showsPrec _ (VkStructureType 1000165008) = showString "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV"
  showsPrec _ (VkStructureType 1000165009) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV"
  showsPrec _ (VkStructureType 1000165011) = showString "VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000165012) = showString "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV"
  showsPrec _ (VkStructureType 1000166000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV"
  showsPrec _ (VkStructureType 1000166001) = showString "VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000170000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT"
  showsPrec _ (VkStructureType 1000170001) = showString "VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000174000) = showString "VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000177000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR"
  showsPrec _ (VkStructureType 1000178000) = showString "VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT"
  showsPrec _ (VkStructureType 1000178001) = showString "VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000178002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000180000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR"
  showsPrec _ (VkStructureType 1000184000) = showString "VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT"
  showsPrec _ (VkStructureType 1000185000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD"
  showsPrec _ (VkStructureType 1000189000) = showString "VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD"
  showsPrec _ (VkStructureType 1000190000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000190001) = showString "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000190002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000191000) = showString "VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP"
  showsPrec _ (VkStructureType 1000192000) = showString "VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000196000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR"
  showsPrec _ (VkStructureType 1000197000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR"
  showsPrec _ (VkStructureType 1000199000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR"
  showsPrec _ (VkStructureType 1000199001) = showString "VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR"
  showsPrec _ (VkStructureType 1000201000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV"
  showsPrec _ (VkStructureType 1000202000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV"
  showsPrec _ (VkStructureType 1000202001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV"
  showsPrec _ (VkStructureType 1000203000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV"
  showsPrec _ (VkStructureType 1000204000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV"
  showsPrec _ (VkStructureType 1000205000) = showString "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000205002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV"
  showsPrec _ (VkStructureType 1000206000) = showString "VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV"
  showsPrec _ (VkStructureType 1000206001) = showString "VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV"
  showsPrec _ (VkStructureType 1000211000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR"
  showsPrec _ (VkStructureType 1000212000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000213000) = showString "VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD"
  showsPrec _ (VkStructureType 1000213001) = showString "VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD"
  showsPrec _ (VkStructureType 1000214000) = showString "VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA"
  showsPrec _ (VkStructureType 1000217000) = showString "VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000218000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000218001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000218002) = showString "VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000221000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000237000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000238000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000238001) = showString "VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000239000) = showString "VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR"
  showsPrec _ (VkStructureType 1000240000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV"
  showsPrec _ (VkStructureType 1000244000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000244001) = showString "VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT"
  showsPrec _ (VkStructureType 1000244002) = showString "VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000246000) = showString "VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000247000) = showString "VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000249000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV"
  showsPrec _ (VkStructureType 1000249001) = showString "VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV"
  showsPrec _ (VkStructureType 1000249002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV"
  showsPrec _ (VkStructureType 1000252000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000255000) = showString "VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT"
  showsPrec _ (VkStructureType 1000255002) = showString "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT"
  showsPrec _ (VkStructureType 1000255001) = showString "VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT"
  showsPrec _ (VkStructureType 1000256000) = showString "VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000261000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT"
  showsPrec p (VkStructureType x) = showParen (p >= 11) (showString "VkStructureType " . showsPrec 11 x)

instance Read VkStructureType where
  readPrec = parens ( choose [ ("VK_STRUCTURE_TYPE_APPLICATION_INFO",                          pure VK_STRUCTURE_TYPE_APPLICATION_INFO)
                             , ("VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO",                      pure VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO",                  pure VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO",                        pure VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_SUBMIT_INFO",                               pure VK_STRUCTURE_TYPE_SUBMIT_INFO)
                             , ("VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO",                      pure VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO)
                             , ("VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE",                       pure VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE)
                             , ("VK_STRUCTURE_TYPE_BIND_SPARSE_INFO",                          pure VK_STRUCTURE_TYPE_BIND_SPARSE_INFO)
                             , ("VK_STRUCTURE_TYPE_FENCE_CREATE_INFO",                         pure VK_STRUCTURE_TYPE_FENCE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO",                     pure VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_EVENT_CREATE_INFO",                         pure VK_STRUCTURE_TYPE_EVENT_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO",                    pure VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO",                        pure VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO",                   pure VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO",                         pure VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO",                    pure VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO",                 pure VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO",                pure VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO",         pure VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO",   pure VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO",   pure VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO",       pure VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO",  pure VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO",    pure VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO",  pure VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO",    pure VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO",        pure VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO",             pure VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO",              pure VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO",               pure VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO",                       pure VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO",         pure VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO",               pure VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO",              pure VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO)
                             , ("VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET",                      pure VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET)
                             , ("VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET",                       pure VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET)
                             , ("VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO",                   pure VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO",                   pure VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO",                  pure VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO",              pure VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO",           pure VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO",                 pure VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO",                    pure VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO)
                             , ("VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER",                     pure VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER)
                             , ("VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER",                      pure VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER)
                             , ("VK_STRUCTURE_TYPE_MEMORY_BARRIER",                            pure VK_STRUCTURE_TYPE_MEMORY_BARRIER)
                             , ("VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO",               pure VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO",                 pure VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES",                             pure (VkStructureType 1000094000))
                             , ("VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO",                                         pure (VkStructureType 1000157000))
                             , ("VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO",                                          pure (VkStructureType 1000157001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES",                          pure (VkStructureType 1000083000))
                             , ("VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS",                                   pure (VkStructureType 1000127000))
                             , ("VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO",                                  pure (VkStructureType 1000127001))
                             , ("VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO",                                      pure (VkStructureType 1000060000))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO",                             pure (VkStructureType 1000060003))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO",                          pure (VkStructureType 1000060004))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO",                                        pure (VkStructureType 1000060005))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO",                                   pure (VkStructureType 1000060006))
                             , ("VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO",                            pure (VkStructureType 1000060013))
                             , ("VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO",                             pure (VkStructureType 1000060014))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES",                                pure (VkStructureType 1000070000))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO",                                 pure (VkStructureType 1000070001))
                             , ("VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2",                               pure (VkStructureType 1000146000))
                             , ("VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2",                                pure (VkStructureType 1000146001))
                             , ("VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2",                         pure (VkStructureType 1000146002))
                             , ("VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2",                                           pure (VkStructureType 1000146003))
                             , ("VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2",                              pure (VkStructureType 1000146004))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2",                                      pure (VkStructureType 1000059000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2",                                    pure (VkStructureType 1000059001))
                             , ("VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2",                                             pure (VkStructureType 1000059002))
                             , ("VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2",                                       pure (VkStructureType 1000059003))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2",                             pure (VkStructureType 1000059004))
                             , ("VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2",                                       pure (VkStructureType 1000059005))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2",                             pure (VkStructureType 1000059006))
                             , ("VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2",                                pure (VkStructureType 1000059007))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2",                      pure (VkStructureType 1000059008))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES",                       pure (VkStructureType 1000117000))
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO",                 pure (VkStructureType 1000117001))
                             , ("VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO",                                    pure (VkStructureType 1000117002))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO",           pure (VkStructureType 1000117003))
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO",                               pure (VkStructureType 1000053000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES",                              pure (VkStructureType 1000053001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES",                            pure (VkStructureType 1000053002))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES",                      pure (VkStructureType 1000120000))
                             , ("VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO",                                           pure (VkStructureType 1000145000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES",                       pure (VkStructureType 1000145001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES",                     pure (VkStructureType 1000145002))
                             , ("VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2",                                             pure (VkStructureType 1000145003))
                             , ("VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO",                            pure (VkStructureType 1000156000))
                             , ("VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO",                                   pure (VkStructureType 1000156001))
                             , ("VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO",                                    pure (VkStructureType 1000156002))
                             , ("VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO",                            pure (VkStructureType 1000156003))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES",               pure (VkStructureType 1000156004))
                             , ("VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES",                pure (VkStructureType 1000156005))
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO",                          pure (VkStructureType 1000085000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO",                      pure (VkStructureType 1000071000))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES",                                pure (VkStructureType 1000071001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO",                            pure (VkStructureType 1000071002))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES",                                      pure (VkStructureType 1000071003))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES",                                   pure (VkStructureType 1000071004))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO",                              pure (VkStructureType 1000072000))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO",                               pure (VkStructureType 1000072001))
                             , ("VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO",                                     pure (VkStructureType 1000072002))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO",                             pure (VkStructureType 1000112000))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES",                                       pure (VkStructureType 1000112001))
                             , ("VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO",                                        pure (VkStructureType 1000113000))
                             , ("VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO",                                    pure (VkStructureType 1000077000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO",                         pure (VkStructureType 1000076000))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES",                                   pure (VkStructureType 1000076001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES",                        pure (VkStructureType 1000168000))
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT",                                   pure (VkStructureType 1000168001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES",                 pure (VkStructureType 1000063000))
                             , ("VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR",                                       pure (VkStructureType 1000001000))
                             , ("VK_STRUCTURE_TYPE_PRESENT_INFO_KHR",                                                pure (VkStructureType 1000001001))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR",                           pure (VkStructureType 1000060007))
                             , ("VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR",                                 pure (VkStructureType 1000060008))
                             , ("VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR",                            pure (VkStructureType 1000060009))
                             , ("VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR",                                     pure (VkStructureType 1000060010))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR",                                   pure (VkStructureType 1000060011))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR",                          pure (VkStructureType 1000060012))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR",                                    pure (VkStructureType 1000002000))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR",                                 pure (VkStructureType 1000002001))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR",                                        pure (VkStructureType 1000003000))
                             , ("VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR",                                    pure (VkStructureType 1000004000))
                             , ("VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR",                                     pure (VkStructureType 1000005000))
                             , ("VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR",                                 pure (VkStructureType 1000006000))
                             , ("VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR",                                 pure (VkStructureType 1000008000))
                             , ("VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR",                                   pure (VkStructureType 1000009000))
                             , ("VK_STRUCTURE_TYPE_NATIVE_BUFFER_ANDROID",                                           pure (VkStructureType 1000010000))
                             , ("VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT",                           pure (VkStructureType 1000011000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD",            pure (VkStructureType 1000018000))
                             , ("VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT",                               pure (VkStructureType 1000022000))
                             , ("VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT",                                pure (VkStructureType 1000022001))
                             , ("VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT",                                    pure (VkStructureType 1000022002))
                             , ("VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV",                       pure (VkStructureType 1000026000))
                             , ("VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV",                      pure (VkStructureType 1000026001))
                             , ("VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV",                    pure (VkStructureType 1000026002))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT",                 pure (VkStructureType 1000028000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT",               pure (VkStructureType 1000028001))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT",             pure (VkStructureType 1000028002))
                             , ("VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX",                                      pure (VkStructureType 1000030000))
                             , ("VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD",                        pure (VkStructureType 1000041000))
                             , ("VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP",                       pure (VkStructureType 1000049000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV",                pure (VkStructureType 1000050000))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV",                            pure (VkStructureType 1000056000))
                             , ("VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV",                                  pure (VkStructureType 1000056001))
                             , ("VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV",                              pure (VkStructureType 1000057000))
                             , ("VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV",                              pure (VkStructureType 1000057001))
                             , ("VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV",                       pure (VkStructureType 1000058000))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR",                           pure (VkStructureType 1000060007))
                             , ("VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR",                                 pure (VkStructureType 1000060008))
                             , ("VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR",                            pure (VkStructureType 1000060009))
                             , ("VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR",                                     pure (VkStructureType 1000060010))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR",                                   pure (VkStructureType 1000060011))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR",                          pure (VkStructureType 1000060012))
                             , ("VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT",                                            pure (VkStructureType 1000061000))
                             , ("VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN",                                       pure (VkStructureType 1000062000))
                             , ("VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT",                                 pure (VkStructureType 1000067000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT",                        pure (VkStructureType 1000067001))
                             , ("VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR",                             pure (VkStructureType 1000073000))
                             , ("VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR",                             pure (VkStructureType 1000073001))
                             , ("VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR",                              pure (VkStructureType 1000073002))
                             , ("VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR",                                pure (VkStructureType 1000073003))
                             , ("VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR",                                       pure (VkStructureType 1000074000))
                             , ("VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR",                                        pure (VkStructureType 1000074001))
                             , ("VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR",                                          pure (VkStructureType 1000074002))
                             , ("VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR",                      pure (VkStructureType 1000075000))
                             , ("VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR",                          pure (VkStructureType 1000078000))
                             , ("VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR",                          pure (VkStructureType 1000078001))
                             , ("VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR",                                     pure (VkStructureType 1000078002))
                             , ("VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR",                             pure (VkStructureType 1000078003))
                             , ("VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR",                                    pure (VkStructureType 1000079000))
                             , ("VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR",                                       pure (VkStructureType 1000079001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR",                  pure (VkStructureType 1000080000))
                             , ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT",       pure (VkStructureType 1000081000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT",              pure (VkStructureType 1000081001))
                             , ("VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT",                            pure (VkStructureType 1000081002))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR",                       pure (VkStructureType 1000082000))
                             , ("VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR",                                             pure (VkStructureType 1000084000))
                             , ("VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX",                                    pure (VkStructureType 1000086000))
                             , ("VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX",                        pure (VkStructureType 1000086001))
                             , ("VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX",                                   pure (VkStructureType 1000086002))
                             , ("VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX",                         pure (VkStructureType 1000086003))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX",                            pure (VkStructureType 1000086004))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX",                          pure (VkStructureType 1000086005))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV",                pure (VkStructureType 1000087000))
                             , ("VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT",                                      pure (VkStructureType 1000090000))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT",                                          pure (VkStructureType 1000091000))
                             , ("VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT",                                           pure (VkStructureType 1000091001))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT",                                          pure (VkStructureType 1000091002))
                             , ("VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT",                               pure (VkStructureType 1000091003))
                             , ("VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE",                                       pure (VkStructureType 1000092000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX",    pure (VkStructureType 1000097000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV",                  pure (VkStructureType 1000098000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT",                pure (VkStructureType 1000099000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT",                pure (VkStructureType 1000099001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT",       pure (VkStructureType 1000101000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT",       pure (VkStructureType 1000101001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT",                  pure (VkStructureType 1000102000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT",         pure (VkStructureType 1000102001))
                             , ("VK_STRUCTURE_TYPE_HDR_METADATA_EXT",                                                pure (VkStructureType 1000105000))
                             , ("VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR",                                    pure (VkStructureType 1000109000))
                             , ("VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR",                                      pure (VkStructureType 1000109001))
                             , ("VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR",                                       pure (VkStructureType 1000109002))
                             , ("VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR",                                        pure (VkStructureType 1000109003))
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR",                                   pure (VkStructureType 1000109004))
                             , ("VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR",                                          pure (VkStructureType 1000109005))
                             , ("VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR",                                            pure (VkStructureType 1000109006))
                             , ("VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR",                         pure (VkStructureType 1000111000))
                             , ("VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR",                              pure (VkStructureType 1000114000))
                             , ("VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR",                              pure (VkStructureType 1000114001))
                             , ("VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR",                                 pure (VkStructureType 1000114002))
                             , ("VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR",                                        pure (VkStructureType 1000115000))
                             , ("VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR",                                           pure (VkStructureType 1000115001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR",                              pure (VkStructureType 1000119000))
                             , ("VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR",                                      pure (VkStructureType 1000119001))
                             , ("VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR",                                            pure (VkStructureType 1000119002))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR",                                        pure (VkStructureType 1000121000))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR",                                  pure (VkStructureType 1000121001))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR",                                   pure (VkStructureType 1000121002))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR",                                        pure (VkStructureType 1000121003))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR",                                pure (VkStructureType 1000121004))
                             , ("VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK",                                     pure (VkStructureType 1000122000))
                             , ("VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK",                                   pure (VkStructureType 1000123000))
                             , ("VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT",                                pure (VkStructureType 1000128000))
                             , ("VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT",                                 pure (VkStructureType 1000128001))
                             , ("VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT",                                           pure (VkStructureType 1000128002))
                             , ("VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT",                         pure (VkStructureType 1000128003))
                             , ("VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT",                           pure (VkStructureType 1000128004))
                             , ("VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID",                           pure (VkStructureType 1000129000))
                             , ("VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID",                      pure (VkStructureType 1000129001))
                             , ("VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID",               pure (VkStructureType 1000129002))
                             , ("VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID",                     pure (VkStructureType 1000129003))
                             , ("VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID",                 pure (VkStructureType 1000129004))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID",                                         pure (VkStructureType 1000129005))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT",            pure (VkStructureType 1000130000))
                             , ("VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT",                          pure (VkStructureType 1000130001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT",               pure (VkStructureType 1000138000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT",             pure (VkStructureType 1000138001))
                             , ("VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT",                   pure (VkStructureType 1000138002))
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT",            pure (VkStructureType 1000138003))
                             , ("VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT",                                       pure (VkStructureType 1000143000))
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT",                     pure (VkStructureType 1000143001))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT",                 pure (VkStructureType 1000143002))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT",                 pure (VkStructureType 1000143003))
                             , ("VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT",                                      pure (VkStructureType 1000143004))
                             , ("VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR",                               pure (VkStructureType 1000147000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT",           pure (VkStructureType 1000148000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT",         pure (VkStructureType 1000148001))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT",             pure (VkStructureType 1000148002))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV",                 pure (VkStructureType 1000149000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV",               pure (VkStructureType 1000152000))
                             , ("VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT",                         pure (VkStructureType 1000158000))
                             , ("VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT",                              pure (VkStructureType 1000158001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT",              pure (VkStructureType 1000158002))
                             , ("VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT",                  pure (VkStructureType 1000158003))
                             , ("VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT",              pure (VkStructureType 1000158004))
                             , ("VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT",                        pure (VkStructureType 1000158005))
                             , ("VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT",                                pure (VkStructureType 1000160000))
                             , ("VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT",                  pure (VkStructureType 1000160001))
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT",             pure (VkStructureType 1000161000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT",                pure (VkStructureType 1000161001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT",              pure (VkStructureType 1000161002))
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT",      pure (VkStructureType 1000161003))
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT",     pure (VkStructureType 1000161004))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV",       pure (VkStructureType 1000164000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV",                  pure (VkStructureType 1000164001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV",                pure (VkStructureType 1000164002))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV",      pure (VkStructureType 1000164005))
                             , ("VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV",                             pure (VkStructureType 1000165000))
                             , ("VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV",                           pure (VkStructureType 1000165001))
                             , ("VK_STRUCTURE_TYPE_GEOMETRY_NV",                                                     pure (VkStructureType 1000165003))
                             , ("VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV",                                           pure (VkStructureType 1000165004))
                             , ("VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV",                                                pure (VkStructureType 1000165005))
                             , ("VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV",                      pure (VkStructureType 1000165006))
                             , ("VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV",                  pure (VkStructureType 1000165007))
                             , ("VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV",              pure (VkStructureType 1000165008))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV",                       pure (VkStructureType 1000165009))
                             , ("VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV",                         pure (VkStructureType 1000165011))
                             , ("VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV",                                  pure (VkStructureType 1000165012))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV",        pure (VkStructureType 1000166000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV",      pure (VkStructureType 1000166001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT",                pure (VkStructureType 1000170000))
                             , ("VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT",             pure (VkStructureType 1000170001))
                             , ("VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT",                    pure (VkStructureType 1000174000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR",                       pure (VkStructureType 1000177000))
                             , ("VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT",                             pure (VkStructureType 1000178000))
                             , ("VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT",                              pure (VkStructureType 1000178001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT",             pure (VkStructureType 1000178002))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR",                pure (VkStructureType 1000180000))
                             , ("VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT",                                   pure (VkStructureType 1000184000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD",                      pure (VkStructureType 1000185000))
                             , ("VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD",                    pure (VkStructureType 1000189000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT",         pure (VkStructureType 1000190000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT",             pure (VkStructureType 1000190001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT",           pure (VkStructureType 1000190002))
                             , ("VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP",                                         pure (VkStructureType 1000191000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT",                      pure (VkStructureType 1000192000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR",                           pure (VkStructureType 1000196000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR",                   pure (VkStructureType 1000197000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR",            pure (VkStructureType 1000199000))
                             , ("VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR",                   pure (VkStructureType 1000199001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV",          pure (VkStructureType 1000201000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV",                         pure (VkStructureType 1000202000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV",                       pure (VkStructureType 1000202001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV",         pure (VkStructureType 1000203000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV",              pure (VkStructureType 1000204000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV",        pure (VkStructureType 1000205000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV",                   pure (VkStructureType 1000205002))
                             , ("VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV",                                              pure (VkStructureType 1000206000))
                             , ("VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV",                           pure (VkStructureType 1000206001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR",                pure (VkStructureType 1000211000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT",                     pure (VkStructureType 1000212000))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD",                     pure (VkStructureType 1000213000))
                             , ("VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD",                    pure (VkStructureType 1000213001))
                             , ("VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA",                           pure (VkStructureType 1000214000))
                             , ("VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT",                                   pure (VkStructureType 1000217000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT",               pure (VkStructureType 1000218000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT",             pure (VkStructureType 1000218001))
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT",                pure (VkStructureType 1000218002))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT",                pure (VkStructureType 1000221000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT",                    pure (VkStructureType 1000237000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT",                    pure (VkStructureType 1000238000))
                             , ("VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT",                               pure (VkStructureType 1000238001))
                             , ("VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR",                              pure (VkStructureType 1000239000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV", pure (VkStructureType 1000240000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT",              pure (VkStructureType 1000244000))
                             , ("VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT",                                  pure (VkStructureType 1000244001))
                             , ("VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT",                           pure (VkStructureType 1000244002))
                             , ("VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT",                             pure (VkStructureType 1000246000))
                             , ("VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT",                                         pure (VkStructureType 1000247000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV",                  pure (VkStructureType 1000249000))
                             , ("VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV",                                pure (VkStructureType 1000249001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV",                pure (VkStructureType 1000249002))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT",                 pure (VkStructureType 1000252000))
                             , ("VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT",                          pure (VkStructureType 1000255000))
                             , ("VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT",                  pure (VkStructureType 1000255002))
                             , ("VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT",                    pure (VkStructureType 1000255001))
                             , ("VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT",                                pure (VkStructureType 1000256000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT",                   pure (VkStructureType 1000261000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkStructureType")
                        v <- step readPrec
                        pure (VkStructureType v)
                        )
                    )

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_APPLICATION_INFO"
pattern VK_STRUCTURE_TYPE_APPLICATION_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_APPLICATION_INFO = VkStructureType 0

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = VkStructureType 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO = VkStructureType 2

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO = VkStructureType 3

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBMIT_INFO"
pattern VK_STRUCTURE_TYPE_SUBMIT_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SUBMIT_INFO = VkStructureType 4

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO"
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = VkStructureType 5

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE"
pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE :: VkStructureType
pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE = VkStructureType 6

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_SPARSE_INFO"
pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO = VkStructureType 7

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FENCE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO = VkStructureType 8

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = VkStructureType 9

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EVENT_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO = VkStructureType 10

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO = VkStructureType 11

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO = VkStructureType 12

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO = VkStructureType 13

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO = VkStructureType 14

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO = VkStructureType 15

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO = VkStructureType 16

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO = VkStructureType 17

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO = VkStructureType 18

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO = VkStructureType 19

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = VkStructureType 20

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO = VkStructureType 21

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO = VkStructureType 22

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO = VkStructureType 23

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO = VkStructureType 24

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO = VkStructureType 25

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO = VkStructureType 26

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO = VkStructureType 27

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO = VkStructureType 28

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO = VkStructureType 29

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO = VkStructureType 30

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO = VkStructureType 31

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO = VkStructureType 32

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO = VkStructureType 33

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO = VkStructureType 34

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET"
pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET :: VkStructureType
pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = VkStructureType 35

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET"
pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET :: VkStructureType
pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET = VkStructureType 36

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO = VkStructureType 37

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO = VkStructureType 38

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO = VkStructureType 39

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO"
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO = VkStructureType 40

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO"
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO = VkStructureType 41

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO"
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO = VkStructureType 42

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO = VkStructureType 43

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER"
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER = VkStructureType 44

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER"
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = VkStructureType 45

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_BARRIER"
pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER = VkStructureType 46

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO = VkStructureType 47

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO = VkStructureType 48

-- ** VkVendorId

-- No documentation found for TopLevel "VkVendorId"
newtype VkVendorId = VkVendorId Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkVendorId where
  showsPrec _ VK_VENDOR_ID_VIV = showString "VK_VENDOR_ID_VIV"
  showsPrec _ VK_VENDOR_ID_VSI = showString "VK_VENDOR_ID_VSI"
  showsPrec _ VK_VENDOR_ID_KAZAN = showString "VK_VENDOR_ID_KAZAN"
  showsPrec p (VkVendorId x) = showParen (p >= 11) (showString "VkVendorId " . showsPrec 11 x)

instance Read VkVendorId where
  readPrec = parens ( choose [ ("VK_VENDOR_ID_VIV",   pure VK_VENDOR_ID_VIV)
                             , ("VK_VENDOR_ID_VSI",   pure VK_VENDOR_ID_VSI)
                             , ("VK_VENDOR_ID_KAZAN", pure VK_VENDOR_ID_KAZAN)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkVendorId")
                        v <- step readPrec
                        pure (VkVendorId v)
                        )
                    )

-- No documentation found for Nested "VkVendorId" "VK_VENDOR_ID_VIV"
pattern VK_VENDOR_ID_VIV :: VkVendorId
pattern VK_VENDOR_ID_VIV = VkVendorId 65537

-- No documentation found for Nested "VkVendorId" "VK_VENDOR_ID_VSI"
pattern VK_VENDOR_ID_VSI :: VkVendorId
pattern VK_VENDOR_ID_VSI = VkVendorId 65538

-- No documentation found for Nested "VkVendorId" "VK_VENDOR_ID_KAZAN"
pattern VK_VENDOR_ID_KAZAN :: VkVendorId
pattern VK_VENDOR_ID_KAZAN = VkVendorId 65539

-- | A class for initializing things with all zero data
class Zero a where
  zero :: a

instance (KnownNat n, Storable a, Zero a) => Zero (Data.Vector.Storable.Sized.Vector n a) where
  zero = Data.Vector.Storable.Sized.replicate zero

instance Zero (Ptr a) where
  zero = nullPtr

instance Zero Int8 where
  zero = 0

instance Zero Int16 where
  zero = 0

instance Zero Int32 where
  zero = 0

instance Zero Int64 where
  zero = 0

instance Zero Word8 where
  zero = 0

instance Zero Word16 where
  zero = 0

instance Zero Word32 where
  zero = 0

instance Zero Word64 where
  zero = 0

instance Zero Float where
  zero = 0

instance Zero CFloat where
  zero = 0

instance Zero CChar where
  zero = 0

instance Zero CSize where
  zero = 0

instance Zero CInt where
  zero = 0
