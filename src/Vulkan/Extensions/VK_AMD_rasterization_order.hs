{-# language CPP #-}
-- | = Name
--
-- VK_AMD_rasterization_order - device extension
--
-- == VK_AMD_rasterization_order
--
-- [__Name String__]
--     @VK_AMD_rasterization_order@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     19
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_AMD_rasterization_order:%20&body=@drakos-amd%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-04-25
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Jaakko Konttinen, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Graham Sellers, AMD
--
--     -   Dominik Witczak, AMD
--
-- == Description
--
-- This extension introduces the possibility for the application to control
-- the order of primitive rasterization. In unextended Vulkan, the
-- following stages are guaranteed to execute in /API order/:
--
-- -   depth bounds test
--
-- -   stencil test, stencil op, and stencil write
--
-- -   depth test and depth write
--
-- -   occlusion queries
--
-- -   blending, logic op, and color write
--
-- This extension enables applications to opt into a relaxed,
-- implementation defined primitive rasterization order that may allow
-- better parallel processing of primitives and thus enabling higher
-- primitive throughput. It is applicable in cases where the primitive
-- rasterization order is known to not affect the output of the rendering
-- or any differences caused by a different rasterization order are not a
-- concern from the point of view of the application’s purpose.
--
-- A few examples of cases when using the relaxed primitive rasterization
-- order would not have an effect on the final rendering:
--
-- -   If the primitives rendered are known to not overlap in framebuffer
--     space.
--
-- -   If depth testing is used with a comparison operator of
--     'Vulkan.Core10.Enums.CompareOp.COMPARE_OP_LESS',
--     'Vulkan.Core10.Enums.CompareOp.COMPARE_OP_LESS_OR_EQUAL',
--     'Vulkan.Core10.Enums.CompareOp.COMPARE_OP_GREATER', or
--     'Vulkan.Core10.Enums.CompareOp.COMPARE_OP_GREATER_OR_EQUAL', and the
--     primitives rendered are known to not overlap in clip space.
--
-- -   If depth testing is not used and blending is enabled for all
--     attachments with a commutative blend operator.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationStateRasterizationOrderAMD'
--
-- == New Enums
--
-- -   'RasterizationOrderAMD'
--
-- == New Enum Constants
--
-- -   'AMD_RASTERIZATION_ORDER_EXTENSION_NAME'
--
-- -   'AMD_RASTERIZATION_ORDER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD'
--
-- == Issues
--
-- 1) How is this extension useful to application developers?
--
-- __RESOLVED__: Allows them to increase primitive throughput for cases
-- when strict API order rasterization is not important due to the nature
-- of the content, the configuration used, or the requirements towards the
-- output of the rendering.
--
-- 2) How does this extension interact with content optimizations aiming to
-- reduce overdraw by appropriately ordering the input primitives?
--
-- __RESOLVED__: While the relaxed rasterization order might somewhat limit
-- the effectiveness of such content optimizations, most of the benefits of
-- it are expected to be retained even when the relaxed rasterization order
-- is used, so applications /should/ still apply these optimizations even
-- if they intend to use the extension.
--
-- 3) Are there any guarantees about the primitive rasterization order when
-- using the new relaxed mode?
--
-- __RESOLVED__: No. In this case the rasterization order is completely
-- implementation dependent, but in practice it is expected to partially
-- still follow the order of incoming primitives.
--
-- 4) Does the new relaxed rasterization order have any adverse effect on
-- repeatability and other invariance rules of the API?
--
-- __RESOLVED__: Yes, in the sense that it extends the list of exceptions
-- when the repeatability requirement does not apply.
--
-- == Examples
--
-- None
--
-- == Issues
--
-- None
--
-- == Version History
--
-- -   Revision 1, 2016-04-25 (Daniel Rakos)
--
--     -   Initial draft.
--
-- = See Also
--
-- 'PipelineRasterizationStateRasterizationOrderAMD',
-- 'RasterizationOrderAMD'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_rasterization_order Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_rasterization_order  ( PipelineRasterizationStateRasterizationOrderAMD(..)
                                                     , RasterizationOrderAMD( RASTERIZATION_ORDER_STRICT_AMD
                                                                            , RASTERIZATION_ORDER_RELAXED_AMD
                                                                            , ..
                                                                            )
                                                     , AMD_RASTERIZATION_ORDER_SPEC_VERSION
                                                     , pattern AMD_RASTERIZATION_ORDER_SPEC_VERSION
                                                     , AMD_RASTERIZATION_ORDER_EXTENSION_NAME
                                                     , pattern AMD_RASTERIZATION_ORDER_EXTENSION_NAME
                                                     ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD))
-- | VkPipelineRasterizationStateRasterizationOrderAMD - Structure defining
-- rasterization order for a graphics pipeline
--
-- == Valid Usage (Implicit)
--
-- If the @VK_AMD_rasterization_order@ device extension is not enabled or
-- the application does not request a particular rasterization order
-- through specifying a 'PipelineRasterizationStateRasterizationOrderAMD'
-- structure then the rasterization order used by the graphics pipeline
-- defaults to 'RASTERIZATION_ORDER_STRICT_AMD'.
--
-- = See Also
--
-- 'RasterizationOrderAMD',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRasterizationStateRasterizationOrderAMD = PipelineRasterizationStateRasterizationOrderAMD
  { -- | @rasterizationOrder@ is a 'RasterizationOrderAMD' value specifying the
    -- primitive rasterization order to use.
    --
    -- #VUID-VkPipelineRasterizationStateRasterizationOrderAMD-rasterizationOrder-parameter#
    -- @rasterizationOrder@ /must/ be a valid 'RasterizationOrderAMD' value
    rasterizationOrder :: RasterizationOrderAMD }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRasterizationStateRasterizationOrderAMD)
#endif
deriving instance Show PipelineRasterizationStateRasterizationOrderAMD

instance ToCStruct PipelineRasterizationStateRasterizationOrderAMD where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationStateRasterizationOrderAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RasterizationOrderAMD)) (rasterizationOrder)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RasterizationOrderAMD)) (zero)
    f

instance FromCStruct PipelineRasterizationStateRasterizationOrderAMD where
  peekCStruct p = do
    rasterizationOrder <- peek @RasterizationOrderAMD ((p `plusPtr` 16 :: Ptr RasterizationOrderAMD))
    pure $ PipelineRasterizationStateRasterizationOrderAMD
             rasterizationOrder

instance Storable PipelineRasterizationStateRasterizationOrderAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationStateRasterizationOrderAMD where
  zero = PipelineRasterizationStateRasterizationOrderAMD
           zero


-- | VkRasterizationOrderAMD - Specify rasterization order for a graphics
-- pipeline
--
-- = See Also
--
-- 'PipelineRasterizationStateRasterizationOrderAMD'
newtype RasterizationOrderAMD = RasterizationOrderAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'RASTERIZATION_ORDER_STRICT_AMD' specifies that operations for each
-- primitive in a subpass /must/ occur in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-order primitive order>.
pattern RASTERIZATION_ORDER_STRICT_AMD = RasterizationOrderAMD 0
-- | 'RASTERIZATION_ORDER_RELAXED_AMD' specifies that operations for each
-- primitive in a subpass /may/ not occur in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-order primitive order>.
pattern RASTERIZATION_ORDER_RELAXED_AMD = RasterizationOrderAMD 1
{-# complete RASTERIZATION_ORDER_STRICT_AMD,
             RASTERIZATION_ORDER_RELAXED_AMD :: RasterizationOrderAMD #-}

instance Show RasterizationOrderAMD where
  showsPrec p = \case
    RASTERIZATION_ORDER_STRICT_AMD -> showString "RASTERIZATION_ORDER_STRICT_AMD"
    RASTERIZATION_ORDER_RELAXED_AMD -> showString "RASTERIZATION_ORDER_RELAXED_AMD"
    RasterizationOrderAMD x -> showParen (p >= 11) (showString "RasterizationOrderAMD " . showsPrec 11 x)

instance Read RasterizationOrderAMD where
  readPrec = parens (choose [("RASTERIZATION_ORDER_STRICT_AMD", pure RASTERIZATION_ORDER_STRICT_AMD)
                            , ("RASTERIZATION_ORDER_RELAXED_AMD", pure RASTERIZATION_ORDER_RELAXED_AMD)]
                     +++
                     prec 10 (do
                       expectP (Ident "RasterizationOrderAMD")
                       v <- step readPrec
                       pure (RasterizationOrderAMD v)))


type AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION"
pattern AMD_RASTERIZATION_ORDER_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1


type AMD_RASTERIZATION_ORDER_EXTENSION_NAME = "VK_AMD_rasterization_order"

-- No documentation found for TopLevel "VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME"
pattern AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_RASTERIZATION_ORDER_EXTENSION_NAME = "VK_AMD_rasterization_order"

