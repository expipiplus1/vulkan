{-# language CPP #-}
-- | = Name
--
-- VK_NV_viewport_swizzle - device extension
--
-- == VK_NV_viewport_swizzle
--
-- [__Name String__]
--     @VK_NV_viewport_swizzle@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     99
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
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_viewport_swizzle:%20&body=@pdaniell-nv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-12-22
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires @multiViewport@ and @geometryShader@
--         features to be useful.
--
-- [__Contributors__]
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension provides a new per-viewport swizzle that can modify the
-- position of primitives sent to each viewport. New viewport swizzle state
-- is added for each viewport, and a new position vector is computed for
-- each vertex by selecting from and optionally negating any of the four
-- components of the original position vector.
--
-- This new viewport swizzle is useful for a number of algorithms,
-- including single-pass cubemap rendering (broadcasting a primitive to
-- multiple faces and reorienting the vertex position for each face) and
-- voxel rasterization. The per-viewport component remapping and negation
-- provided by the swizzle allows application code to re-orient
-- three-dimensional geometry with a view along any of the __X__, __Y__, or
-- __Z__ axes. If a perspective projection and depth buffering is required,
-- 1\/W buffering should be used, as described in the single-pass cubemap
-- rendering example in the “Issues” section below.
--
-- == New Structures
--
-- -   'ViewportSwizzleNV'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo':
--
--     -   'PipelineViewportSwizzleStateCreateInfoNV'
--
-- == New Enums
--
-- -   'ViewportCoordinateSwizzleNV'
--
-- == New Bitmasks
--
-- -   'PipelineViewportSwizzleStateCreateFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_VIEWPORT_SWIZZLE_EXTENSION_NAME'
--
-- -   'NV_VIEWPORT_SWIZZLE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV'
--
-- == Issues
--
-- 1) Where does viewport swizzling occur in the pipeline?
--
-- __RESOLVED__: Despite being associated with the viewport, viewport
-- swizzling must happen prior to the viewport transform. In particular, it
-- needs to be performed before clipping and perspective division.
--
-- The viewport mask expansion (@VK_NV_viewport_array2@) and the viewport
-- swizzle could potentially be performed before or after transform
-- feedback, but feeding back several viewports worth of primitives with
-- different swizzles doesn’t seem particularly useful. This specification
-- applies the viewport mask and swizzle after transform feedback, and
-- makes primitive queries only count each primitive once.
--
-- 2) Any interesting examples of how this extension,
-- @VK_NV_viewport_array2@, and @VK_NV_geometry_shader_passthrough@ can be
-- used together in practice?
--
-- __RESOLVED__: One interesting use case for this extension is for
-- single-pass rendering to a cubemap. In this example, the application
-- would attach a cubemap texture to a layered FBO where the six cube faces
-- are treated as layers. Vertices are sent through the vertex shader
-- without applying a projection matrix, where the @gl_Position@ output is
-- (x,y,z,1) and the center of the cubemap is at (0,0,0). With unextended
-- Vulkan, one could have a conventional instanced geometry shader that
-- looks something like the following:
--
-- > layout(invocations = 6) in;     // separate invocation per face
-- > layout(triangles) in;
-- > layout(triangle_strip) out;
-- > layout(max_vertices = 3) out;
-- >
-- > in Inputs {
-- > vec2 texcoord;
-- > vec3 normal;
-- > vec4 baseColor;
-- > } v[];
-- >
-- >     out Outputs {
-- >     vec2 texcoord;
-- >     vec3 normal;
-- >     vec4 baseColor;
-- >     };
-- >
-- >     void main()
-- >     {
-- >     int face = gl_InvocationID;  // which face am I?
-- >
-- >     // Project gl_Position for each vertex onto the cube map face.
-- >     vec4 positions[3];
-- >     for (int i = 0; i < 3; i++) {
-- >         positions[i] = rotate(gl_in[i].gl_Position, face);
-- >     }
-- >
-- >     // If the primitive doesn't project onto this face, we're done.
-- >     if (shouldCull(positions)) {
-- >         return;
-- >     }
-- >
-- >     // Otherwise, emit a copy of the input primitive to the
-- >     // appropriate face (using gl_Layer).
-- >     for (int i = 0; i < 3; i++) {
-- >         gl_Layer = face;
-- >         gl_Position = positions[i];
-- >         texcoord = v[i].texcoord;
-- >         normal = v[i].normal;
-- >         baseColor = v[i].baseColor;
-- >         EmitVertex();
-- >     }
-- > }
--
-- With passthrough geometry shaders, this can be done using a much simpler
-- shader:
--
-- > layout(triangles) in;
-- > layout(passthrough) in Inputs {
-- >     vec2 texcoord;
-- >     vec3 normal;
-- >     vec4 baseColor;
-- > }
-- > layout(passthrough) in gl_PerVertex {
-- >     vec4 gl_Position;
-- > } gl_in[];
-- > layout(viewport_relative) out int gl_Layer;
-- >
-- > void main()
-- > {
-- >     // Figure out which faces the primitive projects onto and
-- >     // generate a corresponding viewport mask.
-- >     uint mask = 0;
-- >     for (int i = 0; i < 6; i++) {
-- >         if (!shouldCull(face)) {
-- >         mask |= 1U << i;
-- >         }
-- >     }
-- >     gl_ViewportMask = mask;
-- >     gl_Layer = 0;
-- > }
--
-- The application code is set up so that each of the six cube faces has a
-- separate viewport (numbered 0 to 5). Each face also has a separate
-- swizzle, programmed via the 'PipelineViewportSwizzleStateCreateInfoNV'
-- pipeline state. The viewport swizzle feature performs the coordinate
-- transformation handled by the @rotate@() function in the original
-- shader. The @viewport_relative@ layout qualifier says that the viewport
-- number (0 to 5) is added to the base @gl_Layer@ value of 0 to determine
-- which layer (cube face) the primitive should be sent to.
--
-- Note that the use of the passed through input @normal@ in this example
-- suggests that the fragment shader in this example would perform an
-- operation like per-fragment lighting. The viewport swizzle would
-- transform the position to be face-relative, but @normal@ would remain in
-- the original coordinate system. It seems likely that the fragment shader
-- in either version of the example would want to perform lighting in the
-- original coordinate system. It would likely do this by reconstructing
-- the position of the fragment in the original coordinate system using
-- @gl_FragCoord@, a constant or uniform holding the size of the cube face,
-- and the input @gl_ViewportIndex@ (or @gl_Layer@), which identifies the
-- cube face. Since the value of @normal@ is in the original coordinate
-- system, it would not need to be modified as part of this coordinate
-- transformation.
--
-- Note that while the @rotate@() operation in the regular geometry shader
-- above could include an arbitrary post-rotation projection matrix, the
-- viewport swizzle does not support arbitrary math. To get proper
-- projection, 1\/W buffering should be used. To do this:
--
-- 1.  Program the viewport swizzles to move the pre-projection W eye
--     coordinate (typically 1.0) into the Z coordinate of the swizzle
--     output and the eye coordinate component used for depth into the W
--     coordinate. For example, the viewport corresponding to the +Z face
--     might use a swizzle of (+X, -Y, +W, +Z). The Z normalized device
--     coordinate computed after swizzling would then be z\'\/w\' =
--     1\/Zeye.
--
-- 2.  On NVIDIA implementations supporting floating-point depth buffers
--     with values outside [0,1], prevent unwanted near plane clipping by
--     enabling @depthClampEnable@. Ensure that the depth clamp doesn’t
--     mess up depth testing by programming the depth range to very large
--     values, such as @minDepthBounds@=-z, @maxDepthBounds@=+z, where z =
--     2127. It should be possible to use IEEE infinity encodings also
--     (@0xFF800000@ for @-INF@, @0x7F800000@ for @+INF@). Even when
--     near\/far clipping is disabled, primitives extending behind the eye
--     will still be clipped because one or more vertices will have a
--     negative W coordinate and fail X\/Y clipping tests.
--
--     On other implementations, scale X, Y, and Z eye coordinates so that
--     vertices on the near plane have a post-swizzle W coordinate of 1.0.
--     For example, if the near plane is at Zeye = 1\/256, scale X, Y, and
--     Z by 256.
--
-- 3.  Adjust depth testing to reflect the fact that 1\/W values are large
--     near the eye and small away from the eye. Clear the depth buffer to
--     zero (infinitely far away) and use a depth test of
--     'Vulkan.Core10.Enums.CompareOp.COMPARE_OP_GREATER' instead of
--     'Vulkan.Core10.Enums.CompareOp.COMPARE_OP_LESS'.
--
-- == Version History
--
-- -   Revision 1, 2016-12-22 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PipelineViewportSwizzleStateCreateFlagsNV',
-- 'PipelineViewportSwizzleStateCreateInfoNV',
-- 'ViewportCoordinateSwizzleNV', 'ViewportSwizzleNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_viewport_swizzle Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_viewport_swizzle  ( ViewportSwizzleNV(..)
                                                 , PipelineViewportSwizzleStateCreateInfoNV(..)
                                                 , PipelineViewportSwizzleStateCreateFlagsNV(..)
                                                 , ViewportCoordinateSwizzleNV( VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV
                                                                              , ..
                                                                              )
                                                 , NV_VIEWPORT_SWIZZLE_SPEC_VERSION
                                                 , pattern NV_VIEWPORT_SWIZZLE_SPEC_VERSION
                                                 , NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
                                                 , pattern NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
                                                 ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV))
-- | VkViewportSwizzleNV - Structure specifying a viewport swizzle
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PipelineViewportSwizzleStateCreateInfoNV',
-- 'ViewportCoordinateSwizzleNV'
data ViewportSwizzleNV = ViewportSwizzleNV
  { -- | @x@ is a 'ViewportCoordinateSwizzleNV' value specifying the swizzle
    -- operation to apply to the x component of the primitive
    --
    -- #VUID-VkViewportSwizzleNV-x-parameter# @x@ /must/ be a valid
    -- 'ViewportCoordinateSwizzleNV' value
    x :: ViewportCoordinateSwizzleNV
  , -- | @y@ is a 'ViewportCoordinateSwizzleNV' value specifying the swizzle
    -- operation to apply to the y component of the primitive
    --
    -- #VUID-VkViewportSwizzleNV-y-parameter# @y@ /must/ be a valid
    -- 'ViewportCoordinateSwizzleNV' value
    y :: ViewportCoordinateSwizzleNV
  , -- | @z@ is a 'ViewportCoordinateSwizzleNV' value specifying the swizzle
    -- operation to apply to the z component of the primitive
    --
    -- #VUID-VkViewportSwizzleNV-z-parameter# @z@ /must/ be a valid
    -- 'ViewportCoordinateSwizzleNV' value
    z :: ViewportCoordinateSwizzleNV
  , -- | @w@ is a 'ViewportCoordinateSwizzleNV' value specifying the swizzle
    -- operation to apply to the w component of the primitive
    --
    -- #VUID-VkViewportSwizzleNV-w-parameter# @w@ /must/ be a valid
    -- 'ViewportCoordinateSwizzleNV' value
    w :: ViewportCoordinateSwizzleNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ViewportSwizzleNV)
#endif
deriving instance Show ViewportSwizzleNV

instance ToCStruct ViewportSwizzleNV where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ViewportSwizzleNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ViewportCoordinateSwizzleNV)) (x)
    poke ((p `plusPtr` 4 :: Ptr ViewportCoordinateSwizzleNV)) (y)
    poke ((p `plusPtr` 8 :: Ptr ViewportCoordinateSwizzleNV)) (z)
    poke ((p `plusPtr` 12 :: Ptr ViewportCoordinateSwizzleNV)) (w)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ViewportCoordinateSwizzleNV)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ViewportCoordinateSwizzleNV)) (zero)
    poke ((p `plusPtr` 8 :: Ptr ViewportCoordinateSwizzleNV)) (zero)
    poke ((p `plusPtr` 12 :: Ptr ViewportCoordinateSwizzleNV)) (zero)
    f

instance FromCStruct ViewportSwizzleNV where
  peekCStruct p = do
    x <- peek @ViewportCoordinateSwizzleNV ((p `plusPtr` 0 :: Ptr ViewportCoordinateSwizzleNV))
    y <- peek @ViewportCoordinateSwizzleNV ((p `plusPtr` 4 :: Ptr ViewportCoordinateSwizzleNV))
    z <- peek @ViewportCoordinateSwizzleNV ((p `plusPtr` 8 :: Ptr ViewportCoordinateSwizzleNV))
    w <- peek @ViewportCoordinateSwizzleNV ((p `plusPtr` 12 :: Ptr ViewportCoordinateSwizzleNV))
    pure $ ViewportSwizzleNV
             x y z w

instance Storable ViewportSwizzleNV where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ViewportSwizzleNV where
  zero = ViewportSwizzleNV
           zero
           zero
           zero
           zero


-- | VkPipelineViewportSwizzleStateCreateInfoNV - Structure specifying
-- swizzle applied to primitive clip coordinates
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PipelineViewportSwizzleStateCreateFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'ViewportSwizzleNV'
data PipelineViewportSwizzleStateCreateInfoNV = PipelineViewportSwizzleStateCreateInfoNV
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkPipelineViewportSwizzleStateCreateInfoNV-flags-zerobitmask#
    -- @flags@ /must/ be @0@
    flags :: PipelineViewportSwizzleStateCreateFlagsNV
  , -- | @pViewportSwizzles@ is a pointer to an array of 'ViewportSwizzleNV'
    -- structures, defining the viewport swizzles.
    --
    -- #VUID-VkPipelineViewportSwizzleStateCreateInfoNV-pViewportSwizzles-parameter#
    -- @pViewportSwizzles@ /must/ be a valid pointer to an array of
    -- @viewportCount@ valid 'ViewportSwizzleNV' structures
    viewportSwizzles :: Vector ViewportSwizzleNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineViewportSwizzleStateCreateInfoNV)
#endif
deriving instance Show PipelineViewportSwizzleStateCreateInfoNV

instance ToCStruct PipelineViewportSwizzleStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportSwizzleStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineViewportSwizzleStateCreateFlagsNV)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (viewportSwizzles)) :: Word32))
    pPViewportSwizzles' <- ContT $ allocaBytesAligned @ViewportSwizzleNV ((Data.Vector.length (viewportSwizzles)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewportSwizzles' `plusPtr` (16 * (i)) :: Ptr ViewportSwizzleNV) (e)) (viewportSwizzles)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ViewportSwizzleNV))) (pPViewportSwizzles')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineViewportSwizzleStateCreateInfoNV where
  peekCStruct p = do
    flags <- peek @PipelineViewportSwizzleStateCreateFlagsNV ((p `plusPtr` 16 :: Ptr PipelineViewportSwizzleStateCreateFlagsNV))
    viewportCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pViewportSwizzles <- peek @(Ptr ViewportSwizzleNV) ((p `plusPtr` 24 :: Ptr (Ptr ViewportSwizzleNV)))
    pViewportSwizzles' <- generateM (fromIntegral viewportCount) (\i -> peekCStruct @ViewportSwizzleNV ((pViewportSwizzles `advancePtrBytes` (16 * (i)) :: Ptr ViewportSwizzleNV)))
    pure $ PipelineViewportSwizzleStateCreateInfoNV
             flags pViewportSwizzles'

instance Zero PipelineViewportSwizzleStateCreateInfoNV where
  zero = PipelineViewportSwizzleStateCreateInfoNV
           zero
           mempty


-- | VkPipelineViewportSwizzleStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- 'PipelineViewportSwizzleStateCreateFlagsNV' is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'PipelineViewportSwizzleStateCreateInfoNV'
newtype PipelineViewportSwizzleStateCreateFlagsNV = PipelineViewportSwizzleStateCreateFlagsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineViewportSwizzleStateCreateFlagsNV :: String
conNamePipelineViewportSwizzleStateCreateFlagsNV = "PipelineViewportSwizzleStateCreateFlagsNV"

enumPrefixPipelineViewportSwizzleStateCreateFlagsNV :: String
enumPrefixPipelineViewportSwizzleStateCreateFlagsNV = ""

showTablePipelineViewportSwizzleStateCreateFlagsNV :: [(PipelineViewportSwizzleStateCreateFlagsNV, String)]
showTablePipelineViewportSwizzleStateCreateFlagsNV = []

instance Show PipelineViewportSwizzleStateCreateFlagsNV where
  showsPrec = enumShowsPrec enumPrefixPipelineViewportSwizzleStateCreateFlagsNV
                            showTablePipelineViewportSwizzleStateCreateFlagsNV
                            conNamePipelineViewportSwizzleStateCreateFlagsNV
                            (\(PipelineViewportSwizzleStateCreateFlagsNV x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineViewportSwizzleStateCreateFlagsNV where
  readPrec = enumReadPrec enumPrefixPipelineViewportSwizzleStateCreateFlagsNV
                          showTablePipelineViewportSwizzleStateCreateFlagsNV
                          conNamePipelineViewportSwizzleStateCreateFlagsNV
                          PipelineViewportSwizzleStateCreateFlagsNV


-- | VkViewportCoordinateSwizzleNV - Specify how a viewport coordinate is
-- swizzled
--
-- = Description
--
-- These values are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-viewport-swizzle Viewport Swizzle>.
--
-- = See Also
--
-- 'ViewportSwizzleNV'
newtype ViewportCoordinateSwizzleNV = ViewportCoordinateSwizzleNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV = ViewportCoordinateSwizzleNV 0
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV = ViewportCoordinateSwizzleNV 1
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV = ViewportCoordinateSwizzleNV 2
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV = ViewportCoordinateSwizzleNV 3
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV = ViewportCoordinateSwizzleNV 4
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV = ViewportCoordinateSwizzleNV 5
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV = ViewportCoordinateSwizzleNV 6
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV = ViewportCoordinateSwizzleNV 7
{-# complete VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV,
             VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV,
             VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV,
             VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV,
             VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV,
             VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV,
             VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV,
             VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV :: ViewportCoordinateSwizzleNV #-}

conNameViewportCoordinateSwizzleNV :: String
conNameViewportCoordinateSwizzleNV = "ViewportCoordinateSwizzleNV"

enumPrefixViewportCoordinateSwizzleNV :: String
enumPrefixViewportCoordinateSwizzleNV = "VIEWPORT_COORDINATE_SWIZZLE_"

showTableViewportCoordinateSwizzleNV :: [(ViewportCoordinateSwizzleNV, String)]
showTableViewportCoordinateSwizzleNV =
  [ (VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV, "POSITIVE_X_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV, "NEGATIVE_X_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV, "POSITIVE_Y_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV, "NEGATIVE_Y_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV, "POSITIVE_Z_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV, "NEGATIVE_Z_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV, "POSITIVE_W_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV, "NEGATIVE_W_NV")
  ]

instance Show ViewportCoordinateSwizzleNV where
  showsPrec = enumShowsPrec enumPrefixViewportCoordinateSwizzleNV
                            showTableViewportCoordinateSwizzleNV
                            conNameViewportCoordinateSwizzleNV
                            (\(ViewportCoordinateSwizzleNV x) -> x)
                            (showsPrec 11)

instance Read ViewportCoordinateSwizzleNV where
  readPrec = enumReadPrec enumPrefixViewportCoordinateSwizzleNV
                          showTableViewportCoordinateSwizzleNV
                          conNameViewportCoordinateSwizzleNV
                          ViewportCoordinateSwizzleNV


type NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION"
pattern NV_VIEWPORT_SWIZZLE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1


type NV_VIEWPORT_SWIZZLE_EXTENSION_NAME = "VK_NV_viewport_swizzle"

-- No documentation found for TopLevel "VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME"
pattern NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_VIEWPORT_SWIZZLE_EXTENSION_NAME = "VK_NV_viewport_swizzle"

