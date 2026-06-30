{-| Build Vulkan vertex-input descriptions from a reflected vertex shader's input
variables. Built-in inputs (e.g. @gl_VertexIndex@) are skipped; the remaining
inputs are packed tightly into a single binding (binding 0), ordered by
@location@, with per-attribute formats taken directly from reflection (SPIR-V
@Format@ shares Vulkan's numeric values).
-}
module Vulkan.Utils.SpirV.VertexInput
  ( vertexInputState
  , vertexInputAttributes
  , vertexInputBinding
  ) where

import Data.List (sortOn)
import Data.Vector qualified as V
import Data.Word (Word32)
import Vulkan.Core10 qualified as Vk
import Vulkan.Zero (zero)

import Data.SpirV.Enum (BuiltIn (..))
import Data.SpirV.Reflect.Enums.Format qualified as R
import Data.SpirV.Reflect.InterfaceVariable qualified as InterfaceVariable
import Data.SpirV.Reflect.Module (Module)
import Data.SpirV.Reflect.Module qualified
import Data.SpirV.Reflect.Traits qualified as Traits

{- | The vertex-input state reflected from a vertex module: the single packed
binding plus its attributes ('vertexInputBinding' + 'vertexInputAttributes'). A
module with no vertex inputs — e.g. one pulling geometry from an SSBO via
@gl_VertexIndex@ — yields 'zero' (no bindings, no attributes).
-}
vertexInputState :: Module -> Vk.PipelineVertexInputStateCreateInfo '[]
vertexInputState m =
  case vertexInputAttributes m of
    [] -> zero
    attrs ->
      zero
        { Vk.vertexBindingDescriptions = V.fromList [vertexInputBinding m]
        , Vk.vertexAttributeDescriptions = V.fromList attrs
        }

-- | One attribute per non-built-in input variable, tightly packed into binding 0.
vertexInputAttributes :: Module -> [Vk.VertexInputAttributeDescription]
vertexInputAttributes m = go 0 (inputs m)
  where
    go _ [] = []
    go off ((loc, fmt, sz) : rest) =
      zero
        { Vk.location = loc
        , Vk.binding = 0
        , Vk.format = fmt
        , Vk.offset = off
        }
        : go (off + sz) rest

{- | The single binding (binding 0, per-vertex) covering all inputs, with stride
equal to their packed size.
-}
vertexInputBinding :: Module -> Vk.VertexInputBindingDescription
vertexInputBinding m =
  zero
    { Vk.binding = 0
    , Vk.stride = sum [sz | (_, _, sz) <- inputs m]
    , Vk.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
    }

-- | @(location, format, byte size)@ for each non-built-in input, ordered by location.
inputs :: Module -> [(Word32, Vk.Format, Word32)]
inputs m =
  sortOn
    (\(loc, _, _) -> loc)
    -- a non-built-in reads as Nothing (ffi) or the @BuiltIn (-1)@ sentinel (yaml)
    [ (v.location, ivFormat v, ivSize v)
    | v <- V.toList m.input_variables
    , v.built_in `elem` [Nothing, Just (BuiltIn (-1))]
    ]

-- Vulkan-side field extractions.

ivFormat :: InterfaceVariable.InterfaceVariable -> Vk.Format
ivFormat InterfaceVariable.InterfaceVariable{InterfaceVariable.format = R.Format n} =
  Vk.Format (fromIntegral n)

ivSize :: InterfaceVariable.InterfaceVariable -> Word32
ivSize InterfaceVariable.InterfaceVariable{InterfaceVariable.numeric = num} =
  components * (width `div` 8)
  where
    Traits.Numeric
      { Traits.scalar = Traits.Scalar{Traits.width = width}
      , Traits.vector = Traits.Vector{Traits.component_count = vecN}
      } = num
    components = max 1 vecN
