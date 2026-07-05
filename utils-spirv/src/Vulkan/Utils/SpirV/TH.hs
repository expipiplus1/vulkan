{-| Template Haskell entry points: reflect a compiled @.spv@ at build time and
splice Haskell record types for its uniform / storage / push-constant blocks.

Types are keyed by their SPIR-V struct name and generated at most once; a name
that already resolves in scope (e.g. generated from another shader that shares
an include) is referenced rather than redefined, so shared GLSL types map to a
single shared Haskell type.
-}
module Vulkan.Utils.SpirV.TH
  ( reflectShaderTypes
  , reflectShaderTypesWith
  , reflectShaderTypesBytes
  , reflectShaderTypesBytesWith
  , reflectModuleTypes
  , reflectModuleTypesWith
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Language.Haskell.TH

import Data.SpirV.Reflect.BlockVariable qualified
import Data.SpirV.Reflect.DescriptorBinding qualified
import Data.SpirV.Reflect.Enums.DescriptorType qualified as R
import Data.SpirV.Reflect.Module (Module)
import Data.SpirV.Reflect.Module qualified
import Data.SpirV.Reflect.TypeDescription (TypeDescription)

import Vulkan.Utils.SpirV.Block (allMembers16, collectStructs, structRecordDec, structTypeName)
import Vulkan.Utils.SpirV.Reflect (reflectBytes, reflectFileQ)
import Vulkan.Utils.SpirV.Types (LayoutMode (..), TypeMap, geomancyTypeMap)

{- | Reflect a @.spv@ file and generate record types for its blocks, using the
default 'geomancyTypeMap'.
-}
reflectShaderTypes :: FilePath -> Q [Dec]
reflectShaderTypes = reflectShaderTypesWith geomancyTypeMap

-- | As 'reflectShaderTypes', with a caller-supplied 'TypeMap'.
reflectShaderTypesWith :: TypeMap -> FilePath -> Q [Dec]
reflectShaderTypesWith tymap path = reflectFileQ path >>= reflectModuleTypesWith tymap

{- | As 'reflectShaderTypes', but reflecting SPIR-V bytecode already in hand
rather than reading a @.spv@ file — e.g. the 'ByteString' a @[comp|…|]@
quasiquoter produced in a shader module, referenced from the module that
assembles the pipeline:

@
reflectShaderTypesBytes Shaders.compSpirv
@

No file is involved. Because the bytes are an ordinary imported binding, GHC's
cross-module recompilation reruns the splice when they change, so (unlike the
file path) there is no 'Language.Haskell.TH.Syntax.addDependentFile' to forget.
The usual Template Haskell stage restriction applies: the bytes must come from a
different module than the splice.
-}
reflectShaderTypesBytes :: ByteString -> Q [Dec]
reflectShaderTypesBytes = reflectShaderTypesBytesWith geomancyTypeMap

-- | As 'reflectShaderTypesBytes', with a caller-supplied 'TypeMap'.
reflectShaderTypesBytesWith :: TypeMap -> ByteString -> Q [Dec]
reflectShaderTypesBytesWith tymap bytes = runIO (reflectBytes bytes) >>= reflectModuleTypesWith tymap

{- | Generate record types for every block a reflected 'Module' declares.

The shared core of 'reflectShaderTypes' and 'reflectShaderTypesBytes', exposed so
a caller who already holds a 'Module' can reach codegen without re-reflecting —
and can reflect once, then splice from the same 'Module' in more than one place.
-}
reflectModuleTypes :: Module -> Q [Dec]
reflectModuleTypes = reflectModuleTypesWith geomancyTypeMap

-- | As 'reflectModuleTypes', with a caller-supplied 'TypeMap'.
reflectModuleTypesWith :: TypeMap -> Module -> Q [Dec]
reflectModuleTypesWith tymap = genStructs tymap . moduleStructCandidates

{- | The @OpTypeStruct@ descriptions worth turning into records, each paired with
the layout its 'Storable' should follow. Each block is expanded through
'collectStructs' so nested and array-element structs (e.g. an SSBO's @Sphere[]@
element) are generated too.
-}
moduleStructCandidates :: Module -> [(LayoutMode, TypeDescription)]
moduleStructCandidates m =
  concatMap (uncurry collectStructs) $
    [ (layoutForDescriptor b.descriptor_type, td)
    | b <- V.toList m.descriptor_bindings
    , Just td <- [b.type_description]
    ]
      ++ [ (Std430Layout, td)
         | pc <- V.toList m.push_constants
         , Just td <- [pc.type_description]
         ]

layoutForDescriptor :: R.DescriptorType -> LayoutMode
layoutForDescriptor = \case
  R.DESCRIPTOR_TYPE_UNIFORM_BUFFER -> Std140Layout
  _ -> Std430Layout

genStructs :: TypeMap -> [(LayoutMode, TypeDescription)] -> Q [Dec]
genStructs tymap = go Map.empty
  where
    go _ [] = pure []
    go seen ((layout, td) : rest) =
      case structTypeName td of
        Nothing -> go seen rest
        Just nm ->
          case Map.lookup nm seen of
            -- Already requested in this splice.
            Just prev
              | prev == layout -> go seen rest -- same layout; reference it
              | allMembers16 td -> go seen rest -- std140 == std430; safe to share
              | otherwise -> fail (sharingError nm prev layout)
            Nothing -> do
              existing <- lookupTypeName nm
              case existing of
                Just _ -> go (Map.insert nm layout seen) rest -- defined elsewhere; reference it
                Nothing -> do
                  mdec <- structRecordDec tymap layout td
                  decs <- go (Map.insert nm layout seen) rest
                  pure $ maybe decs (++ decs) mdec

{- | A struct used under two layouts that aren't byte-compatible (see
'allMembers16') can't be represented by a single Haskell record.
-}
sharingError :: String -> LayoutMode -> LayoutMode -> String
sharingError nm a b =
  "vulkan-utils-spirv: struct '"
    <> nm
    <> "' is used under both "
    <> showLayout a
    <> " and "
    <> showLayout b
    <> " layouts but is not layout-compatible: not every member is 16-byte "
    <> "aligned. Make each member 16-byte aligned (e.g. use vec4/mat4) so the "
    <> "layouts coincide, or keep the struct to a single layout."
  where
    showLayout Std140Layout = "std140"
    showLayout Std430Layout = "std430"
