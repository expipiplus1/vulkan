{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Vulkan.Utils.Reflect
  ( ShaderStage(..)
  , ShaderInfo(..)
  , reflection
  , reflection'
  , makeShaderInfo
  , makeDescriptorInfo
  , makeInputInfo
  ) where

import           Vulkan.CStruct.Extends         ( SomeStruct(..) )
import           Vulkan.Core10.DescriptorSet    ( DescriptorSetLayoutBinding(..)
                                                , DescriptorSetLayoutCreateInfo(..)
                                                )
import           Vulkan.Core10.Enums.DescriptorType
                                                ( DescriptorType(..) )
import           Vulkan.Core10.Enums.Format     ( Format(..) )
import           Vulkan.Core10.Enums.VertexInputRate
                                                ( VertexInputRate(..) )
import           Vulkan.Core10.Pipeline         ( PipelineShaderStageCreateInfo(..)
                                                , PipelineVertexInputStateCreateInfo(..)
                                                , ShaderStageFlagBits(..)
                                                , VertexInputAttributeDescription(..)
                                                , VertexInputBindingDescription(..)
                                                )
import           Vulkan.Core10.Shader           ( ShaderModule(..)
                                                , ShaderModuleCreateInfo(..)
                                                )
import           Vulkan.NamedType               ( (:::) )
import           Vulkan.Zero                    ( zero )

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Trans.Except     ( ExceptT
                                                , except
                                                , runExceptT
                                                , throwE
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (.=)
                                                , FromJSON(..)
                                                , ToJSON(..)
                                                , Value(String)
                                                , eitherDecodeStrict'
                                                , object
                                                , withObject
                                                , withText
                                                )
import           Data.Aeson.Types               ( Parser )
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           GHC.Generics                   ( Generic )
import           GHC.IO.Exception               ( ExitCode(..) )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           System.IO.Temp                 ( withSystemTempDirectory )
import           System.Process.Typed           ( proc
                                                , readProcess
                                                )

import           Control.Monad                  ( join )
import           Data.Function                  ( on )
import           Data.List                      ( groupBy
                                                , mapAccumL
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.Word                      ( Word32 )

data EntryPoint = EntryPoint
  { name :: Text
  , mode :: ShaderStage
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Input = Input
  { type'    :: VertexAttributeType
  , name     :: Text
  , location :: Int
  , array    :: Maybe (Vector Int)
  }
  deriving (Show, Generic)

instance FromJSON Input where
  parseJSON = withObject "inputs" $ \v ->
    Input <$> v .: "type" <*> v .: "name" <*> v .: "location" <*> v .:? "array"

instance ToJSON Input where
  toJSON (Input type' name location array) = object
    ["type" .= type', "name" .= name, "location" .= location, "array" .= array]

data Ubo = Ubo
  { name    :: Text
  , set     :: Int
  , binding :: Int
  , array   :: Maybe (Vector Int)
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Texture = Texture
  { type'   :: TextureDescriptorType
  , name    :: Text
  , set     :: Int
  , binding :: Int
  , array   :: Maybe (Vector Int)
  }
  deriving (Show, Generic)

instance FromJSON Texture where
  parseJSON = withObject "textures" $ \v ->
    Texture
      <$> v
      .:  "type"
      <*> v
      .:  "name"
      <*> v
      .:  "set"
      <*> v
      .:  "binding"
      <*> v
      .:? "array"

instance ToJSON Texture where
  toJSON (Texture type' name set binding array) = object
    [ "type" .= type'
    , "name" .= name
    , "set" .= set
    , "binding" .= binding
    , "array" .= array
    ]

data SeparateImage = SeparateImage
  { type'   :: SeparateImageDescriptorType
  , name    :: Text
  , set     :: Int
  , binding :: Int
  , array   :: Maybe (Vector Int)
  }
  deriving (Show, Generic)

instance FromJSON SeparateImage where
  parseJSON = withObject "separate_images" $ \v ->
    SeparateImage
      <$> v
      .:  "type"
      <*> v
      .:  "name"
      <*> v
      .:  "set"
      <*> v
      .:  "binding"
      <*> v
      .:? "array"

instance ToJSON SeparateImage where
  toJSON (SeparateImage type' name set binding array) = object
    [ "type" .= type'
    , "name" .= name
    , "set" .= set
    , "binding" .= binding
    , "array" .= array
    ]

data Reflection = Reflection
  { entryPoints     :: Vector EntryPoint
  , inputs          :: Maybe (Vector Input)
  , textures        :: Maybe (Vector Texture)
  , ubos            :: Maybe (Vector Ubo)
  , separate_images :: Maybe (Vector SeparateImage)
  }
  deriving (Show, Generic, FromJSON, ToJSON)

withTextMaybe :: String -> (Text -> Maybe a) -> Value -> Parser a
withTextMaybe label fromText = withText label $ \text -> case fromText text of
  Nothing    -> fail $ "Unexpected " <> label <> " value: " <> show text
  Just known -> pure known

data ShaderStage
  = Vertex
  | Fragment
  | Compute
  | TessellationControl
  | TessellationEvaluation
  | Geometry
  | Raygen
  | Intersection
  | AnyHit
  | ClosestHit
  | Miss
  | Callable
  | Task
  | Mesh
  deriving (Eq, Show)

shaderStageFromText :: Text -> Maybe ShaderStage
shaderStageFromText = \case
  "vert"  -> Just Vertex
  "frag"  -> Just Fragment
  "comp"  -> Just Compute
  "tesc"  -> Just TessellationControl
  "tese"  -> Just TessellationEvaluation
  "geom"  -> Just Geometry
  "rgen"  -> Just Raygen
  "rint"  -> Just Intersection
  "rahit" -> Just AnyHit
  "rchit" -> Just ClosestHit
  "rmiss" -> Just Miss
  "rcall" -> Just Callable
  "task"  -> Just Task
  "mesh"  -> Just Mesh
  _       -> Nothing

shaderStageToText :: ShaderStage -> Text
shaderStageToText = \case
  Vertex                 -> "vert"
  Fragment               -> "frag"
  Compute                -> "comp"
  TessellationControl    -> "tesc"
  TessellationEvaluation -> "tese"
  Geometry               -> "geom"
  Raygen                 -> "rgen"
  Intersection           -> "rint"
  AnyHit                 -> "rahit"
  ClosestHit             -> "rchit"
  Miss                   -> "rmiss"
  Callable               -> "rcall"
  Task                   -> "task"
  Mesh                   -> "mesh"

instance FromJSON ShaderStage where
  parseJSON = withTextMaybe "mode" shaderStageFromText

instance ToJSON ShaderStage where
  toJSON = String . shaderStageToText

shaderStageFlagBits :: ShaderStage -> ShaderStageFlagBits
shaderStageFlagBits = \case
  Vertex                 -> SHADER_STAGE_VERTEX_BIT
  Fragment               -> SHADER_STAGE_FRAGMENT_BIT
  Compute                -> SHADER_STAGE_COMPUTE_BIT
  TessellationControl    -> SHADER_STAGE_TESSELLATION_CONTROL_BIT
  TessellationEvaluation -> SHADER_STAGE_TESSELLATION_EVALUATION_BIT
  Geometry               -> SHADER_STAGE_GEOMETRY_BIT
  Raygen                 -> SHADER_STAGE_RAYGEN_BIT_KHR
  Intersection           -> SHADER_STAGE_INTERSECTION_BIT_KHR
  AnyHit                 -> SHADER_STAGE_ANY_HIT_BIT_KHR
  ClosestHit             -> SHADER_STAGE_CLOSEST_HIT_BIT_KHR
  Miss                   -> SHADER_STAGE_MISS_BIT_KHR
  Callable               -> SHADER_STAGE_CALLABLE_BIT_KHR
  Task                   -> SHADER_STAGE_TASK_BIT_NV
  Mesh                   -> SHADER_STAGE_MESH_BIT_NV

data Shader = Shader
  { stage :: ShaderStage
  , code  :: B.ByteString
  }
  deriving Show

data ShaderInfo = ShaderInfo
  { shaderModuleCreateInfo :: ShaderModuleCreateInfo '[]
  , pipelineShaderStageCreateInfos
      :: ShaderModule -> Vector (SomeStruct PipelineShaderStageCreateInfo)
  }

makeShaderInfo :: (Shader, Reflection) -> ShaderInfo
makeShaderInfo (Shader {..}, Reflection {..}) = ShaderInfo { .. }
 where
  shaderModuleCreateInfo = makeShaderModuleCreateInfo code
  pipelineShaderStageCreateInfos =
    (SomeStruct <$>) . makePipelineShaderStageCreateInfos stage entryPoints

makeDescriptorInfo
  :: Vector (Shader, Reflection) -> Vector (DescriptorSetLayoutCreateInfo '[])
makeDescriptorInfo xs = makeDescriptorSetLayoutCreateInfos $ do
  (Shader { stage }, Reflection { ubos, textures }) <- xs
  makeDescriptorSetLayoutBindings stage
                                  (fromMaybe [] ubos)
                                  (fromMaybe [] textures)

makeInputInfo
  :: Vector (Shader, Reflection)
  -> Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
makeInputInfo =
  (SomeStruct <$>)
    . makePipelineVertexInputStateCreateInfo
    . join
    . V.mapMaybe (inputs . snd)
    . V.filter ((== Vertex) . (stage :: Shader -> ShaderStage) . fst)

makeShaderModuleCreateInfo
  :: "code" ::: B.ByteString -> ShaderModuleCreateInfo '[]
makeShaderModuleCreateInfo code = zero { code = code }

makePipelineShaderStageCreateInfo
  :: ShaderStage
  -> ShaderModule
  -> EntryPoint
  -> PipelineShaderStageCreateInfo '[]
makePipelineShaderStageCreateInfo stage shaderModule EntryPoint {..} = zero
  { stage   = shaderStageFlagBits stage
  , module' = shaderModule
  , name    = B.pack . T.unpack $ name
  }

makePipelineShaderStageCreateInfos
  :: ShaderStage
  -> Vector EntryPoint
  -> ShaderModule
  -> Vector (PipelineShaderStageCreateInfo '[])
makePipelineShaderStageCreateInfos stage entryPoints shaderModule =
  V.map (makePipelineShaderStageCreateInfo stage shaderModule) entryPoints

-- https://github.com/KhronosGroup/GLSL/blob/master/extensions/khr/GL_KHR_vulkan_glsl.txt
-- https://vulkan.lunarg.com/doc/view/1.2.135.0/linux/tutorial/html/08-init_pipeline_layout.html
makeUboDescriptorSetLayoutBinding
  :: ShaderStage -> Ubo -> (Int, DescriptorSetLayoutBinding)
makeUboDescriptorSetLayoutBinding stage Ubo {..} =
  ( set
  , zero { binding         = fromIntegral binding
         , descriptorType  = DESCRIPTOR_TYPE_UNIFORM_BUFFER
         , descriptorCount = maybe 1 (fromIntegral . V.product) array
         , stageFlags      = shaderStageFlagBits stage
         }
  )

-- 3.6. Keywords https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.4.60.pdf
data TextureDescriptorType
  = Buffer
  | Sampler1D
  | Sampler1DShadow
  | Sampler1DArray
  | Sampler1DArrayShadow
  | Isampler1D
  | Isampler1DArray
  | Usampler1D
  | Usampler1DArray
  | Sampler2D
  | Sampler2DShadow
  | Sampler2DArray
  | Sampler2DArrayShadow
  | Isampler2D
  | Isampler2DArray
  | Usampler2D
  | Usampler2DArray
  | Sampler2DRect
  | Sampler2DRectShadow
  | Isampler2DRect
  | Usampler2DRect
  | Sampler2DMS
  | Isampler2DMS
  | Usampler2DMS
  | Sampler2DMSArray
  | Isampler2DMSArray
  | Usampler2DMSArray
  | Sampler3D
  | Isampler3D
  | Usampler3D
  | SamplerCube
  | SamplerCubeShadow
  | IsamplerCube
  | UsamplerCube
  | SamplerCubeArray
  | SamplerCubeArrayShadow
  | IsamplerCubeArray
  | UsamplerCubeArray
  | SamplerBuffer
  | IsamplerBuffer
  | UsamplerBuffer
  | Image1D
  | Iimage1D
  | Uimage1D
  | Image1DArray
  | Iimage1DArray
  | Uimage1DArray
  | Image2D
  | Iimage2D
  | Uimage2D
  | Image2DArray
  | Iimage2DArray
  | Uimage2DArray
  | Image2DRect
  | Iimage2DRect
  | Uimage2DRect
  | Image2DMS
  | Iimage2DMS
  | Uimage2DMS
  | Image2DMSArray
  | Iimage2DMSArray
  | Uimage2DMSArray
  | Image3D
  | Iimage3D
  | Uimage3D
  | ImageCube
  | IimageCube
  | UimageCube
  | ImageCubeArray
  | IimageCubeArray
  | UimageCubeArray
  | ImageBuffer
  | IimageBuffer
  | UimageBuffer
  | Texture1D
  | Texture1DArray
  | Itexture1D
  | Itexture1DArray
  | Utexture1D
  | Utexture1DArray
  | Texture2D
  | Texture2DArray
  | Itexture2D
  | Itexture2DArray
  | Utexture2D
  | Utexture2DArray
  | Texture2DRect
  | Itexture2DRect
  | Utexture2DRect
  | Texture2DMS
  | Itexture2DMS
  | Utexture2DMS
  | Texture2DMSArray
  | Itexture2DMSArray
  | Utexture2DMSArray
  | Texture3D
  | Itexture3D
  | Utexture3D
  | TextureCube
  | ItextureCube
  | UtextureCube
  | TextureCubeArray
  | ItextureCubeArray
  | UtextureCubeArray
  | Sampler
  | SamplerShadow
  | SubpassInput
  | IsubpassInput
  | UsubpassInput
  | SubpassInputMS
  | IsubpassInputMS
  | UsubpassInputMS
  deriving Show

textureDescriptorTypeFromText :: Text -> Maybe TextureDescriptorType
textureDescriptorTypeFromText = \case
  "buffer"                 -> Just Buffer
  "sampler1D"              -> Just Sampler1D
  "sampler1DShadow"        -> Just Sampler1DShadow
  "sampler1DArray"         -> Just Sampler1DArray
  "sampler1DArrayShadow"   -> Just Sampler1DArrayShadow
  "isampler1D"             -> Just Isampler1D
  "isampler1DArray"        -> Just Isampler1DArray
  "usampler1D"             -> Just Usampler1D
  "usampler1DArray"        -> Just Usampler1DArray
  "sampler2D"              -> Just Sampler2D
  "sampler2DShadow"        -> Just Sampler2DShadow
  "sampler2DArray"         -> Just Sampler2DArray
  "sampler2DArrayShadow"   -> Just Sampler2DArrayShadow
  "isampler2D"             -> Just Isampler2D
  "isampler2DArray"        -> Just Isampler2DArray
  "usampler2D"             -> Just Usampler2D
  "usampler2DArray"        -> Just Usampler2DArray
  "sampler2DRect"          -> Just Sampler2DRect
  "sampler2DRectShadow"    -> Just Sampler2DRectShadow
  "isampler2DRect"         -> Just Isampler2DRect
  "usampler2DRect"         -> Just Usampler2DRect
  "sampler2DMS"            -> Just Sampler2DMS
  "isampler2DMS"           -> Just Isampler2DMS
  "usampler2DMS"           -> Just Usampler2DMS
  "sampler2DMSArray"       -> Just Sampler2DMSArray
  "isampler2DMSArray"      -> Just Isampler2DMSArray
  "usampler2DMSArray"      -> Just Usampler2DMSArray
  "sampler3D"              -> Just Sampler3D
  "isampler3D"             -> Just Isampler3D
  "usampler3D"             -> Just Usampler3D
  "samplerCube"            -> Just SamplerCube
  "samplerCubeShadow"      -> Just SamplerCubeShadow
  "isamplerCube"           -> Just IsamplerCube
  "usamplerCube"           -> Just UsamplerCube
  "samplerCubeArray"       -> Just SamplerCubeArray
  "samplerCubeArrayShadow" -> Just SamplerCubeArrayShadow
  "isamplerCubeArray"      -> Just IsamplerCubeArray
  "usamplerCubeArray"      -> Just UsamplerCubeArray
  "samplerBuffer"          -> Just SamplerBuffer
  "isamplerBuffer"         -> Just IsamplerBuffer
  "usamplerBuffer"         -> Just UsamplerBuffer
  "image1D"                -> Just Image1D
  "iimage1D"               -> Just Iimage1D
  "uimage1D"               -> Just Uimage1D
  "image1DArray"           -> Just Image1DArray
  "iimage1DArray"          -> Just Iimage1DArray
  "uimage1DArray"          -> Just Uimage1DArray
  "image2D"                -> Just Image2D
  "iimage2D"               -> Just Iimage2D
  "uimage2D"               -> Just Uimage2D
  "image2DArray"           -> Just Image2DArray
  "iimage2DArray"          -> Just Iimage2DArray
  "uimage2DArray"          -> Just Uimage2DArray
  "image2DRect"            -> Just Image2DRect
  "iimage2DRect"           -> Just Iimage2DRect
  "uimage2DRect"           -> Just Uimage2DRect
  "image2DMS"              -> Just Image2DMS
  "iimage2DMS"             -> Just Iimage2DMS
  "uimage2DMS"             -> Just Uimage2DMS
  "image2DMSArray"         -> Just Image2DMSArray
  "iimage2DMSArray"        -> Just Iimage2DMSArray
  "uimage2DMSArray"        -> Just Uimage2DMSArray
  "image3D"                -> Just Image3D
  "Iimage3D"               -> Just Iimage3D
  "Uimage3D"               -> Just Uimage3D
  "imageCube"              -> Just ImageCube
  "iimageCube"             -> Just IimageCube
  "uimageCube"             -> Just UimageCube
  "imageCubeArray"         -> Just ImageCubeArray
  "iimageCubeArray"        -> Just IimageCubeArray
  "uimageCubeArray"        -> Just UimageCubeArray
  "imageBuffer"            -> Just ImageBuffer
  "iimageBuffer"           -> Just IimageBuffer
  "uimageBuffer"           -> Just UimageBuffer
  "texture1D"              -> Just Texture1D
  "texture1DArray"         -> Just Texture1DArray
  "itexture1D"             -> Just Itexture1D
  "itexture1DArray"        -> Just Itexture1DArray
  "utexture1D"             -> Just Utexture1D
  "utexture1DArray"        -> Just Utexture1DArray
  "texture2D"              -> Just Texture2D
  "texture2DArray"         -> Just Texture2DArray
  "itexture2D"             -> Just Itexture2D
  "itexture2DArray"        -> Just Itexture2DArray
  "utexture2D"             -> Just Utexture2D
  "utexture2DArray"        -> Just Utexture2DArray
  "texture2DRect"          -> Just Texture2DRect
  "itexture2DRect"         -> Just Itexture2DRect
  "utexture2DRect"         -> Just Utexture2DRect
  "texture2DMS"            -> Just Texture2DMS
  "itexture2DMS"           -> Just Itexture2DMS
  "utexture2DMS"           -> Just Utexture2DMS
  "texture2DMSArray"       -> Just Texture2DMSArray
  "itexture2DMSArray"      -> Just Itexture2DMSArray
  "utexture2DMSArray"      -> Just Utexture2DMSArray
  "texture3D"              -> Just Texture3D
  "itexture3D"             -> Just Itexture3D
  "utexture3D"             -> Just Utexture3D
  "textureCube"            -> Just TextureCube
  "itextureCube"           -> Just ItextureCube
  "utextureCube"           -> Just UtextureCube
  "textureCubeArray"       -> Just TextureCubeArray
  "itextureCubeArray"      -> Just ItextureCubeArray
  "utextureCubeArray"      -> Just UtextureCubeArray
  "sampler"                -> Just Sampler
  "samplerShadow"          -> Just SamplerShadow
  "subpassInput"           -> Just SubpassInput
  "isubpassInput"          -> Just IsubpassInput
  "usubpassInput"          -> Just UsubpassInput
  "subpassInputMS"         -> Just SubpassInputMS
  "IsubpassInputMS"        -> Just IsubpassInputMS
  "UsubpassInputMS"        -> Just UsubpassInputMS
  _                        -> Nothing

textureDescriptorTypeToText :: TextureDescriptorType -> Text
textureDescriptorTypeToText = \case
  Buffer                 -> "buffer"
  Sampler1D              -> "sampler1D"
  Sampler1DShadow        -> "sampler1DShadow"
  Sampler1DArray         -> "sampler1DArray"
  Sampler1DArrayShadow   -> "sampler1DArrayShadow"
  Isampler1D             -> "isampler1D"
  Isampler1DArray        -> "isampler1DArray"
  Usampler1D             -> "usampler1D"
  Usampler1DArray        -> "usampler1DArray"
  Sampler2D              -> "sampler2D"
  Sampler2DShadow        -> "sampler2DShadow"
  Sampler2DArray         -> "sampler2DArray"
  Sampler2DArrayShadow   -> "sampler2DArrayShadow"
  Isampler2D             -> "isampler2D"
  Isampler2DArray        -> "isampler2DArray"
  Usampler2D             -> "usampler2D"
  Usampler2DArray        -> "usampler2DArray"
  Sampler2DRect          -> "sampler2DRect"
  Sampler2DRectShadow    -> "sampler2DRectShadow"
  Isampler2DRect         -> "isampler2DRect"
  Usampler2DRect         -> "usampler2DRect"
  Sampler2DMS            -> "sampler2DMS"
  Isampler2DMS           -> "isampler2DMS"
  Usampler2DMS           -> "usampler2DMS"
  Sampler2DMSArray       -> "sampler2DMSArray"
  Isampler2DMSArray      -> "isampler2DMSArray"
  Usampler2DMSArray      -> "usampler2DMSArray"
  Sampler3D              -> "sampler3D"
  Isampler3D             -> "isampler3D"
  Usampler3D             -> "usampler3D"
  SamplerCube            -> "samplerCube"
  SamplerCubeShadow      -> "samplerCubeShadow"
  IsamplerCube           -> "isamplerCube"
  UsamplerCube           -> "usamplerCube"
  SamplerCubeArray       -> "samplerCubeArray"
  SamplerCubeArrayShadow -> "samplerCubeArrayShadow"
  IsamplerCubeArray      -> "isamplerCubeArray"
  UsamplerCubeArray      -> "usamplerCubeArray"
  SamplerBuffer          -> "samplerBuffer"
  IsamplerBuffer         -> "isamplerBuffer"
  UsamplerBuffer         -> "usamplerBuffer"
  Image1D                -> "image1D"
  Iimage1D               -> "iimage1D"
  Uimage1D               -> "uimage1D"
  Image1DArray           -> "image1DArray"
  Iimage1DArray          -> "iimage1DArray"
  Uimage1DArray          -> "uimage1DArray"
  Image2D                -> "image2D"
  Iimage2D               -> "iimage2D"
  Uimage2D               -> "uimage2D"
  Image2DArray           -> "image2DArray"
  Iimage2DArray          -> "iimage2DArray"
  Uimage2DArray          -> "uimage2DArray"
  Image2DRect            -> "image2DRect"
  Iimage2DRect           -> "iimage2DRect"
  Uimage2DRect           -> "uimage2DRect"
  Image2DMS              -> "image2DMS"
  Iimage2DMS             -> "iimage2DMS"
  Uimage2DMS             -> "uimage2DMS"
  Image2DMSArray         -> "image2DMSArray"
  Iimage2DMSArray        -> "iimage2DMSArray"
  Uimage2DMSArray        -> "uimage2DMSArray"
  Image3D                -> "image3D"
  Iimage3D               -> "iimage3D"
  Uimage3D               -> "uimage3D"
  ImageCube              -> "imageCube"
  IimageCube             -> "iimageCube"
  UimageCube             -> "uimageCube"
  ImageCubeArray         -> "imageCubeArray"
  IimageCubeArray        -> "iimageCubeArray"
  UimageCubeArray        -> "uimageCubeArray"
  ImageBuffer            -> "imageBuffer"
  IimageBuffer           -> "iimageBuffer"
  UimageBuffer           -> "uimageBuffer"
  Texture1D              -> "texture1D"
  Texture1DArray         -> "texture1DArray"
  Itexture1D             -> "itexture1D"
  Itexture1DArray        -> "itexture1DArray"
  Utexture1D             -> "utexture1D"
  Utexture1DArray        -> "utexture1DArray"
  Texture2D              -> "texture2DArray"
  Texture2DArray         -> "texture2DArray"
  Itexture2D             -> "itexture2D"
  Itexture2DArray        -> "itexture2DArray"
  Utexture2D             -> "utexture2D"
  Utexture2DArray        -> "utexture2DArray"
  Texture2DRect          -> "texture2DRect"
  Itexture2DRect         -> "itexture2DRect"
  Utexture2DRect         -> "utexture2DRect"
  Texture2DMS            -> "texture2DMS"
  Itexture2DMS           -> "itexture2DMS"
  Utexture2DMS           -> "utexture2DMS"
  Texture2DMSArray       -> "texture2DMSArray"
  Itexture2DMSArray      -> "itexture2DMSArray"
  Utexture2DMSArray      -> "utexture2DMSArray"
  Texture3D              -> "texture3D"
  Itexture3D             -> "itexture3D"
  Utexture3D             -> "utexture3D"
  TextureCube            -> "textureCube"
  ItextureCube           -> "itextureCube"
  UtextureCube           -> "utextureCube"
  TextureCubeArray       -> "textureCubeArray"
  ItextureCubeArray      -> "itextureCubeArray"
  UtextureCubeArray      -> "utextureCubeArray"
  Sampler                -> "sampler"
  SamplerShadow          -> "samplerShadow"
  SubpassInput           -> "subpassInput"
  IsubpassInput          -> "isubpassInput"
  UsubpassInput          -> "usubpassInput"
  SubpassInputMS         -> "subpassInputMS"
  IsubpassInputMS        -> "isubpassInputMS"
  UsubpassInputMS        -> "usubpassInputMS"

instance FromJSON TextureDescriptorType where
  parseJSON = withTextMaybe "type" textureDescriptorTypeFromText

instance ToJSON TextureDescriptorType where
  toJSON = String . textureDescriptorTypeToText

convertTextureDescriptorType :: TextureDescriptorType -> DescriptorType
convertTextureDescriptorType = \case
  Buffer                 -> DESCRIPTOR_TYPE_STORAGE_BUFFER
  Sampler1D              -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler1DShadow        -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler1DArray         -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler1DArrayShadow   -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler1D             -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler1DArray        -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler1D             -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler1DArray        -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2D              -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DShadow        -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DArray         -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DArrayShadow   -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler2D             -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler2DArray        -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler2D             -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler2DArray        -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DRect          -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DRectShadow    -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler2DRect         -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler2DRect         -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DMS            -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler2DMS           -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler2DMS           -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DMSArray       -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler2DMSArray      -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler2DMSArray      -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler3D              -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler3D             -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler3D             -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  SamplerCube            -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  SamplerCubeShadow      -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  IsamplerCube           -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  UsamplerCube           -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  SamplerCubeArray       -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  SamplerCubeArrayShadow -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  IsamplerCubeArray      -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  UsamplerCubeArray      -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  SamplerBuffer          -> DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  IsamplerBuffer         -> DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  UsamplerBuffer         -> DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  Image1D                -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage1D               -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage1D               -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image1DArray           -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage1DArray          -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage1DArray          -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image2D                -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage2D               -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage2D               -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image2DArray           -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage2DArray          -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage2DArray          -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image2DRect            -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage2DRect           -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage2DRect           -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image2DMS              -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage2DMS             -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage2DMS             -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image2DMSArray         -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage2DMSArray        -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage2DMSArray        -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image3D                -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage3D               -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage3D               -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  ImageCube              -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  IimageCube             -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  UimageCube             -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  ImageCubeArray         -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  IimageCubeArray        -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  UimageCubeArray        -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  ImageBuffer            -> DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  IimageBuffer           -> DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  UimageBuffer           -> DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  Texture1D              -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture1DArray         -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture1D             -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture1DArray        -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture1D             -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture1DArray        -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture2D              -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture2DArray         -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture2D             -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture2DArray        -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture2D             -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture2DArray        -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture2DRect          -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture2DRect         -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture2DRect         -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture2DMS            -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture2DMS           -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture2DMS           -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture2DMSArray       -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture2DMSArray      -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture2DMSArray      -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture3D              -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture3D             -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture3D             -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  TextureCube            -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  ItextureCube           -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  UtextureCube           -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  TextureCubeArray       -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  ItextureCubeArray      -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  UtextureCubeArray      -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Sampler                -> DESCRIPTOR_TYPE_SAMPLER
  SamplerShadow          -> DESCRIPTOR_TYPE_SAMPLER
  SubpassInput           -> DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  IsubpassInput          -> DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  UsubpassInput          -> DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  SubpassInputMS         -> DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  IsubpassInputMS        -> DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  UsubpassInputMS        -> DESCRIPTOR_TYPE_INPUT_ATTACHMENT

data SeparateImageDescriptorType
  = TextureBuffer
  | ItextureBuffer
  | UtextureBuffer
  deriving (Show)

separateImageDescriptorTypeFromText
  :: Text -> Maybe SeparateImageDescriptorType
separateImageDescriptorTypeFromText = \case
  "samplerBuffer"  -> Just TextureBuffer
  "isamplerBuffer" -> Just ItextureBuffer
  "utextureBuffer" -> Just UtextureBuffer
  _                -> Nothing

separateImageDescriptorTypeToText :: SeparateImageDescriptorType -> Text
separateImageDescriptorTypeToText = \case
  TextureBuffer  -> "samplerBuffer"
  ItextureBuffer -> "isamplerBuffer"
  UtextureBuffer -> "utextureBuffer"

instance FromJSON SeparateImageDescriptorType where
  parseJSON = withTextMaybe "type" separateImageDescriptorTypeFromText

instance ToJSON SeparateImageDescriptorType where
  toJSON = String . separateImageDescriptorTypeToText

convertSeparateImageDescriptorType
  :: SeparateImageDescriptorType -> DescriptorType
convertSeparateImageDescriptorType = \case
  TextureBuffer  -> DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  ItextureBuffer -> DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  UtextureBuffer -> DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER

makeTextureDescriptorSetLayoutBinding
  :: ShaderStage -> Texture -> (Int, DescriptorSetLayoutBinding)
makeTextureDescriptorSetLayoutBinding stage Texture {..} =
  ( set
  , zero { binding         = fromIntegral binding
         , descriptorType  = convertTextureDescriptorType type'
         , descriptorCount = maybe 1 (fromIntegral . V.product) array
         , stageFlags      = shaderStageFlagBits stage
         }
  )

makeDescriptorSetLayoutBindings
  :: ShaderStage
  -> Vector Ubo
  -> Vector Texture
  -> Vector (Int, DescriptorSetLayoutBinding)
makeDescriptorSetLayoutBindings stage ubos textures =
  uboBindings <> textureBindings
 where
  uboBindings = V.map (makeUboDescriptorSetLayoutBinding stage) ubos
  textureBindings =
    V.map (makeTextureDescriptorSetLayoutBinding stage) textures

makeDescriptorSetLayoutCreateInfos
  :: Vector (Int, DescriptorSetLayoutBinding)
  -> V.Vector (DescriptorSetLayoutCreateInfo '[])
makeDescriptorSetLayoutCreateInfos bindings =
  V.map makeDescriptorSetLayoutCreateInfo
    . V.fromList
    . M.elems
    . M.unionWith (V.++) emptySetsMap
    $ setsMap
 where
  setLayoutsSize = V.maximum . fmap fst $ bindings :: Int
  emptySetsMap :: Map Int (Vector DescriptorSetLayoutBinding)
  emptySetsMap = M.fromList . map (, []) $ [0 .. setLayoutsSize]
  setsMap :: Map Int (Vector DescriptorSetLayoutBinding)
  setsMap =
    M.fromList . map extract . NE.groupAllWith fst . V.toList $ bindings
  extract
    :: NE.NonEmpty (Int, DescriptorSetLayoutBinding)
    -> (Int, Vector DescriptorSetLayoutBinding)
  extract groups =
    (fst $ NE.head groups, V.fromList $ NE.toList $ fmap snd groups)

makeDescriptorSetLayoutCreateInfo
  :: Vector DescriptorSetLayoutBinding -> DescriptorSetLayoutCreateInfo '[]
makeDescriptorSetLayoutCreateInfo bindings = zero { bindings = bindings }

data VertexAttributeType
  = Vec2
  | Vec3
  | Vec4
  deriving (Show)

vertexAttributeTypeFromText :: Text -> Maybe VertexAttributeType
vertexAttributeTypeFromText = \case
  "vec2" -> Just Vec2
  "vec3" -> Just Vec3
  "vec4" -> Just Vec4
  _      -> Nothing

vertexAttributeTypeToText :: VertexAttributeType -> Text
vertexAttributeTypeToText = \case
  Vec2 -> "vec2"
  Vec3 -> "vec3"
  Vec4 -> "vec4"

instance FromJSON VertexAttributeType where
  parseJSON = withTextMaybe "type" vertexAttributeTypeFromText

instance ToJSON VertexAttributeType where
  toJSON = String . vertexAttributeTypeToText

convertVertexAttributeType :: VertexAttributeType -> (Word32, Format)
convertVertexAttributeType = \case
  Vec2 -> (2 * floatSize, FORMAT_R32G32_SFLOAT)
  Vec3 -> (3 * floatSize, FORMAT_R32G32B32_SFLOAT)
  Vec4 -> (4 * floatSize, FORMAT_R32G32B32A32_SFLOAT)
  where floatSize = 4

data VertexAttribute = VertexAttribute
  { binding  :: Word32
  , location :: Word32
  , size     :: Word32
  , format   :: Format
  }
  deriving Show

-- only support single binding for input vertex
-- [SaschaWillems - Multiple Vulkan buffer binding points](https://gist.github.com/SaschaWillems/428d15ed4b5d71ead462bc63adffa93a)
-- maybe can provide a binding map parameter to specify individual binding by name, like `foo [(1, ["inPos", "inColor"]) (2, ["texCoord"])]` speficies that `(binding = 1) inPos, (binding = 1) inColor, (binding = 2) texCoord`
-- [island pipeline](https://github.com/tgfrerer/island/blob/76d0d38cba74181fa3774cef38aba4d96b6861dc/modules/le_backend_vk/le_pipeline.cpp#L21)
makePipelineVertexInputStateCreateInfo
  :: Vector Input -> Maybe (PipelineVertexInputStateCreateInfo '[])
makePipelineVertexInputStateCreateInfo []     = Nothing
makePipelineVertexInputStateCreateInfo inputs = Just zero
  { vertexBindingDescriptions
  , vertexAttributeDescriptions
  }
 where
  vertexAttributes =
    join . V.map makeVertexAttribute $ inputs :: Vector VertexAttribute
  vertexAttributeDescriptions =
    makeVertexInputAttributeDescriptions vertexAttributes :: Vector
        VertexInputAttributeDescription
  vertexBindingDescriptions =
    makeVertexInputBindingDescriptions vertexAttributes :: Vector
        VertexInputBindingDescription

makeVertexInputBindingDescriptions
  :: Vector VertexAttribute -> Vector VertexInputBindingDescription
makeVertexInputBindingDescriptions =
  V.fromList
    . map (makeVertexInputBindingDescription . calculate)
    . NE.groupAllWith fst
    . map extract
    . V.toList
 where
  extract :: VertexAttribute -> ("binding" ::: Int, "size" ::: Int)
  extract attr =
    (fromIntegral $ binding (attr :: VertexAttribute), fromIntegral $ size attr)
  calculate
    :: NE.NonEmpty ("binding" ::: Int, "size" ::: Int)
    -> ("binding" ::: Int, "stride" ::: Int)
  calculate groups = (fst $ NE.head groups, sum $ fmap snd groups)

makeVertexInputBindingDescription
  :: ("binding" ::: Int, "stride" ::: Int) -> VertexInputBindingDescription
makeVertexInputBindingDescription (binding, stride) = zero
  { binding   = fromIntegral binding
  , stride    = fromIntegral stride
  , inputRate = VERTEX_INPUT_RATE_VERTEX
  }

makeVertexAttribute :: Input -> Vector VertexAttribute
makeVertexAttribute Input {..} = V.generate
  count
  (\i -> VertexAttribute { binding  = 0
                         , location = fromIntegral $ location + i
                         , size
                         , format
                         }
  )
 where
  count          = maybe 1 V.product array :: Int
  (size, format) = convertVertexAttributeType type'

makeVertexInputAttributeDescriptions
  :: Vector VertexAttribute -> Vector VertexInputAttributeDescription
makeVertexInputAttributeDescriptions =
  V.fromList
    . join
    . map process
    . groupBy ((==) `on` (binding :: VertexAttribute -> Word32))
    . V.toList
 where
  process :: [VertexAttribute] -> [VertexInputAttributeDescription]
  process = snd . mapAccumL makeVertexInputAttributeDescription 0

makeVertexInputAttributeDescription
  :: Word32 -> VertexAttribute -> (Word32, VertexInputAttributeDescription)
makeVertexInputAttributeDescription offset VertexAttribute {..} =
  (offset + size, zero { binding, location, offset, format })

readProcessHandler
  :: MonadIO m
  => (ExitCode, BL.ByteString, BL.ByteString)
  -> ExceptT String m BL.ByteString
readProcessHandler (exitCode, result, err) = case exitCode of
  ExitFailure errCode ->
    throwE $ "errCode: " <> show errCode <> ", " <> BL.unpack
      (if BL.null err then result else err)
  ExitSuccess -> pure result

procE :: MonadIO m => FilePath -> [String] -> ExceptT String m BL.ByteString
procE = fmap ((readProcessHandler =<<) . readProcess) . proc

reflect
  :: MonadIO m
  => ShaderStage
  -> "code" ::: Text
  -> m (Either String (B.ByteString, BL.ByteString))
reflect shaderStage code =
  liftIO . runExceptT . withSystemTempDirectory "th-spirv" $ \dir -> do
    let stage  = T.unpack . shaderStageToText $ shaderStage
    let shader = dir </> stage <.> "glsl"
    let spv    = dir </> stage <.> "spv"
    liftIO . T.writeFile shader $ code
    _spv <- procE "glslangValidator" ["-S", stage, "-V", shader, "-o", spv]
    spirv <- liftIO . B.readFile $ spv
    reflectionRaw <- procE "spirv-cross"
                           [spv, "--vulkan-semantics", "--reflect"]
    pure (spirv, reflectionRaw)

reflect'
  :: MonadIO m => "spirv" ::: B.ByteString -> m (Either String BL.ByteString)
reflect' spirv =
  liftIO . runExceptT . withSystemTempDirectory "th-spirv" $ \dir -> do
    let spv = dir </> "shader" <.> "spv"
    liftIO . B.writeFile spv $ spirv
    procE "spirv-cross" [spv, "--vulkan-semantics", "--reflect"]

reflection
  :: MonadIO m
  => ShaderStage
  -> "code" ::: Text
  -> m (Either String (Shader, Reflection))
reflection stage code = runExceptT $ do
  (spirv, reflectionRaw) <- except =<< reflect stage code
  ref <- except . eitherDecodeStrict' @Reflection . BL.toStrict $ reflectionRaw
  pure (Shader { stage, code = spirv }, ref)

reflection'
  :: MonadIO m
  => "spirv" ::: B.ByteString
  -> m (Either String (Shader, Reflection))
reflection' spirv = runExceptT $ do
  reflectionRaw <- except =<< reflect' spirv
  ref <- except . eitherDecodeStrict' @Reflection . BL.toStrict $ reflectionRaw
  pure (Shader { stage = getShaderStage ref, code = spirv }, ref)

getShaderStage :: Reflection -> ShaderStage
getShaderStage Reflection {..} = mode . V.head $ entryPoints
