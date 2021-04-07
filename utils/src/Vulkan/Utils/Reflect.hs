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
import           Data.Aeson.Types               ( Parser
                                                , parseFail
                                                )
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

import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( join )
import           Data.Function                  ( on )
import           Data.List                      ( groupBy
                                                , mapAccumL
                                                , sortOn
                                                )
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

data Reflection = Reflection
  { entryPoints :: Vector EntryPoint
  , inputs      :: Maybe (Vector Input)
  , textures    :: Maybe (Vector Texture)
  , ubos        :: Maybe (Vector Ubo)
  }
  deriving (Show, Generic, FromJSON, ToJSON)

withTextMaybe :: String -> (Text -> Maybe a) -> Value -> Parser a
withTextMaybe label fromText = withText label $ \text -> case fromText text of
  Nothing    -> parseFail $ "Unexpected " <> label <> " value: " <> show text
  Just known -> pure known

data ShaderStage
  = Vert
  | Frag
  | Comp
  | Tesc
  | Tese
  | Geom
  | Rgen
  | Rint
  | Rahit
  | Rchit
  | Rmiss
  | Rcall
  | Task
  | Mesh
  deriving (Eq, Show)

shaderStageFromText :: Text -> Maybe ShaderStage
shaderStageFromText = \case
  "vert"  -> Just Vert
  "frag"  -> Just Frag
  "comp"  -> Just Comp
  "tesc"  -> Just Tesc
  "tese"  -> Just Tese
  "geom"  -> Just Geom
  "rgen"  -> Just Rgen
  "rint"  -> Just Rint
  "rahit" -> Just Rahit
  "rchit" -> Just Rchit
  "rmiss" -> Just Rmiss
  "rcall" -> Just Rcall
  "task"  -> Just Task
  "mesh"  -> Just Mesh
  _       -> Nothing

shaderStageToText :: ShaderStage -> Text
shaderStageToText = \case
  Vert  -> "vert"
  Frag  -> "frag"
  Comp  -> "comp"
  Tesc  -> "tesc"
  Tese  -> "tese"
  Geom  -> "geom"
  Rgen  -> "rgen"
  Rint  -> "rint"
  Rahit -> "rahit"
  Rchit -> "rchit"
  Rmiss -> "rmiss"
  Rcall -> "rcall"
  Task  -> "task"
  Mesh  -> "mesh"

instance FromJSON ShaderStage where
  parseJSON = withTextMaybe "mode" shaderStageFromText

instance ToJSON ShaderStage where
  toJSON     = String . shaderStageToText
  toEncoding = toEncoding . shaderStageToText

shaderStageFlagBits :: ShaderStage -> ShaderStageFlagBits
shaderStageFlagBits = \case
  Vert  -> SHADER_STAGE_VERTEX_BIT
  Frag  -> SHADER_STAGE_FRAGMENT_BIT
  Comp  -> SHADER_STAGE_COMPUTE_BIT
  Tesc  -> SHADER_STAGE_TESSELLATION_CONTROL_BIT
  Tese  -> SHADER_STAGE_TESSELLATION_EVALUATION_BIT
  Geom  -> SHADER_STAGE_GEOMETRY_BIT
  Rgen  -> SHADER_STAGE_RAYGEN_BIT_KHR
  Rint  -> SHADER_STAGE_INTERSECTION_BIT_KHR
  Rahit -> SHADER_STAGE_ANY_HIT_BIT_KHR
  Rchit -> SHADER_STAGE_CLOSEST_HIT_BIT_KHR
  Rmiss -> SHADER_STAGE_MISS_BIT_KHR
  Rcall -> SHADER_STAGE_CALLABLE_BIT_KHR
  Task  -> SHADER_STAGE_TASK_BIT_NV
  Mesh  -> SHADER_STAGE_MESH_BIT_NV

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
makeDescriptorInfo = makeDescriptorSetLayoutCreateInfos . join . V.map
  (   makeDescriptorSetLayoutBindings
  <$> makeShaderStage
  <*> makeUbos
  <*> makeTextures
  )
 where
  makeShaderStage :: (Shader, Reflection) -> ShaderStage
  makeShaderStage = (stage :: Shader -> ShaderStage) . fst
  makeUbos :: (Shader, Reflection) -> Vector Ubo
  makeUbos = fromMaybe [] . ubos . snd
  makeTextures :: (Shader, Reflection) -> Vector Texture
  makeTextures = fromMaybe [] . textures . snd

makeInputInfo
  :: Vector (Shader, Reflection)
  -> Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
makeInputInfo =
  (SomeStruct <$>)
    . makePipelineVertexInputStateCreateInfo
    . join
    . V.mapMaybe (id <$>)
    . (inputs . snd <$>)
    . V.filter ((== Vert) . (stage :: Shader -> ShaderStage) . fst)

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
         , descriptorCount = maybe 1 (fromIntegral . V.sum) array
         , stageFlags      = shaderStageFlagBits stage
         }
  )

data TextureDescriptorType = Sampler2D
  deriving Show

textureDescriptorTypeFromText :: Text -> Maybe TextureDescriptorType
textureDescriptorTypeFromText = \case
  "sampler2D" -> Just Sampler2D
  _           -> Nothing

textureDescriptorTypeToText :: TextureDescriptorType -> Text
textureDescriptorTypeToText = \case
  Sampler2D -> "sampler2D"

instance FromJSON TextureDescriptorType where
  parseJSON = withTextMaybe "type" textureDescriptorTypeFromText

instance ToJSON TextureDescriptorType where
  toJSON     = String . textureDescriptorTypeToText
  toEncoding = toEncoding . textureDescriptorTypeToText

convertTextureDescriptorType :: TextureDescriptorType -> DescriptorType
convertTextureDescriptorType = \case
  Sampler2D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER

makeTextureDescriptorSetLayoutBinding
  :: ShaderStage -> Texture -> (Int, DescriptorSetLayoutBinding)
makeTextureDescriptorSetLayoutBinding stage Texture {..} =
  ( set
  , zero { binding         = fromIntegral binding
         , descriptorType  = convertTextureDescriptorType type'
         , descriptorCount = maybe 1 (fromIntegral . V.sum) array
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
    . M.unionWith (V.++) sets
    $ setsMap
 where
  setSize = V.maximum . (fst <$>) $ bindings :: Int
  sets :: Map Int (Vector DescriptorSetLayoutBinding)
  sets = M.fromList . map (, []) $ [0 .. setSize]
  setsMap :: Map Int (Vector DescriptorSetLayoutBinding)
  setsMap =
    M.fromList
      . map (liftA2 (,) (fst . head) (V.fromList . (snd <$>)))
      . groupBy ((==) `on` fst)
      . sortOn fst
      . V.toList
      $ bindings

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
  toJSON     = String . vertexAttributeTypeToText
  toEncoding = toEncoding . vertexAttributeTypeToText

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
    . groupBy ((==) `on` fst)
    . sortOn fst
    . map extract
    . V.toList
 where
  extract :: VertexAttribute -> ("binding" ::: Int, "size" ::: Int)
  extract = liftA2 (,)
                   (fromIntegral . (binding :: VertexAttribute -> Word32))
                   (fromIntegral . size)
  calculate
    :: [("binding" ::: Int, "size" ::: Int)]
    -> ("binding" ::: Int, "stride" ::: Int)
  calculate = liftA2 (,) (fst . head) (sum . (snd <$>))

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
  count          = maybe 1 V.sum array :: Int
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
