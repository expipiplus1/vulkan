module Vulkan.Utils.ShaderQQ.ShaderType
  ( ShaderType (..)
  ) where

import           Data.String (IsString (..))

data ShaderType
  = GLSL
  | HLSL

instance IsString ShaderType where
  fromString = \case
    "glsl" -> GLSL
    "hlsl" -> HLSL
    t -> error $ "not support '" ++ t ++ "' shader"

instance Show ShaderType where
  show = \case
    GLSL -> "glsl"
    HLSL -> "hlsl"
