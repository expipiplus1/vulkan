{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_shader_subgroup_ballot
  ( pattern VK_EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME
  , pattern VK_EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )





-- No documentation found for TopLevel "VK_EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME"
pattern VK_EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME = "VK_EXT_shader_subgroup_ballot"

-- No documentation found for TopLevel "VK_EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION"
pattern VK_EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION = 1
