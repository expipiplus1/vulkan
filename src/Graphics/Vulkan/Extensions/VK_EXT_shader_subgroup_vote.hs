{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_EXT_shader_subgroup_vote
  ( pattern VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION
  , pattern VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )





-- No documentation found for TopLevel "VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION"
pattern VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME"
pattern VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME = "VK_EXT_shader_subgroup_vote"
