{-# language CPP #-}
-- No documentation found for Chapter "VK_QCOM_render_pass_store_ops"
module Vulkan.Extensions.VK_QCOM_render_pass_store_ops  ( QCOM_render_pass_store_ops_SPEC_VERSION
                                                        , pattern QCOM_render_pass_store_ops_SPEC_VERSION
                                                        , QCOM_render_pass_store_ops_EXTENSION_NAME
                                                        , pattern QCOM_render_pass_store_ops_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)

type QCOM_render_pass_store_ops_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_QCOM_render_pass_store_ops_SPEC_VERSION"
pattern QCOM_render_pass_store_ops_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_render_pass_store_ops_SPEC_VERSION = 2


type QCOM_render_pass_store_ops_EXTENSION_NAME = "VK_QCOM_render_pass_store_ops"

-- No documentation found for TopLevel "VK_QCOM_render_pass_store_ops_EXTENSION_NAME"
pattern QCOM_render_pass_store_ops_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_render_pass_store_ops_EXTENSION_NAME = "VK_QCOM_render_pass_store_ops"

