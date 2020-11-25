{-# language CPP #-}
-- No documentation found for Chapter "FuncPointers"
module Vulkan.Core10.FuncPointers  ( PFN_vkVoidFunction
                                   , FN_vkVoidFunction
                                   ) where

import Foreign.Ptr (FunPtr)

type FN_vkVoidFunction = () -> IO ()
-- No documentation found for TopLevel "PFN_vkVoidFunction"
type PFN_vkVoidFunction = FunPtr FN_vkVoidFunction

