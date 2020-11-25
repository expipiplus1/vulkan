{-# language CPP #-}
-- No documentation found for Chapter "Exception"
module Vulkan.Exception  (VulkanException(..)) where

import GHC.Exception.Type (Exception(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))

        -- | This exception is thrown from calls to marshalled Vulkan commands
        -- which return a negative VkResult.
        newtype VulkanException = VulkanException { vulkanExceptionResult :: Result }
          deriving (Eq, Ord, Read, Show)

        instance Exception VulkanException where
          displayException (VulkanException r) = show r ++ ": " ++ resultString r

        -- | A human understandable message for each VkResult
        resultString :: Result -> String
        resultString = \case
          
          r -> show r

