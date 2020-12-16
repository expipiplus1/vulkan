{-# language CPP #-}
-- No documentation found for Chapter "Exception"
module OpenXR.Exception  (OpenXrException(..)) where

import GHC.Exception.Type (Exception(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
-- | This exception is thrown from calls to marshalled Vulkan commands
-- which return a negative VkResult.
newtype OpenXrException = OpenXrException { vulkanExceptionResult :: Result }
  deriving (Eq, Ord, Read, Show)

instance Exception OpenXrException where
  displayException (OpenXrException r) = show r ++ ": " ++ resultString r

-- | A human understandable message for each VkResult
resultString :: Result -> String
resultString = \case
  
  r -> show r

