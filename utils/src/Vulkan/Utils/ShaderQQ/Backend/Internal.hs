module Vulkan.Utils.ShaderQQ.Backend.Internal
  ( messageProcess
  ) where

import           Data.ByteString                ( ByteString )
import           Data.List.Extra

messageProcess
  :: (Applicative m, Monad m)
  => String
  -- ^ tool name
  -> (String -> m ())
  -- ^ warning
  -> (String -> m ByteString)
  -- ^ error
  -> ([String], Either [String] ByteString)
  -- ^ Spir-V bytecode with warnings or errors
  -> m ByteString
  -- ^ Spir-V bytecode
messageProcess tool warn err (warnings, result) = do
  case warnings of
    []    -> pure ()
    _some -> warn $ prepare warnings
  case result of
    Left []     -> err $ tool ++ " failed with no errors"
    Left errors -> do
      _ <- err $ prepare errors
      pure mempty
    Right bs -> pure bs
  where
    prepare [singleLine] = singleLine
    prepare multiline =
      intercalate "\n" $ (tool ++ ":") : map (mappend "        ") multiline
