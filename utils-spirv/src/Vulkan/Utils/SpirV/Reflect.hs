{-| Reflect compiled SPIR-V into a 'Module', both at runtime and inside
Template Haskell splices.
-}
module Vulkan.Utils.SpirV.Reflect
  ( Module
  , reflectFile
  , reflectBytes
  , reflectFileQ
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Language.Haskell.TH.Syntax (Q, addDependentFile, runIO)

import Data.SpirV.Reflect.FFI (load, loadBytes)
import Data.SpirV.Reflect.Module (Module)

-- | Reflect a @.spv@ file into a 'Module'.
reflectFile :: (MonadIO m) => FilePath -> m Module
reflectFile = load

-- | Reflect SPIR-V bytecode into a 'Module'.
reflectBytes :: (MonadIO m) => ByteString -> m Module
reflectBytes = loadBytes

{- | Reflect a @.spv@ file at compile time, registering it as a dependency so
the splice is rerun when the file changes.
-}
reflectFileQ :: FilePath -> Q Module
reflectFileQ path = do
  addDependentFile path
  runIO $ reflectFile path
