{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Exception
  ( vkExceptionWriteElement
  ) where

import           Control.Arrow                            ((&&&))
import           Control.Bool
import           Control.Monad
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Char                                (isUpper, toUpper)
import           Data.Closure
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.List                                (partition)
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Data.Monoid                              (Endo (..))
import qualified Data.MultiMap                            as MultiMap
import qualified Data.Set                                 as Set
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Data.Traversable
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Error
import           Spec.Savvy.Struct
import qualified Spec.Savvy.Type.Haskell                  as H

import           Documentation
import           Write.Element

vkExceptionWriteElement :: WriteElement
vkExceptionWriteElement =
  let
    weName       = "VulkanException declaration"
    weImports    = [Import "Control.Exception" ["Exception"]]
    weProvides   = Unguarded <$>
                     [ TypeConstructor "VulkanException"
                     , Term "VulkanException"
                     , Term "vulkanExceptionResult"
                     ]
    weDepends    = [Unguarded (TypeName "VkResult")]
    weExtensions = []
    weDoc = pure [qci|
      -- | This exception is thrown from calls to marshalled vulkan commands
      -- which return a negative VkResult.
      newtype VulkanException = VulkanException \{ vulkanExceptionResult :: VkResult }
        deriving (Eq, Ord, Read, Show)
      instance Exception VulkanException
    |]
  in WriteElement{..}
