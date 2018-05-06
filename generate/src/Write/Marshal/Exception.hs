{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Exception
  ( vkExceptionWriteElement
  ) where

import           Text.InterpolatedString.Perl6.Unindented

import           Write.Element

vkExceptionWriteElement :: WriteElement
vkExceptionWriteElement =
  let
    weName       = "VulkanException declaration"
    weImports    = [Import "Control.Exception" ["Exception"]]
    weProvides   = Unguarded <$>
                     [ TypeConstructor "VulkanException"
                     , Term "VulkanException"
                     ]
    weUndependableProvides = []
    weSourceDepends        = []
    weBootElement          = Nothing
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
