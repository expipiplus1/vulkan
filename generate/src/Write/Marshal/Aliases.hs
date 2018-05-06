{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Aliases
  ( makeMarshalledAliases
  ) where

import           Data.Maybe
import qualified Data.Text.Extra   as T
import           Prelude           hiding (Enum)
import           Spec.Savvy.Alias  as A
import           Spec.Savvy.Enum
import           Spec.Savvy.Handle
import           Spec.Savvy.Spec

makeMarshalledAliases :: Spec -> Aliases
makeMarshalledAliases Spec {..} =
  let
    -- Command aliases require type info, so are handled by the command writer
      commandAliases = []
      -- Anum aliases are not given marshalled names
      enumAliases    = mapMaybe makeMarshalledEnumAlias sEnums
        ++ mapMaybe makeMarshalledAliasAlias (A.enumAliases sAliases)
      handleAliases = mapMaybe makeMarshalledHandleAlias sHandles
        ++ mapMaybe makeMarshalledAliasAlias (A.handleAliases sAliases)
      -- Struct aliases require type info, so are handled by the struct writer
      structAliases        = []
      -- Constants are not given marshalled names
      constantAliases      = []
      -- Enum extensions are not given marshalled names
      enumExtensionAliases = []
  in  Aliases {..}

makeMarshalledHandleAlias :: Handle -> Maybe (Alias Handle)
makeMarshalledHandleAlias h@Handle {..} = do
  aName <- T.dropPrefix "Vk" hName
  let aAliasName = hName
      aAlias     = ATarget h
  pure Alias {..}

makeMarshalledEnumAlias :: Enum -> Maybe (Alias Enum)
makeMarshalledEnumAlias e@Enum {..} = do
  aName <- T.dropPrefix "Vk" eName
  let aAliasName = eName
      aAlias     = ATarget e
  pure Alias {..}

makeMarshalledAliasAlias :: Alias a -> Maybe (Alias a)
makeMarshalledAliasAlias a = do
  aName      <- T.dropPrefix "Vk" (aName a)
  aAliasName <- T.dropPrefix "Vk" (A.aAliasName a)
  let aAlias = AnAlias (Alias aAliasName (A.aAliasName a) (AnAlias a))
  pure Alias {..}
