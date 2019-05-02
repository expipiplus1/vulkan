{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Aliases
  ( makeMarshalledAliases
  , makeMarshalledEnumAliases
  ) where

import           Control.Monad                            ( guard )
import           Data.Maybe
import           Data.Functor
import qualified Data.Text.Extra               as T
import           Data.Text.Extra                          ( Text )
import           Prelude                           hiding ( Enum )
import           Spec.Savvy.Alias              as A
import           Spec.Savvy.Enum
import           Spec.Savvy.Handle
import           Spec.Savvy.Spec
import           Spec.Savvy.Extension
import           Spec.Savvy.Feature

makeMarshalledAliases :: Spec -> Aliases
makeMarshalledAliases Spec {..} =
  let
    -- Command aliases require type info, so are handled by the command writer
    commandAliases = []
    enumAliases    = -- mapMaybe makeMarshalledEnumAlias sEnums ++
                     catMaybes
      (A.enumAliases sAliases <&> \(e) ->
        -- (,,)
          -- <$>
                                          (makeMarshalledAliasAlias "Vk" e)
          -- <*> (traverse (makeMarshalledAliasAlias "VK_") ees)
          -- <*> (traverse (makeMarshalledAliasAlias "VK_") exs)
                                                                           )
    handleAliases = mapMaybe makeMarshalledHandleAlias sHandles
      ++ mapMaybe (makeMarshalledAliasAlias "Vk") (A.handleAliases sAliases)
    -- Struct aliases require type info, so are handled by the struct writer
    structAliases            = []
    -- Constants are not given marshalled names
    constantAliases          = [] -- mapMaybe makeMarshalledConstant sConstants
    constantExtensionAliases = mapMaybe
      makeMarshalledConstantAlias
      (rConstants =<< extRequirements =<< sExtensions)
    -- Enum extensions are not given marshalled names
    enumExtensionAliases = mapMaybe (makeMarshalledAliasAlias "VK_")
                                    (A.enumExtensionAliases sAliases)
  in
    Aliases { .. }

makeMarshalledEnumAliases
  :: Spec -> [(Alias Enum, [Alias EnumElement], [Alias EnumExtension])]
makeMarshalledEnumAliases Spec {..} = flip mapMaybe sEnums $ \e@Enum {..} -> do
  guard (eName `notElem` unmarshalledEnumNames)
  aName <- T.dropPrefix "Vk" eName
  let aAliasName = eName
      aAlias     = ATarget e
  ees <- traverse makeMarshalledEnumElementAlias eElements
  exs <- traverse makeMarshalledEnumExtensionAlias eExtensions
  pure (Alias { .. }, ees, exs)

-- | Note that this doesn't make an alias for dispatchable handles, these are
-- handled separately because they contain the table of command addresses
makeMarshalledHandleAlias :: Handle -> Maybe (Alias Handle)
makeMarshalledHandleAlias h@Handle {..} = do
  guard (NonDispatchable == hHandleType)
  aName <- T.dropPrefix "Vk" hName
  let aAliasName = hName
      aAlias     = ATarget h
  pure Alias {..}

makeMarshalledConstantAlias
  :: ConstantExtension -> Maybe (Alias ConstantExtension)
makeMarshalledConstantAlias e@ConstantExtension {..} = do
  aName <- T.dropPrefix "VK_" ceName
  let aAliasName = ceName
      aAlias     = ATarget e
  pure Alias { .. }

makeMarshalledEnumElementAlias :: EnumElement -> Maybe (Alias EnumElement)
makeMarshalledEnumElementAlias e@EnumElement {..} = do
  aName <- T.dropPrefix "VK_" eeName
  let aAliasName = eeName
      aAlias     = ATarget e
  pure Alias { .. }

makeMarshalledEnumExtensionAlias :: EnumExtension -> Maybe (Alias EnumExtension)
makeMarshalledEnumExtensionAlias e@EnumExtension {..} = do
  aName <- T.dropPrefix "VK_" exName
  let aAliasName = exName
      aAlias     = ATarget e
  pure Alias { .. }

makeMarshalledAliasAlias :: Text -> Alias a -> Maybe (Alias a)
makeMarshalledAliasAlias prefix a = do
  aName      <- T.dropPrefix prefix (aName a)
  aAliasName <- T.dropPrefix prefix (A.aAliasName a)
  let aAlias = AnAlias (Alias aAliasName (A.aAliasName a) (AnAlias a))
  pure Alias {..}

----------------------------------------------------------------
-- Quirks
----------------------------------------------------------------

-- These are only useful on the C side, so don't write aliases for them
unmarshalledEnumNames :: [T.Text]
unmarshalledEnumNames = ["VkSystemAllocationScope", "VkInternalAllocationType"]
