{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.Enum
  ( Enum(..)
  , EnumType(..)
  , EnumElement(..)
  , EnumExtension(..)
  , specEnums
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Closure
import           Data.Either.Validation
import           Data.Foldable
import           Data.Int
import qualified Data.Map                  as Map
import           Data.Maybe
import qualified Data.MultiMap             as MultiMap
import           Data.MultiMap.Extra       ()
import           Data.Semigroup
import qualified Data.Set                  as Set
import           Data.Text.Extra           as T
import           Data.Traversable
import           Data.Word
import           Prelude                   hiding (Enum)
import qualified Spec.Bitmask              as P
import qualified Spec.Enum                 as P
import qualified Spec.ExtensionTag         as P
import           Spec.Savvy.Enum.Extension
import           Spec.Savvy.Error
import qualified Spec.Spec                 as P
import qualified Spec.Type                 as P

data Enum = Enum
  { eName       :: Text
    -- ^ The type name of the enumeration
  , eType       :: EnumType
    -- ^ Is this enumeration a bitmask or not
  , eAliases    :: [Text]
    -- ^ The closure of the aliases to this type, doesn't include aliases from
    -- extensions.
  , eComment    :: Maybe Text
    -- ^ A comment from the XML specification
  , eElements   :: [EnumElement]
    -- ^ The elements of the enumeration
  , eExtensions :: [EnumExtension]
    -- ^ Any extensions to this enumeration from features or extensions
  }
  deriving (Show)

data EnumType
  = EnumTypeBitmask
    -- ^ Bitmask enums are backed by the VkFlags type (Word32)
  | EnumTypeEnum
    -- ^ Enum enums are backed by Int32
  deriving (Show)

data EnumElement = EnumElement
  { eeName    :: Text
    -- ^ The name of this enumerant
  , eeValue   :: Either Int32 Word32
    -- ^ The numeric value of this enumerant
  , eeComment :: Maybe Text
    -- ^ A comment from the XML specification
  }
  deriving (Show)

-- | Bitmasks and Enumerations
specEnums :: P.Spec -> Validation [SpecError] [Enum]
specEnums spec = (<>) <$> specEnumEnums spec <*> specBitmasks spec

-- | Enums which aren't bitmasks
specEnumEnums :: P.Spec -> Validation [SpecError] [Enum]
specEnumEnums spec@P.Spec {..} =
  let --
      -- First, filter out the relevant info from the spec
      --
      bitmaskEnumTypes = Set.fromList
        [ req
        | P.ABitmaskType bmt <- sTypes
        , Just           req <-
          [guessBitmaskRequirement (P.getSpecExtensionTags spec) bmt]
        ]
      enumTypes =
        [ et
        | P.AnEnumType et <- sTypes
        , Set.notMember (P.etName et) bitmaskEnumTypes
        ]
      --- | A map of (target, name) pairs
      enumAliases :: [(Text, Text)]
      enumAliases =
        [ (taAlias, taName)
        | P.AnAlias P.TypeAlias {..} <- sTypes
        , taCategory == "enum"
        , Set.notMember taAlias bitmaskEnumTypes
        ]
      enums    = sEnums
      enumMap  = Map.fromList [ (P.eName e, e) | e <- enums ]
      aliasMap = MultiMap.fromList enumAliases

      (extensionFailures, allExtensionEnums) = specEnumExtensions spec
  in  do
        -- Sanity check what we have in the parsed spec
        _ <- enumSanityCheck (P.etName <$> enumTypes)
                             (fst <$> enumAliases)
                             (P.eName <$> enums)

        _ <- unless (Prelude.null extensionFailures) $ Failure extensionFailures

        -- ApplicativeDo makes me do this :(
        r <- for enumTypes $ \P.EnumType {..} -> do
          -- Try and find the enum value declaration, error if it doesn't exist
          ee <- case Map.lookup etName enumMap of
            Nothing -> Failure [Other ("Missing enum value name: " <> etName)]
            Just ee -> pure ee

          pure
            $ let eName = etName
                  eElements =
                    [ EnumElement {eeValue = Left (P.eeValue el), ..}
                    | el@P.EnumElement {..} <- P.eElements ee
                    ]
                  eAliases    = closeNonReflexive (`MultiMap.lookup` aliasMap) [etName]
                  eComment    = P.eComment ee
                  eExtensions = MultiMap.lookup eName allExtensionEnums
                  eType       = EnumTypeEnum
              in  Enum {..}
        pure r

specBitmasks :: P.Spec -> Validation [SpecError] [Enum]
specBitmasks spec@P.Spec {..}
  = let --
        -- First, filter out the relevant info from the spec
        --
      bitmaskTypes     = [ bmt | P.ABitmaskType bmt <- sTypes ]
      bitmaskEnumTypes = Set.fromList
        [ req
        | bmt      <- bitmaskTypes
        , Just req <-
          [guessBitmaskRequirement (P.getSpecExtensionTags spec) bmt]
        ]
      --- | A list of (target, name) pairs
      bitmaskAliases :: [(Text, Text)]
      bitmaskAliases =
        [ (taAlias, taName)
          | P.AnAlias P.TypeAlias {..} <- sTypes
          , taCategory
            == "enum"
            && Set.member taAlias bitmaskEnumTypes
            || taCategory
            == "bitmask"
          ]
          ++ [ (req, bmtName)
             | P.BitmaskType {..} <- bitmaskTypes
             , Just req           <- [bmtRequires]
             ]
      bitmasks          = sBitmasks
      bitmaskMap        = Map.fromList [ (P.bmName b, b) | b <- bitmasks ]
      aliasMap          = MultiMap.fromList bitmaskAliases

      allExtensionEnums = specBitmaskExtensions spec
    in
      do
        -- Sanity check what we have in the parsed spec
        _ <- enumSanityCheck
          (  (P.bmtName <$> bitmaskTypes)
          ++ [ r | Just r <- P.bmtRequires <$> bitmaskTypes ]
          )
          (fst <$> bitmaskAliases)
          (P.bmName <$> bitmasks)

        -- ApplicativeDo makes me do this :(
        r <-
          for bitmaskTypes $ eitherToValidation . \bmt@P.BitmaskType {..} -> do

            unless (bmtType == "VkFlags") $ Left [WeirdBitmaskType bmtType]

            -- Try and find the bitmask value declaration
            pure $ case bmtRequires of
              Nothing -> emptyEnum
                bmt
                (closeNonReflexive (`MultiMap.lookup` aliasMap) [bmtName])
                (MultiMap.lookup bmtName allExtensionEnums)
              Just bitmaskRequirement ->
                case Map.lookup bitmaskRequirement bitmaskMap of
                  Nothing -> emptyEnum
                    bmt
                    (MultiMap.lookup bmtName aliasMap)
                    (MultiMap.lookup bmtName allExtensionEnums)
                  Just bm@P.Bitmask {..} ->
                    let
                      eName = bmName
                      eElements =
                        [ EnumElement {..}
                          | P.BitmaskBitPosition {..} <- bmBitPositions
                          , let eeName    = bmbpName
                                eeValue   = Right (0x1 `shiftL` bmbpBitPos)
                                eeComment = bmbpComment
                          ]
                          ++ [ EnumElement {..}
                             | P.BitmaskValue {..} <- bmValues
                             , let eeName    = bmvName
                                   eeValue   = Right bmvValue
                                   eeComment = bmvComment
                             ]
                      eAliases =
                        closeNonReflexive (`MultiMap.lookup` aliasMap) [bmName]
                      eComment    = P.bmComment bm
                      eExtensions = MultiMap.lookup bmName allExtensionEnums
                      eType       = EnumTypeBitmask
                    in
                      Enum {..}
        pure r

emptyEnum
  :: P.BitmaskType
  -- ^ The Type to make the enum from
  -> [Text]
  -- ^ A list of aliases
  -> [EnumExtension]
  -- ^ Extensions for this enum
  -> Enum
emptyEnum P.BitmaskType {..} aliases extensions =
  let eName       = bmtName
      eElements   = []
      eAliases    = aliases
      eComment    = Nothing
      -- TODO: Should this always be empty?
      eExtensions = extensions
      eType       = EnumTypeBitmask
  in  Enum {..}

enumSanityCheck
  :: [Text]
  -- ^ Type names
  -> [Text]
  -- ^ Enum type aliases
  -> [Text]
  -- ^ Value declaration type names
  -> Validation [SpecError] ()
enumSanityCheck tNamesL aliases dNamesL = do
  -- Check that each enum value declaration is referenced by an enum type
  let tNames       = Set.fromList tNamesL
      dNames       = Set.fromList dNamesL
      missingNames = dNames Set.\\ tNames
  _ <- unless (Set.null missingNames)
    $ Failure (EnumTypeMissing <$> Set.toList missingNames)

  -- Check that each alias references a type
  let aTargets            = Set.fromList $ aliases
      missingAliasTargets = aTargets Set.\\ tNames
  _ <- unless (Set.null missingAliasTargets)
    $ Failure (AliasTargetMissing <$> Set.toList missingAliasTargets)

  pure ()

-- | Bitmasks with no values (because there are no use cases yet) have no
-- requires field. This function tries to guess it.
--
-- If the bitmask type already has a requirement, use that.
--
-- If we don't know how to generate a requirement name then return Nothing
guessBitmaskRequirement
  :: [P.ExtensionTag]
  -- ^ The list of vendor tags
  -> P.BitmaskType
  -> Maybe Text
guessBitmaskRequirement vendorTags P.BitmaskType {..} = asum
  (bmtRequires : (flip guessTag bmtName <$> (nullExtensionTag : vendorTags)))
  where
    nullExtensionTag = P.ExtensionTag ""

    guessTag :: P.ExtensionTag -> Text -> Maybe Text
    guessTag P.ExtensionTag {..} =
      replaceSuffix ("Flags" <> unExtensionTag) ("FlagBits" <> unExtensionTag)

    replaceSuffix :: Text -> Text -> Text -> Maybe Text
    replaceSuffix from to s = do
      guard (from `T.isSuffixOf` s)
      pure $ T.dropEnd (T.length from) s <> to
