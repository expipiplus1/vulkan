{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.Spec
  ( Spec(..)
  , spec
  ) where

import           Control.Applicative
import           Data.Either.Validation
import           Data.List
import qualified Data.Map                as Map
import           Data.Monoid             (Endo (..))
import           Data.Text               (Text)
import           Prelude                 hiding (Enum)
import           Spec.Savvy.APIConstant
import           Spec.Savvy.Command
import           Spec.Savvy.Enum
import           Spec.Savvy.Error
import           Spec.Savvy.FuncPointer
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import           Spec.Savvy.Type.Packing
import           Spec.Savvy.TypeAlias
import qualified Spec.Spec               as P

data Spec = Spec
  { sEnums        :: [Enum]
  , sTypeAliases  :: [TypeAlias]
  , sConstants    :: [APIConstant]
  , sFuncPointers :: [FuncPointer]
  , sCommands     :: [Command]
  , sStructs      :: [Struct]
  }
  deriving (Show)

spec :: P.Spec -> Either [SpecError] Spec
spec s = do
  pc <- specParserContext s
  (enums, aliases, constants, funcPointers) <-
    validationToEither
    $   (,,,)
    <$> specEnums s
    <*> specTypeAliases s
    <*> specConstants s
    <*> specFuncPointers pc s
  let getType t =
        (TypeName <$> getAlias1 aliases t)
          <|> getFuncPointer funcPointers t
          <|> getEnum enums t
      tc = TypeContext pc
                       (Endo (typeSize getType (getConstantValue constants)))
                       (Endo (typeAlignment getType))
  validationToEither
    $   Spec enums aliases constants funcPointers
    <$> specCommands pc s
    <*> specStructs  tc s

getConstantValue :: [APIConstant] -> Text -> Maybe Word
getConstantValue cs t = do
  c <- find ((== t) . acName) cs
  case acValue c of
    IntegralValue k -> Just k
    _               -> Nothing


-- | From Control.Zipper
farthest :: (a -> Maybe a) -> a -> a
farthest f = go where
  go a = maybe a go (f a)

getAlias :: [TypeAlias] -> Text -> Text
getAlias = farthest . getAlias1

getAlias1 :: [TypeAlias] -> Text -> Maybe Text
getAlias1 as = (`Map.lookup` m)
  where m = Map.fromList [ (taName, taAlias) | TypeAlias {..} <- as ]

getFuncPointer :: [FuncPointer] -> Text -> Maybe Type
getFuncPointer fs t = fpType <$> find ((== t) . fpName) fs

getEnum :: [Enum] -> Text -> Maybe Type
getEnum es = (`Map.lookup` m)
  where
    m = Map.fromList
      [ (name, type')
      | Enum {..} <- es
      , name <- eName : eAliases
      , let type' = case eType of
              EnumTypeBitmask -> TypeName "uint32_t"
              EnumTypeEnum    -> Int
      ]
