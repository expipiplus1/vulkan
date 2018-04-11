{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Write.Spec
  ( writeSpec
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Either.Validation
import           Data.Foldable
import           Data.Maybe
import qualified Data.Set                              as Set
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Say
import           Spec.Bitmask
import           Spec.Constant
import           Spec.Enum
import           Spec.Savvy.Command
import           Spec.Savvy.Enum
import           Spec.Savvy.Error
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import           Spec.Spec
import           Spec.Type
-- import           Write.Bitmask
import           Write.Element
import           Write.Error
import           Write.Type.Enum

writeSpec :: Spec -> IO ()
writeSpec spec = do
  case specParserContext spec of
    Left es ->
     traverse_ (sayErr . prettySpecError) es
    Right pc ->
     case specCommands pc spec of
       Failure es ->
        traverse_ (sayErr . prettySpecError) es
       Success cs -> print cs
  -- case specParserContext spec of
  --   Left es ->
  --    traverse_ (sayErr . prettySpecError) es
  --   Right pc ->
  --    case specStructs pc spec of
  --      Failure es ->
  --       traverse_ (sayErr . prettySpecError) es
  --      Success ss -> print ss
  -- case genWriteElements spec of
  --   Left es ->
  --     traverse_ (sayErr . prettySpecError) es
  --   Right ws ->
  --     traverse_ sayShow ws

genWriteElements :: Spec -> Either [SpecError] [WriteElement]
genWriteElements s
  = validationToEither $ do
      es <- specEnums s
      pure (writeEnum <$> es)


-- -- | Pairs types with bitmasks
-- pairBitmasks :: Spec -> Validation [Text] [(BitmaskType, Bitmask)]
-- pairBitmasks Spec {..} = do
--   let bmts    = [ bmt | ABitmaskType bmt <- sTypes ]
--       bmtRequiresL = catMaybes (bmtRequires <$> bmts)
--       bms     = sBitmasks
--       numBmtRequires = length bmtRequiresL
--       numBms  = length bms
--   _ <- unless (numBmtRequires == numBms) $ Failure
--     [ "Number of bitmask type requires does not equal the number of bitmask declarations: "
--       <> T.pack (show numBmtRequires)
--       <> " and "
--       <> T.pack (show numBms)
--       <> " respectively"
--     ]
--   let bmNames         = Set.fromList (bmName <$> bms)
--       bmtNames        = Set.fromList (bmtName <$> bmts)
--       bmtRequires     = Set.fromList bmtRequiresL
--       missingBmNames  = bmNames Set.\\ bmtRequires
--       missingBmtNames = bmtNames Set.\\ bmNames
--   _ <- unless (missingBmNames == Set.empty) $ Failure
--     [ "Missing Bitmask Names (without a bitmask type): "
--         <> T.pack (show missingBmNames)
--     ]
--   _ <- unless (missingBmtNames == Set.empty) $ Failure
--     [ "Missing Bitmask Type Names (without a bitmask declaration): "
--         <> T.pack (show missingBmtNames)
--     ]
--   return []

{-
specNames :: Spec -> [String]
Data.Text.Prettyprint.Doc.Render.Text

specNames = do
  ts <- concatMap typeNames . sTypes
  cs <- concatMap constantNames . sConstants
  es <- concatMap enumNames . sEnums
  bs <- concatMap bitmaskNames . sBitmasks
  pure (concat [ts, cs, es, bs])

typeNames :: TypeDecl -> [String]
typeNames = \case
  APlatformHeader _ -> []
  ARequirement _ -> []
  ADefine Define{..} -> [dName]
  ABaseType BaseType{..} -> [btName]
  ABitmaskType BitmaskType{..} -> [bmtName]
  AHandleType HandleType{..} -> [htName]
  AnEnumType EnumType{..} -> [etName]
  AFuncPointerType FuncPointerType{..} -> [fptName]
  AStructType StructType{..} -> [stName]
  AUnionType UnionType{..} -> [utName]
  ASectionComment SectionComment{..} -> []
  AnAlias TypeAlias{..} -> [taName]

constantNames :: Constant -> [String]
constantNames Constant{..} = [cName]

enumNames :: Spec.Enum.Enum -> [String]
enumNames Enum{..} = [eName]

bitmaskNames :: Bitmask -> [String]
bitmaskNames Bitmask{..} = [bmName]
-}

{-
import           Spec.Spec
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  (Doc, indent, vcat, (<+>))

import           Control.Arrow                 (second)
import           Data.Foldable                 (traverse_)
import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S
import           Data.List                     (sort)
import           Data.String
import           Spec.Graph
import           Spec.Partition
import           Write.CycleBreak
import           Write.Module
import           Write.Utils
import           Write.WriteMonad

writeSpecModules :: FilePath -> Spec -> IO ()
writeSpecModules root spec = do
  let graph = getSpecGraph spec
      partitions = second S.toList <$> M.toList (moduleExports (partitionSpec spec graph))
      locations = M.unions (uncurry exportMap <$> partitions)
      moduleNames = fst <$> partitions
      moduleStrings = uncurry (writeModule graph locations Normal) <$>
                      partitions
      modules = zip moduleNames moduleStrings
  traverse_ (createModuleDirectory root) (fst <$> modules)
  mapM_ (uncurry (writeModuleFile root)) modules
  writeHsBootFiles root graph locations
  writeModuleFile root (ModuleName "Graphics.Vulkan")
                       (writeParentModule moduleNames)

writeModuleFile :: FilePath -> ModuleName -> String -> IO ()
writeModuleFile root moduleName =
  writeFile (moduleNameToFile root moduleName)

exportMap :: ModuleName -> [String] -> M.HashMap String ModuleName
exportMap moduleName exports = M.fromList ((,moduleName) <$> exports)

writeParentModule :: [ModuleName] -> String
writeParentModule names = show moduleDoc
  where nameStrings = fmap fromString . sort . fmap unModuleName $ names
        moduleDoc :: Doc
        moduleDoc = [qc|module Graphics.Vulkan
  ( {indent (-2) . vcat $ intercalatePrepend (fromString ",") ((fromString "module" <+>) <$> nameStrings)}
  ) where

{vcat $ (fromString "import" <+>) <$> nameStrings}|]

-}
