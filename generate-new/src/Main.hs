module Main
  where

import           Relude                  hiding ( runReader
                                                , uncons
                                                )
import           Relude.Extra.Map
import           Say
import           Polysemy
import           Polysemy.Reader
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import           Data.Text.Extra                ( (<+>)
                                                , cons
                                                , uncons
                                                )
import           Data.Char
import           Language.Haskell.TH            ( nameModule
                                                , nameBase
                                                )

import           CType
import           Error
import           Marshal
import           Marshal.Scheme
import           Render.Element
import           Render.Type
import           Render.Spec
import           Spec.Parse
import           Write.Segment

main :: IO ()
main = do
  sayErr "Reading spec"
  specText <- readFileBS "./Vulkan-Docs/xml/vk.xml"
  let
    r = do
      sayErr "Parsing spec"
      spec@Spec {..} <- parseSpec specText
      let structNames :: HashSet Text
          structNames =
            fromList . (extraStructNames <>) . toList . fmap sName $ specStructs
          isStruct' = (`member` structNames)
          mps = MarshalParams isDefaultable' isStruct' isPassAsPointerType'
      runReader mps $ do
        sayErr "Marshaling structs"
        ss <- traverseV marshalStruct specStructs
        sayErr "Marshaling commands"
        cs <- traverseV marshalCommand specCommands
        sayErr "Rendering"
        renderElements <- renderSpec spec ss cs
        sayErr "Segmenting"
        let
          featureSeeds :: V.Vector (V.Vector Text)
          featureSeeds = V.concatMap
            ( fmap
                (\re -> rCommandNames re <> rTypeNames re <> rEnumValueNames re)
            . V.filter ((/= Just "Header boilerplate") . rComment)
            . fRequires
            )
            specFeatures
          seeds = featureSeeds
          elementExports RenderElement {..} =
            reInternal <> reExports <> V.concatMap exportWith reExports
        (segments, extras) <- segmentGraph
          reName
          show
          (fmap exportName . V.toList . elementExports)
          ( fmap (toText . nameBase)
          . filter (isNothing . nameModule)
          . fmap importName
          . toList
          . reImports
          )
          renderElements
          seeds
        traverse_ (sayErr . reName) extras
        sayErr "Segments:"
        traverse_ (\s -> do
          sayErrShow . V.length $ s
          sayErrShow (reName <$> s)
         ) segments
        sayErr "Extras"
        sayErrShow . V.length $ extras
        pure extras
  (runM . runReader renderParams . runErr $ r) >>= \case
    Left es -> do
      traverse_ sayErr es
      sayErr (show (length es) <+> "errors")
    Right rs -> do
      sayErr "Writing"
      renderModule "out" (V.singleton "Vulkan") rs

----------------------------------------------------------------
-- Names
----------------------------------------------------------------

renderParams :: RenderParams
renderParams = RenderParams { mkTyName          = unReservedWord . upperCaseFirst
                            , mkConName         = unReservedWord . upperCaseFirst
                            , mkMemberName      = unReservedWord . lowerCaseFirst
                            , mkFunName         = unReservedWord
                            , mkParamName       = unReservedWord
                            , mkPatternName     = unReservedWord
                            , mkHandleName      = unReservedWord
                            , mkFuncPointerName = unReservedWord . T.tail
                            }

lowerCaseFirst :: Text -> Text
lowerCaseFirst = onFirst Data.Char.toLower

upperCaseFirst :: Text -> Text
upperCaseFirst = onFirst Data.Char.toUpper

onFirst :: (Char -> Char) -> Text -> Text
onFirst f = \case
  Cons c cs -> Cons (f c) cs
  t         -> t

pattern Cons :: Char -> Text -> Text
pattern Cons c cs <- (uncons -> Just (c, cs))
  where Cons c cs = cons c cs

unReservedWord :: Text -> Text
unReservedWord t = if t `elem` (keywords <> preludeWords) then t <> "'" else t
 where
  keywords =
    [ "as"
    , "case"
    , "class"
    , "data family"
    , "data instance"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "family"
    , "forall"
    , "foreign"
    , "hiding"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "mdo"
    , "module"
    , "newtype"
    , "of"
    , "proc"
    , "qualified"
    , "rec"
    , "then"
    , "type"
    , "where"
    ]
  preludeWords = ["filter"]

----------------------------------------------------------------
-- Bespoke Vulkan stuff
----------------------------------------------------------------

isDefaultable' :: CType -> Bool
isDefaultable' t =
  -- isBitmask'              <- (isJust .) <$> asks lIsBitmask
  -- isNonDispatchableHandle <-
  --   (maybe False (\h -> hHandleType h == NonDispatchable) .) <$> asks lIsHandle
  isDefaultableForeignType t || isIntegral t
    -- TODO
    -- || isBitmask || isNonDispatchableHandle

isIntegral :: CType -> Bool
isIntegral =
  (`elem` [ Int
          , Char
          , TypeName "uint8_t"
          , TypeName "uint16_t"
          , TypeName "uint32_t"
          , TypeName "uint64_t"
          , TypeName "int8_t"
          , TypeName "int16_t"
          , TypeName "int32_t"
          , TypeName "int64_t"
          , TypeName "size_t"
          , TypeName "VkDeviceSize"
          , TypeName "VkDeviceAddress"
          ]
  )

isDefaultableForeignType :: CType -> Bool
isDefaultableForeignType =
  (`elem` [ TypeName "HANDLE"
          , TypeName "DWORD"
          , TypeName "LPCWSTR"
          , TypeName "PFN_vkInternalAllocationNotification"
          , TypeName "PFN_vkInternalFreeNotification"
          , TypeName "PFN_vkAllocationFunction"
          , TypeName "PFN_vkReallocationFunction"
          , TypeName "PFN_vkFreeFunction"
          , Ptr CType.Const (TypeName "SECURITY_ATTRIBUTES")
          ]
  )

-- | Is this a type we don't want to marshal
isPassAsPointerType' :: CType -> Bool
isPassAsPointerType' = \case
  TypeName n
    | n
      `elem` [ "MirConnection"
             , "wl_display"
             , "wl_surface"
             , "Display"
             , "xcb_connection_t"
             , "AHardwareBuffer"
             , "ANativeWindow"
             , "CAMetalLayer"
             , "SECURITY_ATTRIBUTES"
             ]
    -> True
  _ -> False

-- TODO: Remove, extra union names and handle
extraStructNames :: [Text]
extraStructNames = ["VkClearColorValue", "VkSemaphore"]
