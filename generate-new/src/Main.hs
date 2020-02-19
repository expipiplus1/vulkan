module Main
  where

import           Relude                  hiding ( runReader
                                                , uncons
                                                )
import           Relude.Extra.Map
import           Say
import           System.TimeIt
import           Polysemy
import           Polysemy.Reader
import qualified Data.Vector.Storable.Sized    as VSS
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import           Data.Text.Extra                ( lowerCaseFirst
                                                , upperCaseFirst
                                                , (<+>)
                                                )

import           CType
import           Error
import           Marshal
import           Marshal.Scheme
import           Render.Element
import           Render.Aggregate
import           Bespoke.Seeds
import           Render.Type
import           Render.Spec
import           Spec.Parse

main :: IO ()
main = (runM . runErr $ go) >>= \case
  Left es -> do
    traverse_ sayErr es
    sayErr (show (length es) <+> "errors")
  Right () -> pure ()
 where
  go = do
    specText <- timeItNamed "Reading spec"
      $ readFileBS "./Vulkan-Docs/xml/vk.xml"

    spec@Spec {..} <- timeItNamed "Parsing spec" $ parseSpec specText

    let structNames :: HashSet Text
        structNames =
          fromList . (extraStructNames <>) . toList . fmap sName $ specStructs
        isStruct' = (`member` structNames)
        mps       = MarshalParams isDefaultable' isStruct' isPassAsPointerType'

    (ss, cs) <- runReader mps $ do
      ss <- timeItNamed "Marshaling structs"
        $ traverseV marshalStruct specStructs
      cs <- timeItNamed "Marshaling commands"
        $ traverseV marshalCommand specCommands
        -- TODO: Don't use all commands here, just those commands referenced by
        -- features and extensions. Similarly for specs
      pure (ss, cs)

    withTypeInfo spec $ do

      renderElements <-
        timeItNamed "Rendering"
        .   runReader renderParams
        $   traverse evaluateWHNF
        =<< renderSpec spec ss cs

      groups <- timeItNamed "Segmenting" $ do
        seeds <- specSeeds spec
        segmentRenderElements show renderElements seeds

      timeItNamed "writing"
        $ withTypeInfo spec (renderSegments "out" (mergeElements groups))

----------------------------------------------------------------
-- Names
----------------------------------------------------------------

renderParams :: RenderParams
renderParams = RenderParams
  { mkTyName                = unReservedWord . upperCaseFirst
  , mkConName               = \parent ->
                                unReservedWord
                                  . (case parent of
                                      "VkPerformanceCounterResultKHR" -> ("Counter" <>)
                                      _ -> id
                                    )
                                  . upperCaseFirst
  , mkMemberName            = unReservedWord . lowerCaseFirst
  , mkFunName               = unReservedWord
  , mkParamName             = unReservedWord
  , mkPatternName           = unReservedWord
  , mkHandleName            = unReservedWord
  , mkFuncPointerName       = unReservedWord . T.tail
  , mkFuncPointerMemberName = unReservedWord . ("p" <>) . upperCaseFirst
  , alwaysQualifiedNames    = V.fromList [''VSS.Vector]
  }

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
