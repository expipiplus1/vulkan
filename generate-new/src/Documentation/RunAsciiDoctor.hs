module Documentation.RunAsciiDoctor
  ( manTxtToDocbook
  , main
  ) where

import qualified Data.Attoparsec.Text.Lazy     as A
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as T
                                                ( toStrict )
import qualified Data.Text.Lazy                as TL
import           Relude                  hiding ( getArgs )
import           Replace.Attoparsec.Text.Lazy
import           Say
import           Spec.Flavor
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process.Typed

-- | Convert a man page from the Vulkan-Docs repo into docbook format using
-- 'asciidoctor'
--
-- This function also applies a few fixes to the output to make it more
-- friendly for Pandoc.
manTxtToDocbook
  :: SpecFlavor
  -> [Text]
  -- ^ A list of extension names to enable
  -> FilePath
  -- ^ The 'Vulkan-Docs' directory, necessary to find plugins
  -> FilePath
  -- ^ The path to the man page to translate
  -> IO (Either Text Text)
  -- ^ Either an error if something went wrong, or the docbook xml
manTxtToDocbook specFlavor extensions vkPath manTxt =
  fmap (T.toStrict . asciidoctor4076 . asciidoctor4075 . fixupDocbookOutput)
    <$> asciidoctor specFlavor extensions vkPath manTxt

asciidoctor
  :: SpecFlavor
  -> [Text]
  -- ^ Extension names
  -> FilePath
  -- ^ The 'Vulkan-Docs' directory, necessary to find plugins
  -> FilePath
  -- ^ The path to the man page to translate
  -> IO (Either Text TL.Text)
asciidoctor specFlavor extensions vkPathRelative manTxt = do
  vkPath <- makeAbsolute vkPathRelative
  let
    asciidoctorPath = "asciidoctor"
    -- This is mimicing the Makefile in Vulkan-Docs but generates docbook5
    -- output.
    extAttribs      = preceedAll "-a" (T.unpack <$> extensions)
    -- TODO: Version information here
    -- Base path to SPIR-V extensions on the web.
    spirvPath
      = "https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions"
    attribOpts =
      [ "-a"
      , "chapters=" <> vkPath <> "/chapters"
      , "-a"
      , "images=" <> vkPath <> "/images"
      , "-a"
      , "generated=" <> vkPath <> "/gen"
      , "-a"
      , "config=" <> vkPath <> "/config"
      , "-a"
      , "appendices=" <> vkPath <> "/appendices"
      , "-a"
      , "refprefix="
      , "-a"
      , "spirv=" <> spirvPath
      ]

    noteOpts = []
    adocExts = case specFlavor of
      SpecVk ->
        [ "-I"
        , vkPath </> "gen"
        , "-r"
        , vkPath </> "config/spec-macros.rb"
        ]
      SpecXr -> ["-r", vkPath </> "scripts/openxr-macros.rb"]
    adocOpts           = attribOpts ++ noteOpts ++ adocExts
    mathAsInlineImages = False
    mathemeticalOpts   = if mathAsInlineImages
      then
        [ "-r"
        , "asciidoctor-mathematical"
        , "-r"
        , vkPath </> "config/asciidoctor-mathematical-ext.rb"
        , "-a"
        , "mathematical-format=png"
        , "-a"
        , "mathematical-ppi=100"
        ]
      else []
    args =
      attribOpts
        ++ extAttribs
        ++ adocOpts
        ++ mathemeticalOpts
        ++ ["--backend", "docbook5", manTxt, "--out-file", "-"]
    p = setStdin closed $ proc asciidoctorPath args
  (exitCode, out, err) <- readProcess p
  case exitCode of
    ExitFailure e ->
      pure
        .  Left
        $  "asciidoctor failed with code "
        <> T.pack (show e)
        <> ":\ncommand: "
        <> T.pack asciidoctorPath
        <> T.concat ((" " <>) . T.pack <$> args)
        <> "\noutput:"
        <> T.toStrict (decodeUtf8 err)
    ExitSuccess -> do
      pure
        . Right
        . decodeUtf8
        $ out

-- | Some hacky replaces in the docbook XML to make pandoc cope better
-- TODO: Write an asciidoctor plugin to do these
-- TODO: remove the xml:id hack
fixupDocbookOutput :: TL.Text -> TL.Text
fixupDocbookOutput =
  replaceTag "sidebar" Nothing "section"
    . replaceTag "strong" (Just "class=\"purple\"") "emphasis"
    . TL.replace "<sidebar xml:id=\"resources-image-creation-limits\">"
                 "<sidebar>"
    . streamEdit
        ("<literal><xref linkend=\"" *> A.takeTill (== '"') <* "\"/></literal>")
        (\e -> "<literal>" <> e <> "</literal>")

-- | Work around https://github.com/asciidoctor/asciidoctor/issues/4075
asciidoctor4075 :: TL.Text -> TL.Text
asciidoctor4075 = TL.replace
  "base64<"
  "base64\"></imagedata></imageobject></inlinemediaobject><"

-- | Work around https://github.com/asciidoctor/asciidoctor/issues/4076
asciidoctor4076 :: TL.Text -> TL.Text
asciidoctor4076 = TL.replace "</superscript></link>" "</link></superscript>"

replaceTag
  :: TL.Text
  -- ^ Tag name
  -> Maybe TL.Text
  -- ^ Optional attribute to search for
  -> TL.Text
  -- ^ Replacement
  -> TL.Text
  -- ^ Haystack
  -> TL.Text
replaceTag needle maybeAttr replacement =
  let attr = maybe "" (" " <>) maybeAttr
  in  TL.replace ("<" <> needle <> attr <> ">") ("<" <> replacement <> ">")
        . TL.replace ("</" <> needle <> ">") ("</" <> replacement <> ">")

-- call like: :main vk ./Vulkan-Docs ./Vulkan-Docs/gen/api/structs/VkXYColorEXT.txt
main :: IO ()
main = do
  [flavor, d, m] <- getArgs
  let f = case flavor of
        "vk" -> SpecVk
        "xr" -> SpecXr
        _    -> error "invalid flavor"
  manTxtToDocbook f [] d m >>= \case
    Left  e  -> sayErr e
    Right d' -> say d'

-- | @preceedAll x xs@ inserts @x@ before every element in @xs@
preceedAll :: a -> [a] -> [a]
preceedAll x = \case
  [] -> []
  xs -> x : L.intersperse x xs
