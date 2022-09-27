{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Vulkan.Utils.Requirements.TH
  ( req
  , reqs
  )
where

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad
import Data.Char
import Data.Either (lefts)
import Data.Foldable
import Data.Functor ((<&>))
import Data.List ( intercalate, isPrefixOf, dropWhileEnd )
import Data.List.Extra (nubOrd)
import Data.Maybe
import Data.String
import Data.Traversable
import Data.Word
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.ParserCombinators.ReadP hiding
  ( optional
  )
import Text.Read (readMaybe)
import Vulkan.Requirement
import Vulkan.Utils.Internal
import Vulkan.Utils.Misc
import Vulkan.Version (pattern MAKE_API_VERSION)
import Prelude hiding (GT)

-- $setup
-- >>> import           Vulkan.Core11.Promoted_From_VK_KHR_multiview
-- >>> import           Vulkan.Core12
-- >>> import           Vulkan.Extensions.VK_KHR_ray_tracing_pipeline
-- >>> import           Vulkan.Zero

-- | Parse a requirement and produce an appropriate 'DeviceRequirement'
--
-- 'DeviceVersionRequirement's are specified by in the form
-- @<major>.<minor>[.<patch>]@
--
-- 'DeviceFeatureRequirement's are specified in the form @<type name>.<member
-- name>@ and produce a 'RequireDeviceFeature' which checks and sets this
-- feature.
--
-- 'DevicePropertyRequirement's are specified like feature requirements except
-- with an additional description of the constraint. This may be any of
--
-- - @myFunctionName@: To check with an in-scope function taking the property
--   type and returning 'Bool'
-- - @> 123@: To indicate a minimum bound on a integral property
-- - @>= 123@: To indicate an inclusive minimum bound on a integral property
-- - @& SOMETHING_BIT@: To indicate that the specified bit must be present in
--   the bitmask value
--
-- 'DeviceExtensionRequirement's are specified in the form @<extension name>
-- <optional version>@. @<extension name>@ must start with @VK_@. The version
-- will be compared against the 'specVersion' field of the
-- 'ExtensionProperties' record.
--
-- - Names may be qualified.
-- - The separator between the type and member can be any of @.@ @::@ @:@ @->@
--   or any amount of space
--
-- >>> let r = [req|PhysicalDeviceRayTracingPipelineFeaturesKHR.rayTracingPipeline|]
-- >>> featureName r
-- "PhysicalDeviceRayTracingPipelineFeaturesKHR.rayTracingPipeline"
--
-- >>> let r = [req|PhysicalDeviceVulkan11Features.multiview|]
-- >>> featureName r
-- "PhysicalDeviceVulkan11Features.multiview"
--
-- >>> let r = [reqs|  PhysicalDeviceTimelineSemaphoreFeatures.timelineSemaphore |]
-- >>> featureName <$> r
-- ["PhysicalDeviceTimelineSemaphoreFeatures.timelineSemaphore"]
--
-- >>> let r = [req|PhysicalDeviceMultiviewFeatures.doesn'tExist|]
-- ...
--     • Couldn't find member "doesn'tExist" in Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewFeatures
-- ...
--
-- >>> let r = [req|Doesn'tExist.multiview|]
-- ...
--     • Couldn't find type name "Doesn'tExist"
-- ...
--
-- >>> let r = [req|Either.multiview|]
-- ...
--     • Data.Either.Either doesn't seem to be the type of a record constructor
-- ...
req :: QuasiQuoter
req = (badQQ "req"){quoteExp = reqExp}

-- | Like 'reqs' except that this parses a list of newline separated
-- requirements
--
-- It ignores
--
-- - Blank lines
-- - Lines beginning with @--@ or @#@
reqs :: QuasiQuoter
reqs = (badQQ "req"){quoteExp = exps reqExp . filterComments}

reqExp :: String -> Q Exp
reqExp s = do
  case parse s of
    Nothing -> fail $ "Couldn't parse " <> show s
    Just r -> do
      r' <- nameRequest r
      renderRequest s r'

renderRequest :: String -> Request Name Name -> ExpQ
renderRequest input = \case
  Feature s m ->
    let check = explicitRecordGet s m (ConT ''Bool)
        enable = explicitRecordSet s m (ConT ''Bool) [|True|]
     in do
          [|
            let featureName = fromString $(lift input)
                checkFeature = $(check)
                enableFeature = $(enable)
             in RequireDeviceFeature featureName checkFeature enableFeature
            |]
  Property s m c ->
    let t = conT s
        getProp = [|\str -> $(varE m) (str :: $t)|]
        checker = case c of
          GTE v -> [|(>= $(litE (IntegerL v)))|]
          GT v -> [|(> $(litE (IntegerL v)))|]
          AndBit b -> [|(.&&. $(conE b))|]
          Fun f -> [|$(varE f)|]
        check = [|$checker . $getProp|]
     in [|
          let propertyName = fromString $(lift input)
              checkProperty = $(check)
           in RequireDeviceProperty propertyName checkProperty
          |]
  Extension s v ->
    [|
      let deviceExtensionLayerName = Nothing
          deviceExtensionName = fromString $(lift s)
          deviceExtensionMinVersion = $(lift (fromMaybe minBound v))
       in RequireDeviceExtension
            deviceExtensionLayerName
            deviceExtensionName
            deviceExtensionMinVersion
      |]
  Version v -> [|RequireDeviceVersion $(lift v)|]

nameRequest :: Request [String] String -> Q (Request Name Name)
nameRequest = \case
  Feature s m -> do
    sName <- getQualTyName s
    let mName = mkName m
    pure $ Feature sName mName
  Property s m c -> do
    sName <- getQualTyName s
    let mName = mkName m
    c' <- for c getQualValueName
    pure $ Property sName mName c'
  Extension s v -> pure $ Extension s v
  Version v -> pure $ Version v
 where
  getQualTyName n = do
    let q = intercalate "." n
    maybe (fail $ "Couldn't find type name " <> show q) pure
      =<< lookupTypeName q
  getQualValueName n = do
    let q = intercalate "." n
    maybe (fail $ "Couldn't find value name " <> show q) pure
      =<< lookupValueName q

data Request qual unqual
  = Version Word32
  | Feature qual unqual
  | Property qual unqual (Constraint qual)
  | Extension String (Maybe Word32)
  deriving (Show)

data Constraint qual
  = GTE Integer
  | GT Integer
  | AndBit qual
  | Fun qual
  deriving (Show, Functor, Foldable, Traversable)

-- |
-- >>> parse ""
-- Nothing
--
-- >>> parse "Foo->bar"
-- Just (Feature ["Foo"] "bar")
--
-- >>> parse "V.Foo.bar"
-- Just (Feature ["V","Foo"] "bar")
--
-- >>> parse "V.E.Foo bar"
-- Just (Feature ["V","E","Foo"] "bar")
--
-- >>> parse "1.2"
-- Just (Version 4202496)
--
-- >>> parse "1 2 1"
-- Just (Version 4202497)
--
-- >>> parse "Foo.bar >= 10"
-- Just (Property ["Foo"] "bar" (GTE 10))
--
-- >>> parse "V.Foo.bar & A.B.C_BIT"
-- Just (Property ["V","Foo"] "bar" (AndBit ["A","B","C_BIT"]))
--
-- >>> parse "V.Foo.bar even"
-- Just (Property ["V","Foo"] "bar" (Fun ["even"]))
--
-- >>> parse "V.Foo.bar Prelude.even"
-- Just (Property ["V","Foo"] "bar" (Fun ["Prelude","even"]))
parse :: String -> Maybe (Request [String] String)
parse =
  let varRemChars = munch (isAlphaNum <||> (== '\'') <||> (== '_'))
      var = (:) <$> satisfy (isLower <||> (== '_')) <*> varRemChars
      con = (:) <$> satisfy isUpper <*> varRemChars
      mod' = con
      qual :: ReadP String -> ReadP [String]
      qual x = (pure <$> x) <|> ((:) <$> (mod' <* char '.') <*> qual x)
      separator = asum (skipSpaces : (void . string <$> [".", "->", "::", ":"]))

      digits = munch1 isDigit
      integer = readS_to_P (reads @Integer)
      word = do
        Just w <- readMaybe <$> digits
        pure w

      comp = do
        c <- (GT <$ string ">") <|> (GTE <$ string ">=")
        skipSpaces
        w <- integer
        pure $ c w
      andBit = do
        _ <- string "&"
        skipSpaces
        q <- qual con
        pure $ AndBit q
      fun = Fun <$> qual var
      constraint = comp <|> andBit <|> fun

      version = do
        ma <- word
        separator
        mi <- word
        pa <- fromMaybe 0 <$> (separator *> optional word)
        pure $ Version (MAKE_API_VERSION ma mi pa)

      feature = do
        s <- qual con
        _ <- separator
        m <- var
        pure $ Feature s m

      property = do
        s <- qual con
        _ <- separator
        m <- var
        skipSpaces
        c <- constraint
        pure $ Property s m c

      extension = do
        let prefix = "VK_"
        _ <- string prefix
        e <- (prefix <>) <$> munch (isAlphaNum <||> (== '_'))
        skipSpaces
        v <- optional word
        pure $ Extension e v

      request = do
        skipSpaces
        asum
          [ p <* skipSpaces <* eof
          | p <- [version, feature, property, extension]
          ]
   in readP_to_S request >>> \case
        -- xs        -> pure $ Feature [] (show xs)
        [(r, "")] -> pure r
        _ -> Nothing
{-# ANN parse ("HLint: ignore Use <$>" :: String) #-}

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- | Filters blank or commented lines, remove duplicates
filterComments :: String -> [String]
filterComments =
  let bad = (("--" `isPrefixOf`) <||> ("#" `isPrefixOf`) <||> null)
   in nubOrd . filter (not . bad) . fmap strip . lines

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

exps :: (String -> ExpQ) -> [String] -> ExpQ
exps f = listE . fmap f

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

----------------------------------------------------------------
-- TH Utils
----------------------------------------------------------------

explicitRecordSet
  :: Name
  -- ^ Type name
  -> Name
  -- ^ member name
  -> Type
  -- ^ member type
  -> ExpQ
  -- ^ new value
  -> ExpQ
explicitRecordSet s m t x = do
  (con, ns) <- overRecord s m t
  ns' <- traverse (traverse newName) ns
  let pats = either (const wildP) varP <$> ns'
  let apps = either (const x) varE <$> ns'
  [|\ $(conP con pats) -> $(foldl appE (conE con) apps)|]

explicitRecordGet
  :: Name
  -- ^ Type name
  -> Name
  -- ^ member name
  -> Type
  -- ^ member type
  -> ExpQ
explicitRecordGet s m t = do
  (con, ns) <- overRecord s m t
  x <- newName "x"
  let pats = either (const (varP x)) (const wildP) <$> ns
  [|\ $(conP con pats) -> $(varE x)|]

overRecord
  :: Name
  -- ^ Type name
  -> Name
  -- ^ member name
  -> Type
  -- ^ member type
  -> Q (Name, [Either Type String])
  -- ^ (Constructor, [Left var name, Right selected member type])
overRecord s (nameBase -> m) t = do
  reify s >>= \case
    TyConI (DataD _ _ _ _ [c] _) | RecC con vs <- c ->
      let ns = vs <&> \case
            (nameBase -> n, _bang, t') | n == m    -> Left t'
                                       | otherwise -> Right n
      in  case lefts ns of
            [] -> fail $ "Couldn't find member " <> show m <> " in " <> show s
            [t']
              | t == t'
              -> pure (con, ns)
              | otherwise
              -> fail
                $  "Member "
                <> show m
                <> " of "
                <> show s
                <> " has type "
                <> show t'
                <> " but we expected "
                <> show t
            _ ->
              fail
                $  "Found multiple members called"
                <> show m
                <> " in "
                <> show s
                <> " ...what?"
    _ ->
      fail $ show s <> " doesn't seem to be the type of a record constructor"
