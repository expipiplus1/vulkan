module Bracket
  where

import           Relude                  hiding ( Handle
                                                , Type
                                                )
import           Data.List.Extra                ( nubOrd
                                                , elemIndex
                                                )
import qualified Data.Text.Extra               as T
import           Language.Haskell.TH            ( mkName )
import           Data.Text.Prettyprint.Doc
                                         hiding ( brackets
                                                , plural
                                                )
import           Polysemy
import           Polysemy.Input

import qualified Control.Exception

import           Render.Element
import           Render.Utils
import           Render.SpecInfo
import           Render.Command
import           Render.Names
import           Spec.Parse
import           Haskell                       as H
import           Error
import           Marshal.Scheme
import           Marshal.Command
import           Render.Scheme

data Bracket = Bracket
  { bInnerTypes          :: [MarshalScheme Parameter]
  , bWrapperName         :: CName
  , bCreate              :: CName
  , bDestroy             :: CName
  , bCreateArguments     :: [Argument]
  , bDestroyArguments    :: [Argument]
  , bDestroyIndividually :: Bool
  }
  deriving (Show)

data Argument
  = Provided Text (MarshalScheme Parameter)
  -- ^ This value is passed in explicitly
  | Resource ResourceType Int
  -- ^ This value is the resource being used, element n of generated tuple
  | Member Text Text
  -- ^ (The name of a provided parameter, The record selector)
  deriving (Show, Eq, Ord)

data ResourceType
  = IdentityResource
  -- ^ This value is just the resource being used
  | ElemResource
  -- ^ This value is an element in the vector of resources being used
  deriving (Show, Eq, Ord)

isResource :: Argument -> Bool
isResource = \case
  Resource     _ _ -> True
  _                -> False

-- | Try to generate a bracket automatically from the types of a pair of
-- marshaled commands.
autoBracket
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => MarshaledCommand
  -- ^ Begin
  -> MarshaledCommand
  -- ^ End
  -> CName
  -- ^ Bracketing function name
  -> Sem r Bracket
autoBracket create destroy with = do
  beginHasReturnType <- marshaledCommandShouldIncludeReturnValue create
  let bWrapperName = with
      bCreate      = mcName create
      bDestroy     = mcName destroy
      bInnerTypes =
        [ Normal t | beginHasReturnType, let t = mcReturn create ]
          <> [ s
             | MarshaledParam {..} <- toList (mcParams create)
             , not (isElided mpScheme)
             , Returned s <- pure mpScheme
             ]
      bCreateArguments =
        [ Provided (unCName (pName mpParam)) mpScheme
        | MarshaledParam {..} <- toList (mcParams create)
        , not (isElided mpScheme)
        , isNegative mpScheme
        ]
      bDestroyIndividually = False
      destroyNegativeParams =
        [ mp
        | mp@MarshaledParam {..} <- toList (mcParams destroy)
        , not (isElided mpScheme)
        , isNegative mpScheme
        ]
  destroyReturnTypes <- marshaledCommandReturnTypes False destroy
  unless (null destroyReturnTypes)
    $ throw "TODO: Bracketing functions where the destructor returns a value"
  bDestroyArguments <- forV destroyNegativeParams $ \MarshaledParam {..} ->
    let
      provided             = Provided (unCName (pName mpParam)) mpScheme
      providedForCreate    = provided `elem` bCreateArguments
      providedByCreate     = mpScheme `elemIndex` bInnerTypes
      providedByCreateElem = Vector mpScheme `elemIndex` bInnerTypes
    in
      case (providedForCreate, providedByCreate, providedByCreateElem) of
        (False, Nothing, Nothing) -> pure provided
        (True , Nothing, Nothing) -> pure provided
        (False, Just i , Nothing) -> pure (Resource IdentityResource i)
        (False, Nothing, Just i ) -> pure (Resource ElemResource i)
        -- TODO: Neaten error messages
        (True, Just _, Just _) ->
          throw
            $ "Destructor input "
            <> show (pName mpParam)
            <> " is ambiguous, it is provided for the create function and is also present in the output of the create function and an element in an output vector of the create function"
        (False, Just _, Just _) ->
          throw
            $ "Destructor input "
            <> show (pName mpParam)
            <> " is ambiguous, it is both provided for by the output of the create function and an element in an output vector of the create function"
        (True, Just _, Nothing) ->
          throw
            $ "Destructor input "
            <> show (pName mpParam)
            <> " is ambiguous, it is both provided for the create function and output by the create function"
        (True, Nothing, Just _) ->
          throw
            $ "Destructor input "
            <> show (pName mpParam)
            <> " is ambiguous, it is both provided for the create function and output (in a vector) by the create function"
  pure Bracket { .. }

renderBracket
  :: (HasErr r, HasRenderParams r, HasRenderedNames r, HasSpecInfo r)
  => (Text -> Text)
  -- ^ Render param name
  -> Bracket
  -> Sem r (CName, CName, RenderElement)
  -- ^ (create, with, render element)
renderBracket paramName b@Bracket {..} =
  let arguments = nubOrd (bCreateArguments ++ bDestroyArguments)
  in
    fmap (bCreate, bWrapperName, )
    . genRe ("bracket " <> unCName bWrapperName)
    $ do
        RenderParams {..} <- input
        let create      = mkFunName bCreate
            destroy     = mkFunName bDestroy
            wrapperName = mkFunName bWrapperName
        tellExport (ETerm wrapperName)

        --
        -- Getting the bracket type
        --
        argHsTypes <- traverseV
          (   note "argument type has no representation in a negative position"
          <=< schemeTypeNegative
          )
          [ t | Provided _ t <- arguments ]
        let argHsVars = [ pretty (paramName v) | Provided v _ <- arguments ]
        innerHsType <- do
          ts <- traverse
            (   note "Inner type has no representation in a negative position"
            <=< schemeTypeNegative
            )
            bInnerTypes
          pure $ foldl' (:@) (TupleT (length ts)) ts
        let noDestructorResource = not (any isResource bDestroyArguments)
            noResource           = null bInnerTypes && noDestructorResource
            cont                 = if noResource
              then ConT ''IO :@ VarT (mkName "r")
              else innerHsType ~> ConT ''IO :@ VarT (mkName "r")
            wrapperType = foldr (~>)
                                (ConT ''IO :@ VarT (mkName "r"))
                                (argHsTypes ++ [cont])
        constrainedType <- constrainStructVariables wrapperType
        wrapperTDoc     <- renderType constrainedType
        bracketDoc      <- if noResource
          then do
            tellImport 'Control.Exception.bracket_
            pure "bracket_"
          else do
            tellImport 'Control.Exception.bracket
            pure "bracket"

        --
        -- The actual function
        --
        createCall  <- renderCreate paramName b
        destroyCall <- renderDestroy paramName b
        tellDoc $ vsep
          [ comment
            (T.unlines
              (  [ "A safe wrapper for '"
                   <> unName create
                   <> "' and '"
                   <> unName destroy
                   <> "' using '"
                   <> bracketDoc
                   <> "'"
                 ]
              <> bool
                   [ ""
                   , "The allocated value must not be returned from the provided computation"
                   ]
                   []
                   noResource
              )
            )
          , pretty wrapperName <+> "::" <+> wrapperTDoc
          , pretty wrapperName <+> sep argHsVars <+> "=" <> line <> indent
            2
            (pretty bracketDoc <> line <> indent
              2
              (vsep [parens createCall, parens destroyCall])
            )
          ]

renderCreate
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasRenderedNames r
     , HasRenderElem r
     )
  => (Text -> Text)
  -> Bracket
  -> Sem r (Doc ())
renderCreate paramName Bracket {..} = do
  RenderParams {..} <- input
  let create = mkFunName bCreate
  createArgVars <- forV bCreateArguments $ \case
    Provided v _ -> pure (pretty (paramName v))
    Resource _ _ -> throw "Resource used in its own construction"
    -- Would be a bit weird to hit this, but nothing unhandleable
    Member   _ _ -> throw "TODO: Member used during construction"
  tellImport create
  pure $ pretty create <+> sep createArgVars

renderDestroy
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasRenderedNames r
     , HasRenderElem r
     )
  => (Text -> Text)
  -> Bracket
  -> Sem r (Doc ())
renderDestroy paramName Bracket {..} = do
  RenderParams {..} <- input
  let destroy              = mkFunName bDestroy
      noDestructorResource = not . any isResource $ bDestroyArguments
      noResource           = null bInnerTypes && noDestructorResource
      usedResourceIndices  = [ n | Resource _ n <- bDestroyArguments ]
      resourcePattern      = case length bInnerTypes of
        n -> tupled
          [ if i `elem` usedResourceIndices then "o" <> show i else "_"
          | i <- [0 .. n - 1]
          ]
  destroyArgVars <- forV bDestroyArguments $ \case
    Provided v                _ -> pure (pretty (paramName v), Nothing)
    Resource IdentityResource n -> pure ("o" <> show n, Nothing)
    Resource ElemResource n ->
      let v = "o" <> show n in pure (v <> "Elem", Just v)
    Member sibling member -> do
      let correctSibling = \case
            Provided n s | n == sibling -> Just s
            _                           -> Nothing
      case mapMaybe correctSibling bCreateArguments of
        []  -> throw $ "Unable to find sibling " <> sibling
        [s] -> schemeTypeNegative s >>= \case
          Nothing ->
            throw
              $  "Unable to get type for sibling member "
              <> sibling
              <> "::"
              <> member
          Just t -> do
            tDoc <- renderType t
            pure
              ( parens
                (pretty member <+> parens (pretty sibling <+> "::" <+> tDoc))
              , Nothing
              )
        _ -> throw $ "Found multiple siblings with the same name " <> sibling
  let (appVars, catMaybes -> toTraverse) = unzip destroyArgVars
  tellImport destroy
  unless (null toTraverse) (tellImport 'traverse_)
  when (length toTraverse >= 2)
    $ throw
        "TODO: zipping resource vectors, at the moment this will destroy according to the cartesian product of the destructor resource vectors, which given the way this library marshals commands is almost certainly not what you want"
  let withTraversals call = \case
        []     -> call
        x : xs -> withTraversals
          ("traverse_" <+> parens ("\\" <> x <> "Elem" <+> "->" <+> call) <+> x)
          xs
      callDestructor =
        (if noResource then emptyDoc else "\\" <> resourcePattern <+> "-> ")
          <> withTraversals (pretty destroy <+> sep appVars) toTraverse
      traverseDestroy = "traverse" <+> parens callDestructor
  pure $ bool callDestructor traverseDestroy bDestroyIndividually
