module Bracket where

import           Data.List                      ( findIndex )
import           Data.List.Extra                ( elemIndex
                                                , nubOrd
                                                )
import qualified Data.Text.Extra               as T
import           Prettyprinter
                                         hiding ( brackets
                                                , plural
                                                )
import           Language.Haskell.TH            ( mkName )
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( Handle
                                                , Type
                                                )

import           Error
import           Haskell                       as H
import           Marshal.Command
import           Marshal.Scheme
import           Render.Command
import           Render.Element
import           Render.Names
import           Render.Scheme
import           Render.SpecInfo
import           Render.Utils
import           Spec.Parse

data Bracket = Bracket
  { bInnerTypes          :: [MarshalScheme Parameter]
  , bWrapperName         :: CName
  , bCreate              :: CName
  , bDestroy             :: CName
  , bCreateArguments     :: [Argument]
  , bDestroyArguments    :: [Argument]
  , bDestroyIndividually :: DestroyIndividually
  , bBracketType         :: BracketType
  , bDestroyReturnTypes  :: [Type]
  }
  deriving Show

data DestroyIndividually = DoDestroyIndividually | DoNotDestroyIndividually
  deriving Show

data BracketType
  = BracketBookend
  -- ^ This bracket takes an action to perform between a begin and end action
  -- with no exception handling.
  | BracketCPS
  -- ^ This bracket takes a continuation which is given a matching pair of
  -- construct and destroy actions
  deriving Show

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
  Resource _ _ -> True
  _            -> False

-- | Try to generate a bracket automatically from the types of a pair of
-- marshaled commands.
autoBracket
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => BracketType
  -> MarshaledCommand
  -- ^ Begin
  -> MarshaledCommand
  -- ^ End
  -> CName
  -- ^ Bracketing function name
  -> Sem r Bracket
autoBracket bBracketType create destroy with = do
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
      bDestroyIndividually = DoNotDestroyIndividually
      destroyNegativeParams =
        [ mp
        | mp@MarshaledParam {..} <- toList (mcParams destroy)
        , not (isElided mpScheme)
        , isNegative mpScheme
        ]
  bDestroyReturnTypes <- toList <$> marshaledCommandReturnTypes False destroy
  bDestroyArguments   <- forV destroyNegativeParams $ \MarshaledParam {..} ->
    let
      provided             = Provided (unCName (pName mpParam)) mpScheme
      providedForCreate    = provided `elem` bCreateArguments
      providedByCreate     = mpScheme `elemIndex` bInnerTypes
      providedByCreateElem = findIndex
        (\case
          Vector _ s | s == mpScheme -> True
          _                          -> False
        )
        bInnerTypes
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
          <=< schemeTypeNegativeIgnoreContext
          )
          [ t | Provided _ t <- arguments ]
        let argHsVars =
              [ pretty (paramName v) | Provided v _ <- arguments ]
              <> case bBracketType of
                   BracketCPS     -> ["b"]
                   BracketBookend -> ["a"]
        innerHsType <- do
          ts <- traverse
            (   note "Inner type has no representation in a negative position"
            <=< schemeTypeNegativeIgnoreContext
            )
            bInnerTypes
          pure $ case ts of
            [x] -> x
            _   -> foldl' (:@) (TupleT (length ts)) ts
        let noDestructorResource = not (any isResource bDestroyArguments)
            noResource           = null bInnerTypes && noDestructorResource
            ioVar                = VarT (mkName "io")
            rVar                 = VarT (mkName "r")
            rTy                  = case bDestroyReturnTypes of
              [] -> rVar
              ts -> foldl' (:@) (TupleT (length ts + 1)) (ts <> [rVar])
            userParamType = case bBracketType of
              BracketCPS -> if noResource
                then ioVar :@ innerHsType ~> (ioVar :@ ConT ''()) ~> rVar
                else
                  ioVar
                  :@ innerHsType
                  ~> (innerHsType ~> ioVar :@ ConT ''())
                  ~> rVar
              BracketBookend -> if noResource
                then ioVar :@ rVar
                else innerHsType ~> ioVar :@ rVar
            returnType = case bBracketType of
              BracketCPS     -> rTy
              BracketBookend -> ioVar :@ rTy

            wrapperType = foldr (~>) returnType (argHsTypes <> [userParamType])
            bracketSuffix = bool "" "_" noResource
        constrainedType <- addConstraints [ConT ''MonadIO :@ ioVar]
          <$> constrainStructVariables wrapperType
        wrapperTDoc <- renderType constrainedType

        --
        -- The actual function
        --
        createCall  <- renderCreate paramName b
        destroyCall <- renderDestroy paramName b
        bracketRHS  <- case bBracketType of
          BracketCPS -> case bDestroyReturnTypes of
            [] -> pure $ "b" <+> indent
              0
              (vsep [parens createCall, parens destroyCall])
            _ ->
              throw "TODO: Handle destructor return values with CPS brackets"
          BracketBookend
            | noResource -> pure $ case bDestroyReturnTypes of
              [] -> parens createCall <+> "*> a <*" <+> parens destroyCall
              _  -> doBlock
                [createCall, "r <- a", "d <-" <+> destroyCall, "pure (d, r)"]
            | otherwise -> pure $ case bDestroyReturnTypes of
              [] -> doBlock
                [ "x <-" <+> createCall
                , "r <- a x"
                , parens destroyCall <+> "x"
                , "pure r"
                ]
              _ -> doBlock
                [ "x <-" <+> createCall
                , "r <- a x"
                , "d <-" <+> parens destroyCall <+> "x"
                , "pure (d, r)"
                ]

        let bracketBody =
              pretty wrapperName
                <+> sep argHsVars
                <+> "="
                <>  line
                <>  indent 2 bracketRHS

        tellDoc $ vsep
          [ comment
            (T.unlines $ case bBracketType of
              BracketCPS ->
                [ "A convenience wrapper to make a compatible pair of calls to '"
                  <> unName create
                  <> "' and '"
                  <> unName destroy
                  <> "'"
                  , ""
                  , "To ensure that '"
                  <> unName destroy
                  <> "' is always called: pass 'Control.Exception.bracket"
                  <> bracketSuffix
                  <> "' (or the allocate function from your favourite resource management library) as the last argument."
                  , "To just extract the pair pass '(,)' as the last argument."
                  , ""
                  ]
                  <> [ "Note that there is no inner resource" | noResource ]
              BracketBookend ->
                [ "This function will call the supplied action between calls to '"
                  <> unName create
                  <> "' and '"
                  <> unName destroy
                  <> "'"
                , ""
                , "Note that '"
                  <> unName destroy
                  <> "' is *not* called if an exception is thrown by the inner action."
                ]
            )
          , pretty wrapperName <+> "::" <+> wrapperTDoc
          , bracketBody
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
        [s] -> schemeTypeNegativeIgnoreContext s >>= \case
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
  pure $ case bDestroyIndividually of
    DoDestroyIndividually    -> traverseDestroy
    DoNotDestroyIndividually -> callDestructor
