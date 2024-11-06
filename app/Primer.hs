{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Primer (start) where

import Foreword

import Control.Monad.Extra (eitherM)
import Control.Monad.Fresh (MonadFresh (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data (..))
import Data.Generics.Uniplate.Data (children)
import Data.Map qualified as Map
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import GHC.Base (error)
import Layout
import Miso
import Optics hiding (view)
import Optics.State.Operators ((<<%=), (?=))
import Primer.App
import Primer.Core hiding (App)
import Primer.Core qualified as Primer
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Def (ASTDef (..), Def (..), astDefExpr)
import Primer.Module (Module (moduleDefs, moduleName))
import Primer.Name (NameCounter, unName)
import Primer.Typecheck (ExprT, TypeError, check)

start :: JSM ()
start =
    startAppWithSavedState
        App
            { model = Model{expr = mapExpr, selection = Nothing}
            , update = updateModel
            , view = viewModel
            , subs = []
            , events = defaultEvents
            , initialAction = StartApp
            , mountPoint = Nothing
            , logLevel = Off
            }
  where
    -- TODO we display a single hardcoded expression, for the sake of demonstration
    mapExpr =
        either (error . ("Prelude.map failed to typecheck: " <>) . show) identity
            . tcBasicProg p
            $ fromMaybe (error "prog doesn't contain Prelude.map") do
                m <- find ((== mkSimpleModuleName "Prelude") . moduleName) $ progImports p
                DefAST d <- Map.lookup "map" $ moduleDefs m
                pure d
      where
        (p, _, _) = newProg

data Model = Model
    { expr :: ExprT -- We typecheck everything up front so that we can use `ExprT`, guaranteeing existence of metadata.
    , selection :: Maybe NodeSelectionT -- TODO once we move beyond one-tree prototype, we'll need to generalise this
    }
    deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Action
    = StartApp
    | SelectNode NodeSelectionT
    | UnselectableNodeClicked
    deriving (Eq, Show)

updateModel :: Action -> Model -> Effect Action Model
updateModel =
    fromTransition . \case
        StartApp -> pure ()
        SelectNode sel -> #selection ?= sel
        UnselectableNodeClicked -> pure ()

viewModel :: Model -> View Action
viewModel Model{..} =
    div_ []
        $ [ "Primer"
          , div_
                [ style_
                    [ ("display", "grid")
                    , ("grid-template-columns", "1fr 1fr")
                    , ("justify-items", "center")
                    ]
                ]
                [ SelectNode . NodeSelection BodyNode <$> viewTree (viewTreeExpr expr)
                , case selection of
                    Nothing -> "no selection"
                    Just s ->
                        UnselectableNodeClicked <$ case nodeSelectionType s of
                            Left t -> viewTree $ viewTreeType t
                            Right (Left t) -> viewTree $ viewTreeKind t
                            -- TODO this isn't really correct - kinds in Primer don't have kinds
                            Right (Right ()) -> viewTree $ viewTreeKind $ KType ()
                ]
          ]

viewTreeExpr ::
    (Data a, Data b, Data c) =>
    Expr' a b c ->
    Tree.Tree (View (TermMeta' a b c))
viewTreeExpr e = Tree.Node viewNode viewChildren
  where
    viewNode = div_
        [onClick $ Left $ e ^. _exprMetaLens]
        $ pure case e of
            Hole{} -> textNode "⚠️"
            EmptyHole{} -> textNode "?"
            Ann{} -> textNode ":"
            Primer.App{} -> textNode "←"
            APP{} -> textNode "←"
            Con _ c _ -> textNode $ gname c
            Lam _ v _ -> textNode $ "λ" <> lname v
            LAM _ v _ -> textNode $ "Λ" <> lname v
            Var _ (GlobalVarRef v) -> textNode $ gname v
            Var _ (LocalVarRef v) -> textNode $ lname v
            Let{} -> textNode "let"
            LetType{} -> textNode "let type"
            Letrec{} -> textNode "let rec"
            PrimCon _ c -> case c of
                PrimChar c' -> textNode $ show c'
                PrimInt n -> textNode $ show n
                PrimAnimation a -> img_ [src_ ("data:img/gif;base64," <> a)]
            Case{} -> textNode "match"
    viewChildren = case e of
        Case _ scrut branches fb ->
            viewTreeExpr scrut
                : ( branches
                        <&> \(CaseBranch p bindings r) ->
                            Tree.Node
                                ( div_
                                    []
                                    $ ( text case p of
                                            PatCon c -> gname c
                                            PatPrim c -> case c of
                                                PrimChar c' -> show c'
                                                PrimInt n -> show n
                                                -- This branch should never actually be triggered,
                                                -- since such programs can't be constructed.
                                                PrimAnimation _ -> "error: can't pattern match on animation"
                                      )
                                    : concatMap (\(Bind _ v) -> [text " ", text $ lname v]) bindings
                                )
                                [viewTreeExpr r]
                  )
                    <> case fb of
                        CaseExhaustive -> []
                        CaseFallback r -> [Tree.Node (textNode "_") [viewTreeExpr r]]
        _ -> map viewTreeType (e ^.. typesInExpr) <> map viewTreeExpr (children e)
    textNode t = div_ [] [text t]

viewTreeType ::
    (Data b, Data c) =>
    Type' b c ->
    Tree.Tree (View (TermMeta' a b c))
viewTreeType t = Tree.Node viewNode viewChildren
  where
    viewNode = div_
        [onClick $ Right $ Left $ t ^. _typeMetaLens]
        $ pure case t of
            TEmptyHole{} -> text "?"
            THole{} -> text "⚠️"
            TCon _ c -> text $ gname c
            TFun{} -> text "→"
            TVar _ v -> text $ lname v
            TApp{} -> text "←"
            TForall _ v _ _ -> text $ "∀" <> lname v
            TLet{} -> text "let"
    viewChildren = map viewTreeKind (t ^.. kindsInType) <> map viewTreeType (children t)

viewTreeKind :: (Data c) => Kind' c -> Tree.Tree (View (TermMeta' a b c))
viewTreeKind k = Tree.Node viewNode viewChildren
  where
    viewNode = div_
        [onClick $ Right $ Right $ k ^. _kindMetaLens]
        $ pure case k of
            KType{} -> text "*"
            KFun{} -> text "→"
            KHole{} -> text "?"
    viewChildren = map viewTreeKind (children k)

gname :: GlobalName k -> Text
gname = unName . baseName
lname :: LocalName k -> Text
lname = unName . unLocalName

viewTree :: Tree (View action) -> View action
viewTree =
    -- TODO consider taking top-level attributes and `Tree ([Attribute action] -> View action)`
    -- in order to avoid so many nested `div`s
    div_ []
        . map
            ( \(v, P2 x y) ->
                div_
                    [ style_
                        [ ("position", "absolute")
                        , ("transform", "translate(" <> show x <> "px, " <> show -y <> "px)")
                        ]
                    ]
                    [v]
            )
        . toList
        -- TODO these values are quite arbitrary and will require tweaking
        . symmLayout' @Double (def & slHSep .~ 30 & slVSep .~ 40)

-- TODO upstream: https://github.com/dmjio/miso/issues/749
startAppWithSavedState :: forall model action. (Eq model, FromJSON model, ToJSON model) => Miso.App model action -> JSM ()
startAppWithSavedState app = do
    savedModel <-
        eitherM (\e -> putStrLn ("saved state not loaded: " <> e) >> pure Nothing) (pure . Just)
            $ getLocalStorage storageKey
    startApp
        app
            { model = fromMaybe app.model savedModel
            , update = \case
                Nothing -> pure
                Just a -> \m -> do
                    m' <- first Just $ app.update a m
                    m' <# do
                        setLocalStorage storageKey m'
                        pure Nothing
            , subs = mapSub Just <$> app.subs
            , view = fmap Just . app.view
            , initialAction = Just app.initialAction
            }
  where
    storageKey = "miso-app-state"

-- `tcWholeProg` throws away information by not returning a prog containing `ExprT`s
-- we use `check` since, for whatever reason, `synth` deletes the case branches in `map`
tcBasicProg :: Prog -> ASTDef -> Either TypeError ExprT
tcBasicProg p ASTDef{..} =
    runTC
        . flip (runReaderT @_ @(M TypeError)) (progCxt p)
        $ check (forgetTypeMetadata astDefType) astDefExpr

-- TODO this is all basically copied from unexposed parts of Primer library - find a way to expose
newtype M e a = M {unM :: StateT (ID, NameCounter) (Except e) a}
    deriving newtype (Functor, Applicative, Monad, MonadError e)
instance MonadFresh ID (M e) where
    fresh = M $ _1 <<%= succ
instance MonadFresh NameCounter (M e) where
    fresh = M $ _2 <<%= succ
runTC :: M e a -> Either e a
runTC = runExcept . flip evalStateT (0, toEnum 0) . (.unM)

-- analogous with `ExprT`/`TypeT`
-- type TypeT = Type' TypeMetaT KindMetaT
-- type KindT = Kind' KindMetaT
-- type SelectionT = Selection' (Either ExprMetaT (Either TypeMetaT KindMetaT))
type TermMeta' a b c = Either a (Either b c) -- TODO make this a proper sum type
type NodeSelectionT = NodeSelection (TermMeta' ExprMetaT TypeMetaT KindMetaT)
type ExprMetaT = Meta TypeCache
type TypeMetaT = Meta (Kind' ())
type KindMetaT = Meta ()

-- analogous to `typesInExpr`
kindsInType :: AffineTraversal' (Type' a b) (Kind' b)
kindsInType = atraversalVL $ \point f -> \case
    TForall m a k t -> flip (TForall m a) t <$> f k
    e -> point e

-- TODO generalise to full selections and DRY with `getSelectionTypeOrKind` from `primer-api`
nodeSelectionType :: NodeSelectionT -> Either (Type' () ()) (Either (Kind' ()) ())
nodeSelectionType =
    bimap
        (getAPIType . (^. _type))
        (bimap (^. _type) (^. _type))
        . (.meta)
  where
    -- copied directly from innards of `getSelectionTypeOrKind`
    getAPIType :: TypeCache -> Type' () ()
    getAPIType = \case
        TCSynthed t -> t
        TCChkedAt t -> t
        TCEmb (TCBoth{tcSynthed, tcChkedAt})
            -- If this node is an embedding, we have a choice of two types to report.
            -- We choose the one that is not a hole;
            | isHole tcSynthed -> tcChkedAt
            | isHole tcChkedAt -> tcSynthed
            -- if neither is a hole (in which case the two are consistent), we choose the synthed type
            | otherwise -> tcSynthed
      where
        isHole :: Type' a b -> Bool
        isHole = \case
            THole{} -> True
            TEmptyHole{} -> True
            _ -> False
