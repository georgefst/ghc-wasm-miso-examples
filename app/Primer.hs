{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Primer (start) where

import Foreword

import Colours
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
import Primer.Name (Name, NameCounter, unName)
import Primer.Typecheck (ExprT, TypeError, check, checkKind)

start :: JSM ()
start =
    startAppWithSavedState
        App
            { model = Model{def = mapDef, selection = Nothing}
            , update = updateModel
            , view = viewModel
            , subs = []
            , events = defaultEvents
            , initialAction = NoOp "start"
            , mountPoint = Nothing
            , logLevel = Off
            }
  where
    -- TODO we display a single hardcoded expression, for the sake of demonstration
    mapDef =
        either (error . ("Prelude.map failed to typecheck: " <>) . show) identity
            . tcBasicProg p
            $ fromMaybe (error "prog doesn't contain Prelude.map") do
                m <- find ((== mkSimpleModuleName "Prelude") . moduleName) $ progImports p
                DefAST d <- Map.lookup "map" $ moduleDefs m
                pure d
      where
        (p, _, _) = newProg

data Model = Model
    { def :: ASTDefT -- We typecheck everything up front so that we can use `ExprT`, guaranteeing existence of metadata.
    , selection :: Maybe NodeSelectionT -- TODO once we move beyond one-tree prototype, we'll need to generalise this
    }
    deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Action
    = NoOp Text -- For situations where Miso requires an action, but we don't actually want to do anything.
    | SelectNode NodeSelectionT
    deriving (Eq, Show)

updateModel :: Action -> Model -> Effect Action Model
updateModel =
    fromTransition . \case
        NoOp _ -> pure ()
        SelectNode sel -> #selection ?= sel

viewModel :: Model -> View Action
viewModel Model{..} =
    div_ [] $
        [ div_
            [ style_
                [ ("display", "grid")
                , ("grid-template-columns", "1fr 1fr 1fr")
                , ("justify-items", "center")
                ]
            ]
            [ SelectNode . NodeSelection SigNode <$> viewTree (viewTreeType def.sig)
            , SelectNode . NodeSelection BodyNode <$> viewTree (viewTreeExpr def.expr)
            , case selection of
                Nothing -> "no selection"
                Just s ->
                    NoOp "clicked non-interactive node" <$ case nodeSelectionType s of
                        Left t -> viewTree $ viewTreeType t
                        Right (Left t) -> viewTree $ viewTreeKind t
                        -- TODO this isn't really correct - kinds in Primer don't have kinds
                        Right (Right ()) -> viewTree $ viewTreeKind $ KType ()
            ]
        ]

-- | A renderable node with dimensions.
data NodeView action = NodeView
    { view :: View action
    , width :: Int
    , height :: Int
    }
    deriving (Generic)

data NodeViewData
    = SyntaxNode {wide :: Bool, color :: Text, text :: Text}
    | HoleNode {empty :: Bool}
    | PrimNode PrimCon
    | ConNode {name :: Name, scope :: ModuleName}
    | VarNode {name :: Name, mscope :: Maybe ModuleName} -- TODO we should be able to re-use the name `scope`: https://github.com/ghc-proposals/ghc-proposals/pull/535#issuecomment-1694388075

mkNodeView :: NodeViewData -> Map Text Text -> Map Text Text -> NodeView action
mkNodeView opts extraOuterStyles extraInnerStyles =
    NodeView
        { width
        , height
        , view = case opts of
            PrimNode (PrimAnimation animation) ->
                img_
                    [ src_ ("data:img/gif;base64," <> animation)
                    , style_ $
                        [ ("width", show width <> "px")
                        , ("height", show height <> "px")
                        ]
                            <> extraOuterStyles
                            <> extraInnerStyles
                    ]
            _ ->
                div_
                    [ style_ $
                        [ ("display", "flex")
                        , ("justify-content", "center")
                        , ("align-items", "center")
                        , ("border-style", "solid")
                        , ("box-sizing", "border-box")
                        , ("border-color", borderColor)
                        , ("background-color", backgroundColor)
                        , ("color", fontColor)
                        , ("width", show width <> "px")
                        , ("height", show height <> "px")
                        , ("border-width", ".25rem")
                        , ("padding-left", ".25rem")
                        , ("padding-right", ".25rem")
                        ]
                            <> case opts of
                                HoleNode{} -> [("font-style", "italic")]
                                _ -> []
                            <> extraOuterStyles
                    ]
                    [ div_
                        [ style_ $
                            [ ("overflow", "hidden")
                            , ("text-overflow", "ellipsis")
                            , ("white-space", "nowrap")
                            ]
                                <> extraInnerStyles
                        ]
                        [ text case opts of
                            SyntaxNode{text} -> text
                            HoleNode{empty} -> if empty then "?" else "⚠️"
                            PrimNode pc -> case pc of
                                PrimChar c' -> show c'
                                PrimInt n -> show n
                            ConNode{name} -> unName name
                            VarNode{name} -> unName name
                        ]
                    ]
              where
                borderColor = case opts of
                    SyntaxNode{..} -> color
                    HoleNode{} -> redTertiary
                    PrimNode{} -> greenPrimary
                    ConNode{} -> greenPrimary
                    VarNode{} -> blueQuaternary
                (backgroundColor, fontColor) = case opts of
                    SyntaxNode{..} -> (color, whitePrimary)
                    _ -> (whitePrimary, bluePrimary)
        }
  where
    width = case opts of
        SyntaxNode{wide = False} -> height
        _ -> 80
    height = 35

viewTreeExpr ::
    (Data a, Data b, Data c) =>
    Expr' a b c ->
    Tree.Tree (NodeView (TermMeta' a b c))
viewTreeExpr e =
    Tree.Node
        ( over #view (div_ [onClick $ Left $ e ^. _exprMetaLens] . pure) $
            mkNodeView
                viewNode
                -- Curved nodes to indicate value-level expressions.
                [("border-radius", "1.5rem")]
                []
        )
        viewChildren
  where
    viewNode = case e of
        Hole{} -> HoleNode{empty = False}
        EmptyHole{} -> HoleNode{empty = False}
        Ann{} -> SyntaxNode False blackPrimary ":"
        Primer.App{} -> SyntaxNode False blueTertiary "←"
        APP{} -> SyntaxNode False blueTertiary "←"
        Con _ c _ -> ConNode{name = baseName c, scope = qualifiedModule c}
        Lam{} -> SyntaxNode False bluePrimary "λ"
        LAM{} -> SyntaxNode False blueSecondary "Λ"
        Var _ (GlobalVarRef v) -> VarNode{name = baseName v, mscope = Just $ qualifiedModule v}
        Var _ (LocalVarRef v) -> VarNode{name = unLocalName v, mscope = Nothing}
        Let{} -> SyntaxNode False blueQuaternary "let"
        LetType{} -> SyntaxNode False blueQuaternary "let type"
        Letrec{} -> SyntaxNode False blueQuaternary "let rec"
        PrimCon _ c -> PrimNode c
        Case{} -> SyntaxNode True yellowPrimary "match"
    viewChildren = case e of
        Case _ scrut branches fb ->
            viewTreeExpr scrut
                : ( branches
                        <&> \(CaseBranch p bindings r) ->
                            Tree.Node
                                ( NodeView
                                    { width = 25 -- TODO just an approximation until we render patterns properly
                                    , height = 25 -- TODO ditto
                                    , view =
                                        div_
                                            []
                                            $ ( text case p of
                                                    PatCon c -> unName $ baseName c
                                                    PatPrim c -> case c of
                                                        PrimChar c' -> show c'
                                                        PrimInt n -> show n
                                                        -- This branch should never actually be triggered,
                                                        -- since such programs can't be constructed.
                                                        PrimAnimation _ -> "error: can't pattern match on animation"
                                              )
                                                : concatMap (\(Bind _ v) -> [text " ", text $ unName $ unLocalName v]) bindings
                                    }
                                )
                                [viewTreeExpr r]
                  )
                    <> case fb of
                        CaseExhaustive -> []
                        CaseFallback r -> [Tree.Node (mkNodeView (SyntaxNode False yellowPrimary "_") [] []) [viewTreeExpr r]]
        _ ->
            map
                (\name -> Tree.Node (mkNodeView VarNode{name, mscope = Nothing} [] []) [])
                (e ^.. typeBindingsInExpr % to unLocalName <> e ^.. bindingsInExpr % to unLocalName)
                <> map viewTreeType (e ^.. typesInExpr)
                <> map viewTreeExpr (children e)

viewTreeType ::
    (Data b, Data c) =>
    Type' b c ->
    (Tree.Tree (NodeView (TermMeta' a b c)))
viewTreeType t =
    Tree.Node
        ( over #view (div_ [onClick $ Right $ Left $ t ^. _typeMetaLens] . pure) $
            mkNodeView viewNode [] []
        )
        viewChildren
  where
    viewNode = case t of
        TEmptyHole{} -> HoleNode{empty = True}
        THole{} -> HoleNode{empty = True}
        TCon _ c -> ConNode{name = baseName c, scope = qualifiedModule c}
        TFun{} -> SyntaxNode False bluePrimary "→"
        TVar _ v -> VarNode{name = unLocalName v, mscope = Nothing}
        TApp{} -> SyntaxNode False blueTertiary "←"
        TForall{} -> SyntaxNode False blueSecondary "∀"
        TLet{} -> SyntaxNode False blueQuaternary "let"
    viewChildren =
        map
            (\name -> Tree.Node (mkNodeView VarNode{name, mscope = Nothing} [] []) [])
            (t ^.. bindingsInType % to unLocalName)
            <> map viewTreeKind (t ^.. kindsInType)
            <> map viewTreeType (children t)

viewTreeKind :: (Data c) => Kind' c -> Tree.Tree (NodeView (TermMeta' a b c))
viewTreeKind k =
    Tree.Node
        ( over #view (div_ [onClick $ Right $ Right $ k ^. _kindMetaLens] . pure) $
            mkNodeView
                viewNode
                -- Rotate to indicate kind.
                -- We then scale by (1 + 1/√2)/2 so that dimensions used for layout are a good approximation.
                [("transform", "rotate(45deg) scale(0.854)")]
                -- Rotate the content back to it's correct orientation.
                [("transform", "rotate(-45deg)")]
        )
        viewChildren
  where
    viewNode = case k of
        KHole{} -> HoleNode{empty = True}
        KType{} -> SyntaxNode False greenPrimary "*"
        KFun{} -> SyntaxNode False bluePrimary "→"
    viewChildren = map viewTreeKind (children k)

viewTree :: Tree (NodeView action) -> View action
viewTree t@(Tree.Node NodeView{width = rootWidth, height = rootHeight} _) =
    -- TODO consider taking top-level attributes and `Tree ([Attribute action] -> View action)`
    -- in order to avoid so many nested `div`s
    div_ [style_ [("padding", show (padding / 2) <> "px")]]
        . map
            ( \(node, P2 x y) ->
                div_
                    [ style_
                        [ ("position", "absolute")
                        ,
                            ( "transform"
                            , "translate("
                                <> show (x - fromIntegral node.width / 2 - fromIntegral rootWidth / 2)
                                <> "px,"
                                <> show (-y - fromIntegral node.height + fromIntegral rootHeight)
                                <> "px)"
                            )
                        ]
                    ]
                    [node.view]
            )
        . toList
        $ symmLayout' @Double
            ( def
                & (slHSep .~ padding)
                & (slVSep .~ padding)
                & (slWidth .~ \node -> (-(fromIntegral node.width / 2), fromIntegral node.width / 2))
                & (slHeight .~ \node -> (0, fromIntegral node.height))
            )
            t
  where
    padding = 20

-- TODO upstream: https://github.com/dmjio/miso/issues/749
startAppWithSavedState :: forall model action. (Eq model, FromJSON model, ToJSON model) => Miso.App model action -> JSM ()
startAppWithSavedState app = do
    savedModel <-
        eitherM (\e -> putStrLn ("saved state not loaded: " <> e) >> pure Nothing) (pure . Just) $
            getLocalStorage storageKey
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
tcBasicProg :: Prog -> ASTDef -> Either TypeError ASTDefT
tcBasicProg p ASTDef{..} =
    runTC
        . flip (runReaderT @_ @(M TypeError)) (progCxt p)
        $ ASTDefT
            <$> (check (forgetTypeMetadata astDefType) astDefExpr)
            <*> (checkKind (KType ()) astDefType)

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
-- type KindT = Kind' KindMetaT
-- type SelectionT = Selection' (Either ExprMetaT (Either TypeMetaT KindMetaT))
type TypeT = Type' TypeMetaT KindMetaT -- TODO actually exists in Primer lib but is hidden
type TermMeta' a b c = Either a (Either b c) -- TODO make this a proper sum type
type NodeSelectionT = NodeSelection (TermMeta' ExprMetaT TypeMetaT KindMetaT)
type ExprMetaT = Meta TypeCache
type TypeMetaT = Meta (Kind' ())
type KindMetaT = Meta ()
data ASTDefT = ASTDefT {expr :: ExprT, sig :: TypeT} -- TODO parameterise `ASTDef` etc.?
    deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

-- analogous to `typesInExpr`
kindsInType :: AffineTraversal' (Type' a b) (Kind' b)
kindsInType = atraversalVL $ \point f -> \case
    TForall m a k t -> flip (TForall m a) t <$> f k
    e -> point e

-- TODO if we had first-class bindings, we could probably implement all of these generically
bindingsInExpr :: AffineTraversal' (Expr' a b c) LVarName
bindingsInExpr = atraversalVL $ \point f -> \case
    Lam m v e -> f v <&> \v' -> Lam m v' e
    Let m v e1 e2 -> f v <&> \v' -> Let m v' e1 e2
    Letrec m v e1 t e2 -> f v <&> \v' -> Letrec m v' e1 t e2
    e -> point e
typeBindingsInExpr :: AffineTraversal' (Expr' a b c) TyVarName
typeBindingsInExpr = atraversalVL $ \point f -> \case
    LAM m v e -> f v <&> \v' -> LAM m v' e
    LetType m v t e -> f v <&> \v' -> LetType m v' t e
    e -> point e
bindingsInType :: AffineTraversal' (Type' a b) TyVarName
bindingsInType = atraversalVL $ \point f -> \case
    TForall m v k t -> f v <&> \v' -> TForall m v' k t
    TLet m v t1 t2 -> f v <&> \v' -> TLet m v' t1 t2
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
