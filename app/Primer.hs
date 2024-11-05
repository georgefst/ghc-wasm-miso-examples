{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wname-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Primer (start) where

-- TODO some awkward double imports - move Primer stuff to separate module?

import Foreword hiding (conName)

import Control.Monad.Extra (eitherM)
import Control.Monad.Fresh
import Data.Aeson
import Data.Bitraversable (bimapM)
import Data.ByteString (ByteString)
import Data.Data (Data (..), showConstr)
import Data.Either.Extra (eitherToMaybe)
import Data.Function hiding (id)
import Data.Functor ((<&>))
import Data.Generics.Uniplate.Data (para)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Data.Tuple.Extra (firstM)
import Debug.Pretty.Simple
import Language.Javascript.JSaddle hiding ((<#))
import Layout
import Miso
import Optics hiding (view)
import Optics qualified
import Optics.State.Operators ((<<%=), (?=))
import Prettyprinter.Render.Text (putDoc)
import Primer.App
import Primer.Builtins
import Primer.Core hiding (App)
import Primer.Core qualified as Primer
import Primer.Core.DSL hiding (app)
import Primer.Core.DSL qualified as DSL
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Def (ASTDef (..), Def (..), astDefExpr)
import Primer.Module
import Primer.Name (NameCounter, unName)
import Primer.Pretty hiding (prettyPrintExpr, prettyPrintType)
import Primer.Typecheck (Cxt, ExprT, SmartHoles (..), TypeError, check, initialCxt, synth)
import System.Directory (doesFileExist)
import Text.Pretty.Simple
import Prelude (error)

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

data Model = Model
    { expr :: ExprT -- We typecheck everything up front so that we can use `ExprT`, guaranteeing existence of metadata.
    , selection :: Maybe NodeSelectionT -- TODO once we move beyond one-tree prototype, we'll need to generalise this
    }
    deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Action
    = StartApp
    | SelectNode NodeSelectionT
    deriving (Eq, Show)

updateModel :: Action -> Model -> Effect Action Model
updateModel =
    fromTransition . \case
        StartApp -> pure ()
        SelectNode sel -> #selection ?= sel

viewModel :: Model -> View Action
viewModel Model{..} =
    div_ [] $
        [ "Primer \x1f937\x1f3fd\x200d\x2640\xfe0f"
        , br_ []
        , br_ []
        , viewTree
            -- TODO arbitrary height - we should fit to content
            [style_ $ Map.fromList [("height", "400px")]]
            $ viewTreeExpr expr
        , br_ []
        , br_ []
        , div_ [] case selection of
            Nothing -> ["no selection"]
            Just s ->
                [ text $ "selected node ID: " <> (show $ getIDNodeSelection s)
                , br_ []
                , case nodeSelectionType s of
                    Left t -> viewTree [] $ viewTreeType t
                    Right (Left t) -> viewTree [] $ viewTreeKind t
                    -- TODO display something sensible here
                    -- in the React frontend, I don't think we ever worked out what...
                    Right (Right ()) -> "displaying types/kinds/kind1s of kinds is not yet supported"
                ]
        ]

-- TODO improve and upstream this: https://github.com/dmjio/miso/issues/749
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

onClickExpr :: Expr' (Meta TypeCache) b c -> Attribute Action
onClickExpr e = onClick $ SelectNode $ NodeSelection BodyNode $ Left $ e ^. _exprMetaLens
onClickType :: TypeT -> Attribute Action
onClickType t = onClick $ SelectNode $ NodeSelection BodyNode $ Right $ Left $ t ^. _typeMetaLens

viewTreeExpr :: ExprT -> Tree.Tree (View Action)
viewTreeExpr = para \e cs ->
    let lname = unName . unLocalName
        gname = unName . baseName
        exprDiv = div_ . (onClickExpr e :)
        normalNode t = simpleTextNode t $ map viewTreeType (e ^.. typesInExpr) <> cs
        -- TODO this mixes up two things - don't tree-ify and assume `text` in one - see animation prim rendering
        simpleTextNode t = Tree.Node $ exprDiv [] [text t]
     in case e of
            Hole{} -> normalNode "⚠️"
            EmptyHole{} -> normalNode "?"
            Ann{} -> normalNode ":"
            Primer.App{} -> normalNode "←"
            APP{} -> normalNode "←"
            Con _ c _ -> normalNode $ gname c
            Lam _ v _ -> normalNode $ "λ" <> lname v
            LAM _ v _ -> normalNode $ "Λ" <> lname v
            Var _ (GlobalVarRef v) -> normalNode $ gname v
            Var _ (LocalVarRef v) -> normalNode $ lname v
            Let{} -> normalNode "let"
            LetType{} -> normalNode "let type"
            Letrec{} -> normalNode "let rec"
            -- TODO currently rendered in something akin to Vonnegut-style...
            -- boxy would be better but much harder to implement
            -- ideally we'd implement Tidy with a modification to properly handle right-children first
            Case _ scrut bs fb ->
                simpleTextNode "match" $
                    viewTreeExpr scrut
                        : ( bs
                                <&> \(CaseBranch p bs' r) ->
                                    Tree.Node
                                        ( exprDiv
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
                                                : concatMap
                                                    (\v -> [text " ", v])
                                                    (bs' <&> \(Bind _ v) -> text $ lname v)
                                        )
                                        [viewTreeExpr r]
                          )
                            <> case fb of
                                CaseExhaustive -> []
                                CaseFallback r -> [simpleTextNode "_" [viewTreeExpr r]]
            PrimCon _ c -> case c of
                PrimChar c' -> normalNode $ show c'
                PrimInt n -> normalNode $ show n
                PrimAnimation a -> Tree.Node (img_ [src_ $ "data:img/gif;base64," <> a]) []

-- TODO placeholder
-- e -> simpleTextNode $ conName e
viewTreeType :: (TypeMetaThingy a b, Data a, Data b) => Type' a b -> Tree.Tree (View Action)
viewTreeType = para \t cs ->
    Tree.Node (viewTypeNode t) $ map viewTreeKind (t ^.. kindsInType) <> cs
viewTreeKind :: (Data a) => Kind' a -> Tree.Tree (View Action)
viewTreeKind = para $ Tree.Node . viewKindNode

-- TODO inline, simplify and render better - just like exprs
-- viewTypeNode :: TypeT -> View Action
viewTypeNode :: (TypeMetaThingy a b) => Type' a b -> View Action
viewTypeNode t0 = typeMetaThingyClickHandler t0 case t0 of
    TEmptyHole{} -> text "?"
    THole{} -> text "⚠️"
    TCon _ c -> text $ gname c
    TFun{} -> text "→"
    TVar _ v -> text $ lname v
    TApp{} -> text "←"
    TForall _ v _ _ -> text $ "∀" <> lname v
    TLet{} -> text "let"
  where
    -- TODO placeholder
    -- e -> text $ conName e
    lname = unName . unLocalName
    -- TODO show qualified (old frontend never did...)
    gname = unName . baseName
viewKindNode :: (Data a) => Kind' a -> View Action
-- TODO add optional `onClick` handler
viewKindNode = \case
    KType{} -> text "*"
    KFun{} -> text "→"
    -- TODO placeholder
    e -> text $ conName e

viewTree :: [Attribute action] -> Tree (View action) -> View action
viewTree attrs =
    --  uncurry _ . unzip .
    div_ attrs
        . map
            ( \(v, P2 x y) ->
                div_
                    [ style_ $
                        Map.fromList
                            [ ("position", "absolute")
                            ,
                                ( "transform"
                                , -- TODO 200 is a hardcoded guess
                                  -- ideally the algorithm should tell is the dimensions of the whole tree
                                  -- otherwise maybe we can be clever with CSS centering
                                  "translate(" <> show (x + 200) <> "px, " <> show -y <> "px)"
                                )
                            ]
                    ]
                    [v]
            )
        . toList
        -- TODO tweak options
        . symmLayout' @Double (def & slHSep .~ 30 & slVSep .~ 40)

-- rather than this, what we actually want is each node being a div at the same level, not actually nesting the HTML
-- TODO we could use Primer's prettyprinter and adapt `prettyprinter-lucid` to make a `prettyprinter-miso`
-- but we'd probably lose a bit too much information that way - it would be a good POC if the library already existed
-- instead we've copied in `prettyExpr` as a starting point
-- viewExprText :: ExprT -> View Action
-- viewExprText e0 = div_ [onClickExpr e0] $ pure case e0 of
--     Hole _ e -> div_ [] ["{?", viewExprText e, "?}"]
--     EmptyHole _ -> "?"
--     Con _ n tms ->
--         div_ [] $ text (gname n) : map viewExprText tms
--     Var _ v -> case v of
--         GlobalVarRef n -> text $ gname n
--         LocalVarRef n -> text $ lname n
--     Lam _ n e ->
--         div_
--             []
--             [ "λ"
--             , text $ lname n
--             , viewExprText e
--             ]
--     LAM _ n e ->
--         div_
--             []
--             [ "Λ"
--             , text $ lname n
--             , viewExprText e
--             ]
--     Case _ e bs fallback ->
--         div_
--             []
--             $ [ "match"
--               , viewExprText e
--               ]
--                 <> ( bs
--                         <&> \(CaseBranch n bs' e') ->
--                             div_
--                                 []
--                                 $ [pat n]
--                                     <> ( bs' <&> \(Bind _ n') ->
--                                             text $ " " <> lname n'
--                                        )
--                                     <> [ "→"
--                                        , viewExprText e'
--                                        ]
--                    )
--                 <> case fallback of
--                     CaseExhaustive -> []
--                     CaseFallback e' -> ["_", "→", viewExprText e']
--       where
--         pat = \case
--             PatCon n -> text $ gname n
--             PatPrim pc -> prim pc

--     --     casesAligned :: [Doc AnsiStyle]
--     --     casesAligned = map (\(f, s) -> fill caseWidth f <> s) caseParts
--     --       where
--     --         caseWidth :: Int
--     --         -- 'unsafeMaximum' is safe here, as 'caseWidth' is only evaluated
--     --         -- if 'caseParts' is non-empty
--     --         caseWidth = unsafeMaximum $ map (T.length . show . fst) caseParts
--     -- TODO don't just ignore the annotation
--     Ann _ e t -> viewExprText e
--     Primer.App _ e1 e2 ->
--         div_
--             []
--             [ viewExprText e1
--             , viewExprText e2
--             ]
--     APP _ e t ->
--         div_
--             []
--             [ viewExprText e
--             , viewTypeText t
--             ]
--     -- Let _ v e e' ->
--     --     col Yellow "let"
--     --         <+> lname v
--     --         <+> col Yellow "="
--     --         <> inlineblock opts (viewExpr e)
--     --         <> col Yellow "in"
--     --         <> line
--     --         <> indent' 2 (viewExpr e')
--     -- LetType _ v t e ->
--     --     col Yellow "let type"
--     --         <+> lname v
--     --         <+> col Yellow "="
--     --         <> inlineblock opts (viewType t)
--     --         <> col Yellow "in"
--     --         <> line
--     --         <> indent' 2 (viewExpr e)
--     -- Letrec _ v e t e' ->
--     --     col Yellow "let rec"
--     --         <+> lname v
--     --         <+> col Yellow "="
--     --         <> inlineblock opts (typeann e t)
--     --         <> col Yellow "in"
--     --         <> line
--     --         <> indent' 2 (viewExpr e')
--     -- PrimCon _ p -> prim p
--     todo -> error $ "term not yet implemented: " <> show todo
--   where
--     lname = unName . unLocalName
--     gname = unName . baseName
--     prim = \case
--         PrimChar c -> text $ T.pack $ show c
--         PrimInt n -> text $ T.pack $ show n
--         -- TODO just GIF - shouldn't be hard...
--         PrimAnimation n -> error "animation not yet supported"

-- TODO is this a bit too ad-hoc? thing of good API and naming
class TypeMetaThingy a b where
    typeMetaThingyClickHandler :: Type' a b -> View Action -> View Action
instance TypeMetaThingy () () where
    typeMetaThingyClickHandler _ = identity
instance TypeMetaThingy (Meta (Kind' ())) (Meta ()) where -- this is the type metadata used by `ExprT`/`TypeT`
    typeMetaThingyClickHandler t = div_ [onClickType t] . pure

-- viewTypeText :: (TypeMetaThingy a b, Show a, Show b) => Type' a b -> View Action
-- viewTypeText t0 = typeMetaThingyClickHandler t0 case t0 of
--     TEmptyHole _ -> "?"
--     THole _ t -> div_ [] ["{?", viewTypeText t, "?}"]
--     TCon _ n -> text $ gname n
--     TFun _ t1 t2 ->
--         div_
--             []
--             [ viewTypeText t1
--             , "->"
--             , viewTypeText t2
--             ]
--     TVar _ n -> text $ lname n
--     TApp _ t1 t2 ->
--         div_
--             []
--             [ viewTypeText t1
--             , " "
--             , viewTypeText t2
--             ]
--     -- TODO show kind _somehow_ - did the JS frontend display it?
--     TForall _ n k t ->
--         div_
--             []
--             [ "∀"
--             , text $ lname n
--             , viewTypeText t
--             ]
--     -- TLet _ v t b ->
--     --     col Yellow "let"
--     --         <+> lname v
--     --         <+> col Yellow "="
--     --         <> inlineblock opts (viewType t)
--     --         <> col Yellow "in"
--     --         <> line
--     --         <> indent' 2 (viewType b)
--     todo -> error $ "type not yet implemented: " <> show todo
--   where
--     lname = unName . unLocalName
--     gname = unName . baseName

-- tcBasic :: Expr -> Either TypeError (Type' () (), ExprT)
-- tcBasic = runTC . flip (runReaderT @_ @(M TypeError)) (initialCxt SmartHoles) . synth

-- TODO just use `tcWholeProg`? unfortunately it doesn't return a prog containing `ExprT`'s so we throw away information
-- tcBasicProg :: Prog -> Expr -> Either TypeError (Type' () (), ExprT)
-- tcBasicProg p = runTC . flip (runReaderT @_ @(M TypeError)) (progCxt p) . synth
-- for whatever reason, `synth` deletes the case branches in `map`
tcBasicProg :: Prog -> ASTDef -> Either TypeError ExprT
tcBasicProg p ASTDef{..} = runTC . flip (runReaderT @_ @(M TypeError)) (progCxt p) $ check (forgetTypeMetadata astDefType) astDefExpr

-- tcBasic = runTC . flip (runReaderT @_ @(M TypeError)) (initialCxt NoSmartHoles) . synth

mapExpr :: ExprT
mapExpr =
    let (p, _, _) = newProg
     in either (error . ("static well-formed expression can't fail TC: " <>) . show) identity
            . tcBasicProg p
            $ fromMaybe (error "prog doesn't contain Prelude.map") do
                m <- find ((== mkSimpleModuleName "Prelude") . moduleName) $ progImports p
                DefAST d <- Map.lookup "map" $ moduleDefs m
                pure d

conName :: (Data a) => a -> Text
conName = T.pack . showConstr . toConstr

-- TODO upstream this stuff to Primer library if similar doesn't already exist
-- instance HasID a => HasID (Selection' a) where
--   _id :: Lens' (Selection' a) ID
--   _id = _
-- getIDSelection :: (HasID a) => Selection' a -> Maybe ID
-- getIDSelection = \case
--     SelectionDef DefSelection{node = Just NodeSelection{nodeType = BodyNode, meta}} -> Just $ getID meta
--     sel -> error "only body node selections are supported so far"
getIDNodeSelection :: (HasID a) => NodeSelection a -> ID
getIDNodeSelection ns = getID ns.meta
typeFromCache :: TypeCache -> Type' () ()
typeFromCache = \case
    TCSynthed t -> t
    TCChkedAt t -> t
    -- TODO prefer synthed or checked?
    -- I'm sure I discussed this same question with Ben at some point in some other context
    TCEmb (TCBoth{tcChkedAt = _tcChkedAt, tcSynthed}) -> tcSynthed

-- type SelectionT = Selection' (Either (Meta TypeCache) (Either (Meta (Kind' ())) KindMeta)) -- analogous with `ExprT`/`TypeT`
type NodeSelectionT = NodeSelection (Either (Meta TypeCache) (Either (Meta (Kind' ())) KindMeta)) -- analogous with `ExprT`/`TypeT`
type TypeT = Type' (Meta (Kind' ())) (Meta ()) -- comes from hidden module
-- type KindT = Kind' (Meta ())

kindsInType :: AffineTraversal' (Type' a b) (Kind' b) -- anologous to `typesInExpr`
kindsInType = atraversalVL $ \point f -> \case
    TForall m a k t -> flip (TForall m a) t <$> f k
    e -> point e

-- TODO this is all basically copied from Primer library - find a way to expose
newtype M e a = M {unM :: StateT (ID, NameCounter) (Except e) a}
    deriving newtype (Functor, Applicative, Monad, MonadError e)
instance MonadFresh ID (M e) where
    fresh = M $ _1 <<%= succ
instance MonadFresh NameCounter (M e) where
    fresh = M $ _2 <<%= succ
runTC :: M e a -> Either e a
runTC = runExcept . flip evalStateT (0, toEnum 0) . (.unM)

nodeSelectionType :: NodeSelectionT -> Either (Type' () ()) (Either (Kind' ()) ())
nodeSelectionType =
    bimap
        (typeFromCache . Optics.view _type)
        (bimap (Optics.view _type) (Optics.view _type))
        . (.meta)
