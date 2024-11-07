{- Vendored from: https://hackage.haskell.org/package/diagrams-contrib-1.4.5.1/docs/Diagrams-TwoD-Layout-Tree.html

We can't use `diagrams-contrib` since it pulls in `diagrams-lib`, which in turn uses `fsnotify`, which uses `unix-compat`, which contains some C code that fails to build. It potentially makes sense to avoid such a huge dependency tree anyway. Besides, this is all a temporary solution in lieu of a better layout algorithm, such as a Haskell implementation of Tidy.

Differences from upstream:
- Remove everything but the simple symmetric layout algorithm.
- Vendor some further required definitions from `diagrams-contrib`'s own dependencies, mostly from `linear`. These are all in one block at the very top of the module body. It might be possible to depend on `linear` instead, now that we have Template Haskell support for WASM.
- Add `NoLexicalNegation` to override this package's default, in order to avoid a syntax error in `unRelativize`.
-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NoLexicalNegation         #-}
module Layout
       ( -- * Layout algorithms

         -- ** Symmetric layout

         -- $symmetric
         symmLayout
       , symmLayout'
       , SymmLayoutOpts(..), slHSep, slVSep, slWidth, slHeight

       , P2(..)
       , Default(..)

       ) where

import           Control.Arrow       ((&&&), (***))

import           Data.List           (mapAccumL)
import           Data.Tree

import Optics hiding (Empty)

data P2 a = P2{x :: a, y :: a} deriving (Eq, Show, Functor)
origin :: Num a => P2 a
origin = P2 0 0
class Default a where def :: a
unitX :: (Num n) => P2 n
unitX = P2 1 0
-- unit_X :: Num n => P2 n
-- unit_X = P2 (-1) 0
-- unitY :: Num n => P2 n
-- unitY = P2 0 1
unit_Y :: Num n => P2 n
unit_Y = P2 0 (-1)
(*^) :: Num a => a -> P2 a -> P2 a
(*^) a = fmap (a *)
(.+^) :: Num a => P2 a -> P2 a -> P2 a
P2 x1 y1 .+^ P2 x2 y2 = P2 (x1 + x2) (y1 + y2)

------------------------------------------------------------
--  Binary trees
------------------------------------------------------------

-- $BTree
-- There is a standard type of rose trees ('Tree') defined in the
-- @containers@ package, but there is no standard type for binary
-- trees, so we define one here.  Note, if you want to draw binary
-- trees with data of type @a@ at the leaves, you can use something
-- like @BTree (Maybe a)@ with @Nothing@ at internal nodes;
-- 'renderTree' lets you specify how to draw each node.


------------------------------------------------------------
--  Layout algorithms
------------------------------------------------------------

--------------------------------------------------
-- "Symmetric" layout of rose trees.

-- $symmetric
-- \"Symmetric\" layout of rose trees, based on the algorithm described in:
--
-- Andrew J. Kennedy. /Drawing Trees/, J Func. Prog. 6 (3): 527-534,
-- May 1996.
--
-- Trees laid out using this algorithm satisfy:
--
--   1. Nodes at a given level are always separated by at least a
--   given minimum distance.
--
--   2. Parent nodes are centered with respect to their immediate
--   offspring (though /not/ necessarily with respect to the entire
--   subtrees under them).
--
--   3. Layout commutes with mirroring: that is, the layout of a given
--   tree is the mirror image of the layout of the tree's mirror
--   image.  Put another way, there is no inherent left or right bias.
--
--   4. Identical subtrees are always rendered identically.  Put
--   another way, the layout of any subtree is independent of the rest
--   of the tree.
--
--   5. The layouts are as narrow as possible while satisfying all the
--   above constraints.

-- | A tree with /relative/ positioning information.  The @n@
--   at each node is the horizontal /offset/ from its parent.
type Rel t n a = t (a, n)

-- | Shift a RelTree horizontally.
moveTree :: Num n => n -> Rel Tree n a -> Rel Tree n a
moveTree x' (Node (a, x) ts) = Node (a, x+x') ts

-- | An /extent/ is a list of pairs, recording the leftmost and
--   rightmost (absolute) horizontal positions of a tree at each
--   depth.
newtype Extent n = Extent { getExtent :: [(n, n)] }

extent :: ([(n, n)] -> [(n, n)]) -> Extent n -> Extent n
extent f = Extent . f . getExtent

consExtent :: (n, n) -> Extent n -> Extent n
consExtent = extent . (:)

-- | Shift an extent horizontally.
moveExtent :: Num n => n -> Extent n -> Extent n
moveExtent x = (extent . map) ((+x) *** (+x))

-- | Reflect an extent about the vertical axis.
flipExtent :: Num n => Extent n -> Extent n
flipExtent = (extent . map) (\(p,q) -> (-q, -p))

-- | Merge two non-overlapping extents.
mergeExtents :: Extent n -> Extent n -> Extent n
mergeExtents (Extent e1) (Extent e2) = Extent $ mergeExtents' e1 e2
  where

    mergeExtents' [] qs = qs
    mergeExtents' ps [] = ps
    mergeExtents' ((p,_) : ps) ((_,q) : qs) = (p,q) : mergeExtents' ps qs

instance Semigroup (Extent n) where
  (<>) = mergeExtents

instance Monoid (Extent n) where
  mempty  = Extent []
  mappend = (<>)

-- | Determine the amount to shift in order to \"fit\" two extents
--   next to one another.  The first argument is the separation to
--   leave between them.
fit :: (Num n, Ord n) => n -> Extent n -> Extent n -> n
fit hSep (Extent ps) (Extent qs) = maximum (0 : zipWith (\(_,p) (q,_) -> p - q + hSep) ps qs)

-- | Fit a list of subtree extents together using a left-biased
--   algorithm.  Compute a list of positions (relative to the leftmost
--   subtree which is considered to have position 0).
fitListL :: (Num n, Ord n) => n -> [Extent n] -> [n]
fitListL hSep = snd . mapAccumL fitOne mempty
  where
    fitOne acc e =
      let x = fit hSep acc e
      in  (acc <> moveExtent x e, x)

-- | Fit a list of subtree extents together with a right bias.
fitListR :: (Num n, Ord n) => n -> [Extent n] -> [n]
fitListR hSep = reverse . map negate . fitListL hSep . map flipExtent . reverse

-- | Compute a symmetric fitting by averaging the results of left- and
--   right-biased fitting.
fitList :: (Fractional n, Ord n) => n -> [Extent n] -> [n]
fitList hSep = uncurry (zipWith mean) . (fitListL hSep &&& fitListR hSep)
  where mean x y = (x+y)/2

-- | Options for controlling the symmetric tree layout algorithm.
data SymmLayoutOpts n a =
  SLOpts { _slHSep   :: n -- ^ Minimum horizontal
                                         --   separation between sibling
                                         --   nodes.  The default is 1.
         , _slVSep   :: n -- ^ Vertical separation
                                         --   between adjacent levels of
                                         --   the tree.  The default is 1.
         , _slWidth  :: a -> (n, n)
           -- ^ A function for measuring the horizontal extent (a pair
           --   of x-coordinates) of an item in the tree.  The default
           --   is @const (0,0)@, that is, the nodes are considered as
           --   taking up no space, so the centers of the nodes will
           --   be separated according to the @slHSep@ and @slVSep@.
           --   However, this can be useful, /e.g./ if you have a tree
           --   of diagrams of irregular size and want to make sure no
           --   diagrams overlap.  In that case you could use
           --   @fromMaybe (0,0) . extentX@.
         , _slHeight :: a -> (n, n)
           -- ^ A function for measuring the vertical extent of an
           --   item in the tree.  The default is @const (0,0)@.  See
           --   the documentation for 'slWidth' for more information.
         }

makeLenses ''SymmLayoutOpts

instance Num n => Default (SymmLayoutOpts n a) where
  def = SLOpts
          { _slHSep   = 1
          , _slVSep   = 1
          , _slWidth  = const (0,0)
          , _slHeight = const (0,0)
          }

-- | Actual recursive tree layout algorithm, which returns a tree
--   layout as well as an extent.
symmLayoutR :: (Fractional n, Ord n) => SymmLayoutOpts n a -> Tree a -> (Rel Tree n a, Extent n)
symmLayoutR opts (Node a ts) = (rt, ext)
  where (trees, extents) = unzip (map (symmLayoutR opts) ts)
        positions        = fitList (opts ^. slHSep) extents
        pTrees           = zipWith moveTree positions trees
        pExtents         = zipWith moveExtent positions extents
        ext              = (opts^.slWidth) a `consExtent` mconcat pExtents
        rt               = Node (a, 0) pTrees

-- | Run the symmetric rose tree layout algorithm on a given tree,
--   resulting in the same tree annotated with node positions.
symmLayout' :: (Fractional n, Ord n) => SymmLayoutOpts n a -> Tree a -> Tree (a, P2 n)
symmLayout' opts = unRelativize opts origin . fst . symmLayoutR opts

-- | Run the symmetric rose tree layout algorithm on a given tree
--   using default options, resulting in the same tree annotated with
--   node positions.
symmLayout :: (Fractional n, Ord n) => Tree a -> Tree (a, P2 n)
symmLayout = symmLayout' def

-- | Given a fixed location for the root, turn a tree with
--   \"relative\" positioning into one with absolute locations
--   associated to all the nodes.
unRelativize :: (Num n, Ord n) =>
                SymmLayoutOpts n a -> P2 n -> Rel Tree n a -> Tree (a, P2 n)
unRelativize opts curPt (Node (a,hOffs) ts)
    = Node (a, rootPt) (map (unRelativize opts (rootPt .+^ (vOffs *^ unit_Y))) ts)
  where rootPt = curPt .+^ (hOffs *^ unitX)
        vOffs  = - fst ((opts^.slHeight) a)
               + (maximum . map (snd . (opts^.slHeight) . fst . rootLabel) $ ts)
               + (opts ^. slVSep)
