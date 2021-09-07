-- Copyright (c) Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds, AllowAmbiguousTypes #-}
module Retrie.ExactPrint.Annotated
  ( -- * Annotated
    Annotated
  , astA
  , annsA
  , seedA
  -- ** Synonyms
  , AnnotatedHsDecl
  , AnnotatedHsExpr
  , AnnotatedHsType
  , AnnotatedImport
  , AnnotatedImports
  , AnnotatedModule
  , AnnotatedPat
  , AnnotatedStmt
  -- ** Operations
  , pruneA
  , graftA
  , transformA
  , trimA
  , printA
    -- * Internal
  , unsafeMkA
  ) where

import Control.Monad.State.Lazy hiding (fix)
import Data.Default as D
import Data.Functor.Identity

import Language.Haskell.GHC.ExactPrint hiding
  ( cloneT
  , setEntryDP
  , setEntryDPT
  , transferEntryDPT
  , transferEntryDP
  )
#if __GLASGOW_HASKELL__ >= 902
import Language.Haskell.GHC.ExactPrint.ExactPrint (ExactPrint)
#else
import Language.Haskell.GHC.ExactPrint.Annotate (Annotate)
import Language.Haskell.GHC.ExactPrint.Types (emptyAnns)
#endif

import Retrie.GHC
import Retrie.SYB

#if __GLASGOW_HASKELL__ >= 902
type Anns = ()

emptyAnns :: Anns
emptyAnns = ()
#else
type ExactPrint = Annotate
#endif

-- Annotated -----------------------------------------------------------------

type AnnotatedHsDecl = Annotated (LHsDecl GhcPs)
type AnnotatedHsExpr = Annotated (LHsExpr GhcPs)
type AnnotatedHsType = Annotated (LHsType GhcPs)
type AnnotatedImport = Annotated (LImportDecl GhcPs)
type AnnotatedImports = Annotated [LImportDecl GhcPs]
type AnnotatedModule = Annotated (Located HsModule)
type AnnotatedPat = Annotated (LocatedA (Pat GhcPs))
type AnnotatedStmt = Annotated (LStmt GhcPs (LHsExpr GhcPs))

-- | 'Annotated' packages an AST fragment with the annotations necessary to
-- 'exactPrint' or 'transform' that AST.
data Annotated ast = Annotated
  { astA :: ast
  -- ^ Examine the actual AST.
  , annsA  :: Anns
  -- ^ Annotations generated/consumed by ghc-exactprint
  , seedA  :: Int
  -- ^ Name supply used by ghc-exactprint to generate unique locations.
  }

instance Functor Annotated where
  fmap f Annotated{..} = Annotated{astA = f astA, ..}

instance Foldable Annotated where
  foldMap f = f . astA

instance Traversable Annotated where
  traverse f Annotated{..} =
    (\ast -> Annotated{astA = ast, ..}) <$> f astA

instance Default ast => Default (Annotated ast) where
  def = Annotated D.def emptyAnns 0

instance (Data ast, Monoid ast) => Semigroup (Annotated ast) where
  (<>) = mappend

instance (Data ast, Monoid ast) => Monoid (Annotated ast) where
  mempty = Annotated mempty emptyAnns 0
  mappend a1 (Annotated ast2 anns _) =
    runIdentity $ transformA a1 $ \ ast1 ->
#if __GLASGOW_HASKELL__ >= 902
      mappend ast1 <$> pure ast2
#else
      mappend ast1 <$> graftT anns ast2
#endif

-- | Construct an 'Annotated'.
-- This should really only be used in the parsing functions, hence the scary name.
-- Don't use this unless you know what you are doing.
unsafeMkA :: ast -> Anns -> Int -> Annotated ast
unsafeMkA = Annotated

-- | Transform an 'Annotated' thing.
transformA
  :: Monad m => Annotated ast1 -> (ast1 -> TransformT m ast2) -> m (Annotated ast2)
transformA (Annotated ast anns seed) f = do
#if __GLASGOW_HASKELL__ >= 902
  (ast', seed',_) <- runTransformFromT seed (f ast)
  let anns' = emptyAnns
#else
  (ast',(anns',seed'),_) <- runTransformFromT seed anns (f ast)
#endif
  return $ Annotated ast' anns' seed'


-- | Graft an 'Annotated' thing into the current transformation.
-- The resulting AST will have proper annotations within the 'TransformT'
-- computation. For example:
--
-- > mkDeclList :: IO (Annotated [LHsDecl GhcPs])
-- > mkDeclList = do
-- >   ad1 <- parseDecl "myId :: a -> a"
-- >   ad2 <- parseDecl "myId x = x"
-- >   transformA ad1 $ \ d1 -> do
-- >     d2 <- graftA ad2
-- >     return [d1, d2]
--
graftA :: (Data ast, Monad m) => Annotated ast -> TransformT m ast
graftA (Annotated x anns _) =
#if __GLASGOW_HASKELL__ >= 902
  pure x
#else
  graftT anns x
#endif


-- | Encapsulate something in the current transformation into an 'Annotated'
-- thing. This is the inverse of 'graftT'. For example:
--
-- > splitHead :: Monad m => Annotated [a] -> m (Annotated a, Annotated [a])
-- > splitHead l = fmap astA $ transformA l $ \(x:xs) -> do
-- >   y <- pruneA x
-- >   ys <- pruneA xs
-- >   return (y, ys)
--
pruneA :: (Data ast, Monad m) => ast -> TransformT m (Annotated ast)
pruneA ast = Annotated ast <$> undefined <*> undefined

-- | Trim the annotation data to only include annotations for 'ast'.
-- (Usually, the annotation data is a superset of what is necessary.)
-- Also freshens all source locations, so filename information
-- in annotation keys is discarded.
--
-- Note: not commonly needed, but useful if you want to inspect the annotation
-- data directly and don't want to wade through a mountain of output.
trimA :: Data ast => Annotated ast -> Annotated ast
trimA = runIdentity . transformA nil . const . graftA
  where
    nil :: Annotated ()
    nil = mempty

-- | Exactprint an 'Annotated' thing.
printA :: ExactPrint ast => Annotated (Located ast) -> String
printA (Annotated ast anns _) =
#if __GLASGOW_HASKELL__ >= 902
  exactPrint ast
#else
  exactPrint ast anns
#endif
