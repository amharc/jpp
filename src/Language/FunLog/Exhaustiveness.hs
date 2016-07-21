{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.FunLog.Exhaustiveness (unmatched) where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import Language.FunLog.Syntax hiding (Type(..))
import Language.FunLog.Types

type EcM = MonadReader Env

-- |Splits the given 'DataSkel' into multiple skeletons,
-- only one of which is matched by the 'Pattern' provided.
--
-- If no such refinement could be found, returns only the
-- skeleton provided.
refine :: EcM m => Pattern -> DataSkel -> m [DataSkel]
refine PatWildcard skel = pure [skel]
refine (PatBind _) skel = pure [skel]
refine (PatLit _) _ = pure [DsLargeInt]
refine pat@(PatTuple pats) DsAny = refine pat . DsTuple $ replicate (length pats) DsAny
refine (PatTuple pats) (DsTuple skels) = fmap DsTuple <$> refineMany pats skels
refine (PatData patCtor patArgs) (DsCtor dsCtor dsArgs)
    | patCtor == dsCtor = fmap (DsCtor dsCtor) <$> refineMany patArgs dsArgs
refine (PatData patCtor _) DsAny = do
    Just ctor <- view $ envCtors . at patCtor
    adt <- ctorADT $ ctor ^. ctorType
    pure $ ctorSkel <$> adt ^. adtCtors
refine (PatType pat _) skel = refine pat skel
refine _ _ = error "Pattern and skeleton mismatch in refine"

refineMany :: EcM m => [Pattern] -> [DataSkel] -> m [[DataSkel]]
refineMany [] [] = pure [[]]
refineMany (pat:pats) (skel:skels) = refine pat skel >>= \case
    [skel'] | skel' == skel -> fmap (skel:) <$> refineMany pats skels
    skel's -> pure $ (:skels) <$> skel's
refineMany _ _ = error "Arity mismatch in refineMany"

ctorADT :: EcM m => Scheme -> m ADT
ctorADT (Forall _ ty) = views (envTypes . at name) fromJust
  where
    name = go ty

    go (TyFun _ res) = go res
    go (TyCtor cname _) = cname
    go _ = error "Illegal constructor type in ctorADT"

ctorSkel :: Ctor -> DataSkel
ctorSkel ctor = DsCtor (ctor ^. ctorName) (replicate (ctor ^. ctorArity) DsAny)

matches :: Pattern -> DataSkel -> Bool
matches _ DsAny = True
matches PatWildcard _ = True
matches (PatBind _) _ = True
matches (PatTuple pats) (DsTuple args) = matchesMany pats args
matches (PatData ctor pats) (DsCtor ctor' args)
    | ctor == ctor' = matchesMany pats args
matches (PatType pat _) skel = matches pat skel
matches _ _ = False

matchesMany :: [Pattern] -> [DataSkel] -> Bool
matchesMany pats skel = and $ zipWith matches pats skel

unmatched :: EcM m => [[Pattern]] -> m [[DataSkel]]
unmatched pats0 = execWriterT $ go pats0 (replicate (length $ head pats0) DsAny)
  where
    go :: (EcM m, MonadWriter [[DataSkel]] m) => [[Pattern]] -> [DataSkel] -> m ()
    go [] skels = tell [skels]
    go (pat:pats) skels = refineMany pat skels >>= processRefined (pat:pats) skels

    processRefined :: (EcM m, MonadWriter [[DataSkel]] m) => [MultiPattern] -> [DataSkel] -> [[DataSkel]] -> m ()
    processRefined _ _ [] = pure ()
    processRefined pats skels skels' = unless ([skels] == skels') . forM_ skels' $ \skel ->
        go (filter (`matchesMany` skel) pats) skel
