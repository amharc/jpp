module UnificationSpec (spec) where

import Data.Either
import Language.FunLog.Types
import Language.FunLog.Typecheck
import Test.Hspec

shouldUnify :: Type -> Type -> Spec
shouldUnify ty ty' =
    it ("unifies " ++ show ty ++ " with " ++ show ty') $ do
        res <- runTcM (unify ty ty')
        res `shouldSatisfy` isRight

shouldNotUnify :: Type -> Type -> Spec
shouldNotUnify ty ty' =
    it ("does not unify " ++ show ty ++ " with " ++ show ty') $ do
        res <- runTcM (unify ty ty')
        res `shouldSatisfy` isLeft

spec :: Spec
spec = context "Unification algorithm" $ do
   TyInt `shouldUnify` TyInt
   TyInt `shouldNotUnify` (TyInt `TyFun` TyInt)
   TyTuple [] `shouldUnify` TyTuple []
   TyTuple [] `shouldNotUnify` TyTuple [TyInt, TyInt]
   TyTuple [TyInt `TyFun` TyInt] `shouldUnify` TyTuple [TyInt `TyFun` TyInt]
   TyVar (BoundTyVar "a") `shouldUnify` TyVar (BoundTyVar "a")
   TyVar (BoundTyVar "a") `shouldNotUnify` TyVar (BoundTyVar "b")
   TyVar (SkolemTyVar "a" 0) `shouldUnify` TyVar (SkolemTyVar "a" 0)
   TyVar (SkolemTyVar "a" 0) `shouldNotUnify` TyVar (BoundTyVar "a")
   TyVar (SkolemTyVar "a" 0) `shouldNotUnify` TyVar (SkolemTyVar "b" 1)
   TyVar (SkolemTyVar "a" 0) `shouldNotUnify` TyVar (SkolemTyVar "a" 1)
    
