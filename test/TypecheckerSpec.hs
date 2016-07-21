module TypecheckerSpec where

import Data.Either
import Language.FunLog.Types
import Language.FunLog.Typecheck
import qualified Language.FunLog.Syntax as P
import qualified Language.FunLog.Parser as P
import qualified Text.Parsec as P
import Test.Hspec

stdlib = concat
    [ "maybe = type Just : 'A -> maybe 'A. Nothing : maybe 'A. end."
    , "list = type Nil : list 'A. Cons : 'A -> list 'A -> list 'A. end."
    , "either = type Left : 'A -> either 'A 'B. Right : 'A -> either 'B 'A. end."
    , "odd = type OSucc : even -> odd. end."
    , "even = type EZero : even. ESucc : odd -> even. end."
    ]

testTypecheck :: (Either TypeCheckerError () -> Bool) -> String -> IO ()
testTypecheck fun str = do
    let Right (P.Program decls) = P.parse P.parseProgram "<input>" (stdlib ++ str)
    ret <- runTcM $ declare DeclGlobal decls $ pure ()
    ret `shouldSatisfy` fun

shouldTypecheck = testTypecheck isRight
shouldNotTypecheck = testTypecheck isLeft

simpleExpressions :: Spec
simpleExpressions = context "When checking simple expressions" $ do
    it "should check let correctly" $
        shouldTypecheck "f = let x = 7 in Cons x Nil."

    it "should check annotated let correctly" $
        shouldTypecheck "f = let (x : int) = 7 in x."

    it "should check annotated let's result correctly" $
        shouldTypecheck "f = let x = 7 in x : int."

    it "detects unbound patterns" $
        shouldNotTypecheck "x = let Foo = 42 in 42."

    it "detects unbound names" $
        shouldNotTypecheck "x = let y = y in 42."

functionTypes :: Spec
functionTypes = context "When checking functions" $ do
    it "infers patterns correctly" $
        shouldTypecheck "f = (fun (Just x) (Just y) => Just (x # y). _ _ => Nothing. end) (Just 42) (Just 50) : maybe (int # int)."

    it "infers types for nested patterns correctly" $
        shouldTypecheck "join = fun (Just (Just x)) => Just x. _ => Nothing. end : maybe (maybe 'A) -> maybe 'A."

    it "allows recursion" $
        shouldTypecheck "map = fun _ Nil => Nil. f (Cons x xs) => Cons (f x) (map f xs). end."

    it "checks for pattern arity mismatches" $
        shouldNotTypecheck "wrong = fun (Cons 5) => 42. end."

    it "orders correctly" $ do
        shouldTypecheck "id = fun x => x. end. x = id 5. y = id Nil."
        shouldTypecheck "x = id 5. id = fun x => x. end. y = id Nil."
        shouldTypecheck "x = id 5. y = id Nil. id = fun x => x. end."

    it "generalises en block in presence of signatures" $
        shouldTypecheck "id : 'A -> 'A. id = fun x => x. end. x = id 5. y = id Nil."

    it "allows mutual recursion on algebraic data types" $
        shouldTypecheck "decEven = fun (ESucc n) => decOdd n. _ => fail. end. decOdd = fun (OSucc n) => decEven n. end."

    it "disallows function arity mismatches" $
        shouldNotTypecheck "f = fun 5 => 42. x y => 9. end."

    it "prohibits name reuse" $
        shouldNotTypecheck "f = fun x x=> 42. end."


failGADTs :: Spec
failGADTs = context "When encountering GADTs" $ do
    it "does not accept existentials" $
        shouldNotTypecheck "wrong = type Wrong : 'A -> wrong. end."

    it "does not accept constrained types" $
        shouldNotTypecheck "wrong = type Wrong : wrong int. end."

failKindMismatch :: Spec
failKindMismatch = context "When kind-checking" $ do
    it "checks kinds in constructors" $
        shouldNotTypecheck "wrong = type One : wrong 'A. Two : wrong 'A 'B'. end."

    it "checks kinds in constructor arguments" $
        shouldNotTypecheck "wrong = type One : wrong 'A -> wrong 'A 'B. end."

    it "checks kinds in signatures" $
        shouldNotTypecheck "id = fun x => x. end : list -> list."

    it "checks kind in expression signatures" $
        shouldNotTypecheck "f = Nil : list."

    it "checks kind in pattern signatures" $
        shouldNotTypecheck "f = fun (x : list) => x. end."

annotations :: Spec
annotations = context "When checking annotations" $ do
    it "checks whether they are correct" $
        shouldNotTypecheck "x = 5 : list int."

    it "allows them on expressions" $
        shouldTypecheck "id = fun x => x. end : list int -> list int."

    it "allows them as annotations" $
        shouldTypecheck "id : list 'A -> list 'A. id = fun x => x. end."

    it "respects them" $
        shouldNotTypecheck "x = let id = fun x => x. end : list int -> list int in id 5."

    it "performs skolem escape checks" $
        shouldNotTypecheck "go = go : 'A."

    it "respects top-level annotations" $
        shouldNotTypecheck "id : list 'A -> list 'A. id = fun x => x. end. x = id 5."

    it "respects out-of-order top-level annotations" $
        shouldNotTypecheck "x = id 5. id : list 'A -> list 'A. id = fun x => x. end."

    it "respects pattern signatures" $
        shouldNotTypecheck "id = fun (x : int) => x. end. x = id Nil."

    it "allows pattern signatures" $
        shouldTypecheck "x = fun 5 => 6. (x : int) => x. end."

    it "disallows polymorphic pattern signatures" $
        shouldNotTypecheck "x = fun (x : 'A) => x. end."

    it "disallows wrong signatures" $
        shouldNotTypecheck "f : 'A -> 'B -> 'A. f = fun x => id. end where id = fun x => x. end. end."

    it "detects spurious signatures" $
        shouldNotTypecheck "x : int."

    it "detects duplicate signatures" $
        shouldNotTypecheck "x : int. x : int -> int. x = x."

localBindings :: Spec
localBindings = context "When checking local bindings" $ do
    it "allows mutual recursion" $
        shouldTypecheck "f = fun x => x. end where x = y. y = x. end."

    it "allows polymorphic mutual recursion" $
        shouldTypecheck "f = fun x => x. end where x = y. y = x. end."

    it "shadows names from the outer scope" $
        shouldTypecheck "foo = 5. f = fun x => Cons x foo. end where foo = Nil. end."

builtins :: Spec
builtins = context "When using builtins" $ do
    it "checks them correctly" $
        shouldTypecheck "x = 5 + 2 * 3 - 7 / 9."

    it "checks polymorphic ones correctly" $
        shouldTypecheck "x = fun z => compare z Nil. end."

    it "disallows incorrect use" $
        shouldNotTypecheck "x = 5 + 2 * Nil."

    it "gives the correct result type to polymorphic operators" $
        shouldNotTypecheck "x = (2 == 3) : int."

exhaustiveness :: Spec
exhaustiveness = context "When performing exhaustiveness checks" $ do
    it "rejects incomplete matches in functions" $
        shouldNotTypecheck "foo = type A : foo. B : foo. end. f = fun A => 42. end."

    it "rejects incomplete matches in matches" $
        shouldNotTypecheck "foo = type A : foo. B : foo. end. f = match A with A => 42. end."

    it "rejects incomplete multimatches" $
        shouldNotTypecheck "foo = type A : foo. B : foo. end. f = fun A _ => 42. _ A => 42. end."

    it "rejects incomplete numeric matches" $
        shouldNotTypecheck "f = fun 42 => 42. end."

    it "does not reject complete multimatches" $
        shouldTypecheck "both = fun f (Just x) (Just y) => f x y. _ Nothing _ => Nothing. _ _ Nothing => Nothing. end."

spec :: Spec
spec = context "The typechecker" $ do
    simpleExpressions
    failGADTs
    failKindMismatch
    functionTypes
    annotations
    localBindings
    builtins
    exhaustiveness
