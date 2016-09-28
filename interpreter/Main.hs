{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Language.FunLog.Parser
import Language.FunLog.Eval as E
import Language.FunLog.Syntax
import Language.FunLog.Typecheck as TC
import Text.Parsec
import Text.Parsec.String
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import System.IO
import System.Environment

import TH

stdlib :: [Decl]
stdlib = [declFile|stdlib.fl|]

run :: FilePath -> IO ()
run = parseFromFile parseProgram >=> either (hPrint stderr) typecheck

typecheck :: Program -> IO ()
typecheck (Program prog) = runTcM (TC.declare DeclGlobal stdlib . TC.declare DeclGlobal prog $ TC.infer (ExpName "main")) >>= \case
    Left err -> hPrint stderr err
    Right _ -> eval $ Program prog

eval :: Program -> IO ()
eval (Program prog) = runIM (denoteDecls stdlib . denoteDecls prog $ denote (ExpName "main") >>= scrutinise) >>= \case
    Left err -> hPrint stderr err
    _ -> pure ()

main = getArgs >>= \case
    [file] -> run file
    _ -> hPutStrLn stderr "Usage: interpreter filename"
