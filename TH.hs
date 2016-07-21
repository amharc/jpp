{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module TH where

import Language.FunLog.Parser
import Language.FunLog.Syntax
import Language.FunLog.Typecheck
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Text.Parsec

quoteDecls :: FilePath -> ExpQ
quoteDecls fileName = do
    contents <- runIO $ readFile fileName
    addDependentFile fileName
    case parse parseProgram fileName contents of
        -- Unfortunately DeriveLift is available only in GHC 8...
        Right (Program tree) ->
            runIO (runTcM (declare DeclGlobal tree $ pure ())) >>= \case
                Left err -> fail $ show err
                Right _ -> dataToExpQ (const Nothing) tree
        Left err -> fail $ show err

declFile :: QuasiQuoter
declFile = QuasiQuoter
    { quoteExp = quoteDecls
    }
