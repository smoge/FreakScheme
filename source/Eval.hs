{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Env
import LispVal
import Text.Parsec

-- Eval is a monad that will contain the reader monad, which will
-- lexical scoping, user input, and will contain the return
-- LispVal for any valid expression, done by monad transformers

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO
    )

basicEnv :: Map T.Text LispVal
basicEnv =
  Map.fromList $
    primEnv
      <> [("read", Fun $ IFunc $ unop $ readFn)]

-- Source File
evalFile :: T.Text -> IO ()
evalFile fileExpr = (runASTinEnv basicEnv $ fileToEvalForm fileExpr) >>= print

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input =
  either
    (throw . PError . show)
    evalBody
    $ readExprFile input

runParseTest :: T.Text -> T.Text
runParseTest input =
  either
    (T.pack . show)
    (T.pack . show)
    $ readExpr input

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnvcode action = runResourceT 
                         $ runReaderT (unEval action) code 

