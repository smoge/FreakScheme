{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LispVal where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Text as T
import Data.Typeable



{-

Number, Bool e String são exatamente como em Haskell.

- monad transformers:
  - IO
' - Reader






-}
data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  deriving (Eq, Typeable)

instance Show LispVal where
  show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val =
  case val of
    (Atom atom) -> atom
    (String txt) -> T.concat ["\"", txt, "\""]
    (Number num) -> T.pack $ show num
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    Nil -> "'()"
    (List contents) -> T.concat ["(", unwordsList contents, ")"]
    (Fun _) -> "(internal function)"
    (Lambda _ _) -> "(lambda function)"

data IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO
    )

-------------------------------------------------------------
-- Lisp Exception
-------------------------------------------------------------

data LispException
  = NumArgs Integer [LispVal]
  | LengthOfList T.Text Int
  | ExpectedList T.Text
  | TypeMismatch T.Text LispVal
  | BadSpecialForm T.Text
  | NotFunction LispVal
  | UnboundVar T.Text
  | Default LispVal
  | PError String -- from show anyway
  | IOError T.Text

instance Show LispException where
  show = T.unpack . showError

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $ showVal <$> list

showError :: LispException -> T.Text
showError err =
  case err of
    (IOError txt) -> T.concat ["Error reading file: ", txt]
    (NumArgs int args) -> T.concat ["Error Number Arguments, expected ", T.pack $ show int, " recieved args: ", unwordsList args]
    (LengthOfList txt int) -> T.concat ["Error Length of List in ", txt, " length: ", T.pack $ show int]
    (ExpectedList txt) -> T.concat ["Error Expected List in funciton ", txt]
    (TypeMismatch txt val) -> T.concat ["Error Type Mismatch: ", txt, showVal val]
    (BadSpecialForm txt) -> T.concat ["Error Bad Special Form: ", txt]
    (NotFunction val) -> T.concat ["Error Not a Function: ", showVal val]
    (UnboundVar txt) -> T.concat ["Error Unbound Variable: ", txt]
    (PError str) -> T.concat ["Parser Error, expression cannot evaluate: ", T.pack str]
    (Default val) -> T.concat ["Error, Danger Will Robinson! Evaluation could not proceed!  ", showVal val]

{- Handling Errors in Our Interpreter

Special Error Handling for PError

- PError Case: The showError function includes
a specific handling mechanism for PError, which is associated with parsing
errors. This case utilizes a String to encapsulate the error message directly
from the parser, providing a straightforward way to communicate parsing
issues.\]

Dealing with IO Errors
IO Error Complexity: Handling errors originating from IO operations introduces
additional complexity. While our system is equipped to throw an IOError, it's
important to note that unchecked exceptions during IO activities might bypass
our established LispException handling mechanism.

This nuanced approach to error handling ensures that parsing errors are clearly
communicated, while also acknowledging the challenges associated with managing
IO errors within our interpreter's framework.-}

-- TODO !!!!!!-- !!!! TO TO !!!!

-- [ Understanding Check ]

-- Go through Eval.hs, find an LispException used in a few places and replace it
-- with a new error that is more specific, or merge two LispException that are
-- better served by a single constructor. Include support for showError.

-- Many programming languages have information like, “line 4, column 10:
-- variable x is not bound”. How would you go about adding line and column
-- information to messages passed to throwError in Eval.hs?

-- We are taking a basic approach to format error messages: T.concat and show.
-- Text.PrettyPrint offers a rich way to display messages using pretty print
-- combinators. Implement a PrettyPrint interface for LispException that
-- provides a uniform interface.
