{-# LANGUAGE OverloadedStrings #-}

module Prim (primEnv, unop) where

import Control.Exception (throw)
import Control.Monad.Except (MonadIO (liftIO), foldM)
import Data.Functor ((<&>))
import Data.Text as T (Text, concat, pack, unpack)
import Data.Text.IO as TIO (hGetContents, hPutStr)
import LispVal
  ( Eval,
    IFunc (IFunc),
    LispException (ExpectedList, IOError, NumArgs, TypeMismatch),
    LispVal (Atom, Bool, Fun, List, Nil, Number, String),
  )
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import System.Directory (doesFileExist)
import System.IO
  ( Handle,
    IOMode (ReadMode, WriteMode),
    hIsWritable,
    withFile,
  )

{- Our approach involves constructing a list of tuples, `[(T.Text, LispVal)]`,
where each tuple pairs a primitive's name (as `T.Text`) with a `LispVal` that
encapsulates an internal function (`Fun`). This list is then transformed into a
`Map` using `Map.fromList`, forming our evaluation environment. To bridge
Haskell functions to our Lisp environment, we map Haskell functions of various
types to the form `[LispVal] -> Eval LispVal`. This requires pattern matching to
extract LispVals of appropriate types, retrieving their values, and then
applying the Haskell function. We've developed helper functions like `binop`,
`binopFold`, and `unop` to streamline this process. Due to the complexity of
explaining this verbally, I'll demonstrate with an example in the following
sections to clarify how we simplify the type signatures.-}

mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF = Fun . IFunc

primEnv :: Prim
primEnv =
  [ ("+", mkF $ binopFold (numOp (+)) (Number 0)),
    ("*", mkF $ binopFold (numOp (*)) (Number 1)),
    ("++", mkF $ binopFold (strOp (<>)) (String "")),
    ("-", mkF $ binop $ numOp (-)),
    ("<", mkF $ binop $ numCmp (<)),
    ("<=", mkF $ binop $ numCmp (<=)),
    (">", mkF $ binop $ numCmp (>)),
    (">=", mkF $ binop $ numCmp (>=)),
    ("==", mkF $ binop $ numCmp (==)),
    ("even?", mkF $ unop $ numBool even),
    ("odd?", mkF $ unop $ numBool odd),
    ("pos?", mkF $ unop $ numBool (< 0)),
    ("neg?", mkF $ unop $ numBool (> 0)),
    ("eq?", mkF $ binop eqCmd),
    ("bl-eq?", mkF $ binop $ eqOp (==)),
    ("and", mkF $ binopFold (eqOp (&&)) (Bool True)),
    ("or", mkF $ binopFold (eqOp (||)) (Bool False)),
    ("cons", mkF Prim.cons),
    ("cdr", mkF Prim.cdr),
    ("car", mkF Prim.car),
    ("file?", mkF $ unop fileExists),
    ("slurp", mkF $ unop slurp)
  ]

----------------------------
-- Primitive Creation
----------------------------

-- Lets go through an individual example to see how all the types mash!

-- Function Definition

type Binary = LispVal -> LispVal -> Eval LispVal

(+) :: (Num a) => a -> a -> a
numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal

{- Indeed, the transformation process is intricate, yet the types align correctly
upon closer examination. The key engineering strategy employed involves
leveraging the `numOp` function, along with similar functions, across a broad
range of operators. This approach significantly minimizes the volume of code
required. Additionally, the `binop` and `unop` functions offer reusability for
the majority of operations. Handling variadic arguments, however, necessitates a
distinct approach, likely involving individual pattern matching for each case.
-}

--
-- Helper Functions
--

type Prim = [(T.Text, LispVal)]

type Unary = LispVal -> Eval LispVal

type Binary = LispVal -> LispVal -> Eval LispVal

unop :: Unary -> [LispVal] -> Eval LispVal
unop op [x] = op x
unop _ args = throw $ NumArgs 1 args

binop :: Binary -> [LispVal] -> Eval LispVal
binop op [x, y] = op x y
binop _ args = throw $ NumArgs 2 args

binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op farg args = case args of
  [a, b] -> op a b
  (a : as) -> foldM op farg args
  [] -> throw $ NumArgs 2 args




  {- The functions `binop`, `unop`, and `binopFold` serve as unwrappers that
  process a list of `LispVal` arguments and apply these to a given operator.
  `binopFold` specifically employs `foldM`, incorporating an extra argument in
  its operation. It's important to highlight that for `binopFold` to function
  correctly, the operator in question must be compatible with monoids -}