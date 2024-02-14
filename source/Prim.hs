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
    ("/", mkF $ binop divOp),
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
    ("slurp", mkF $ unop slurp),
    ("nil?", mkF nilCheck)
  ]

-- -- For example, let's use it for the "+" function
-- ("+", Lambda ["x", "y"] [Plus (Var "x") (Var "y")])
----------------------------
-- Primitive Creation
----------------------------

divOp :: LispVal -> LispVal -> Eval LispVal
divOp (Number x) (Number y)
  | y == 0 = throw $ DivByZero "division by zero"
  | otherwise = return $ Number $ x `div` y
divOp _ _ = throw $ TypeMismatch "expected two numbers"

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

binopFold1 :: Binary -> [LispVal] -> Eval LispVal
binopFold1 _ [] = throw $ NumArgs 1 []
binopFold1 op (x : xs) = foldM op x xs

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

--
--
--
------------------------
--  IO Functions
------------------------

fileExists :: LispVal -> Eval LispVal
fileExists (Atom atom) = fileExists $ String atom
fileExists (String txt) = Bool <$> liftIO (doesFileExist $ T.unpack txt)
fileExists val = throw $ TypeMismatch "expects str, got: " val

slurp :: LispVal -> Eval LispVal
slurp (String txt) = liftIO $ wFileSlurp txt
slurp val = throw $ TypeMismatch "expects str, got:" val

wFileSlurp :: T.Text -> IO LispVal
wFileSlurp fileName = withFile (T.unpack fileName) ReadMode go
  where
    go = readTextFile fileName

readTextFile :: T.Text -> Handle -> IO LispVal
readTextFile fileName handle = do
  exists <- hIsEOF handle
  if exists
    then (TIO.hGetContents handle) >>= (return . String)
    else throw $ IOError $ T.concat [" file does not exits: ", fileName]

{- The fundamental file operations include `slurp`, which reads the contents of
a file into a string, and `fileExists`, which checks for the presence of a file
and returns a boolean indicating whether the file exists. -}

-- List Comprehension

cons :: [LispVal] -> Eval LispVal
cons [x, y@(List yList)] = return $ List $ x : yList
cons [c] = return $ List [c]
cons [] = return $ List []
cons _ = throw $ ExpectedList "cons, in second argumnet"

car :: [LispVal] -> Eval LispVal
car [List []] = return Nil
car [List (x : _)] = return x
car [] = return Nil
car x = throw $ ExpectedList "car"

cdr :: [LispVal] -> Eval LispVal
cdr [List (x : xs)] = return $ List xs
cdr [List []] = return Nil
cdr [] = return Nil
cdr x = throw $ ExpectedList "cdr"

{- Given that S-Expressions form the core syntax of Scheme, list comprehension
operators play a crucial role in the primitive environment. In our
implementation, these operators do not utilize the `unop` or `binop` helper
functions due to the necessity of supporting variadic arguments (varargs). An
alternative method could involve treating these operators as special forms;
however, special forms are characterized by their unique argument evaluation
behavior. Therefore, it is more appropriate to classify these list comprehension
operators as primitives, aligning with their standard argument evaluation
process. -}

{- --=-=--=-=-=-=-=-=-=-=-=-=-=--=-=-=
Unary and Binary Function Handlers
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= -}

numBool :: (Integer -> Bool) -> LispVal -> Eval LispVal
numBool op (Number x) = return $ Bool $ op x
numBool op x = throw $ TypeMismatch "numeric op " x

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x y
numOp op x (Number y) = throw $ TypeMismatch "numeric op " x
numOp op (Number x) y = throw $ TypeMismatch "numeric op " y
numOp op x y = throw $ TypeMismatch "numeric op " x

strOp :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strOp op (String x) (String y) = return $ String $ op x y
strOp op x (String y) = throw $ TypeMismatch "string op " x
strOp op (String x) y = throw $ TypeMismatch "string op " y
strOp op x y = throw $ TypeMismatch "string op " x

eqOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqOp op (Bool x) (Bool y) = return $ Bool $ op x y
eqOp op x (Bool y) = throw $ TypeMismatch "bool op " x
eqOp op (Bool x) y = throw $ TypeMismatch "bool op " y
eqOp op x y = throw $ TypeMismatch "bool op " x

numCmp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
numCmp op (Number x) (Number y) = return . Bool $ op x y
numCmp op x (Number y) = throw $ TypeMismatch "numeric op " x
numCmp op (Number x) y = throw $ TypeMismatch "numeric op " y
numCmp op x y = throw $ TypeMismatch "numeric op " x

eqCmd :: LispVal -> LispVal -> Eval LispVal
eqCmd (Atom x) (Atom y) = return . Bool $ x == y
eqCmd (Number x) (Number y) = return . Bool $ x == y
eqCmd (String x) (String y) = return . Bool $ x == y
eqCmd (Bool x) (Bool y) = return . Bool $ x == y
eqCmd Nil Nil = return $ Bool True
eqCmd _ _ = return $ Bool False

{- These helper functions are essential for adapting Haskell functions to work
with `LispVal` arguments through pattern matching. Additionally, this pattern
matching facilitates dynamic function dispatch at runtime, based on the data
constructor of the arguments. This capability is a hallmark of dynamically typed
languages and contributes to their slower performance relative to statically
typed languages. Another critical aspect of these functions is their ability to
throw errors when encountering incorrect or mismatched types for function
arguments. This mechanism prevents Haskell's type errors and enables error
handling that supports detailed error reporting. For instance, attempting to
execute `(+ 1 "a")` would result in an error message. -}

{- The primitive environment is established by encapsulating a Haskell function
within a helper function. This helper function employs pattern matching on
`LispVals` to extract the underlying values compatible with the Haskell
function. Subsequently, this function is transformed to match the type
`[LispVal] -> Eval LispVal`, corresponding to our `IFunc` type, which serves as
the exclusive argument for the `LispVal` data constructor `Fun`. This process
enables the association of a `Text` value with a `LispVal` that represents a
function, forming our primitive environment. The aim is to maintain a minimal
core of primitive functions, delegating the addition of new functions to the
standard library whenever they can be constructed using existing primitives. -}

-- Implement a new primitive function nil? which accepts a single argument, a
-- list, returns true if the list is empty, and false under all other
-- situations.

-- Use the Lambda constructor instead of Fun for one of the primitive functions.
-- Create a Division primitive function which works for Number, returning a
-- Number by dividing then rounding down.

nilCheck :: [LispVal] -> Eval LispVal
nilCheck [List []] = return $ Bool True
nilCheck _ = return $ Bool False

binopFold1 :: Binary -> [LispVal] -> Eval LispVal
binopFold1 _ [] = throw $ NumArgs 1 []
binopFold1 op (x : xs) = foldM op x xs
