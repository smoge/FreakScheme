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


{- Note [Eval]
  
  Eval is a monad that will contain the reader monad, which will
  lexical scoping, user input, and will contain the return
  LispVal for any valid expression, done by monad transformers

-}


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

-- | For source File
--   evalFile is used from the main :: IO () loop to run a program file
evalFile :: T.Text -> IO ()
evalFile fileExpr = (runASTinEnv basicEnv $ fileToEvalForm fileExpr) >>= print

-- readExprFile (from Parser.hs) runs the parser on
-- the program text to return a LispVal or ParseError.

-- |  fileToEvalForm runs the parser, if
-- an error occurs, it converts it into a LispException, PError
-- and throws it, else, it evaluates it using evalBody.
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

-- | runASTinEnv executes the the evalBody :: LispVal -> Eval Body with the EnvCtx, essentially
-- running our program by first unwrapping Eval with unEval (the data accessor to Eval),
-- then using the runReaderT and  runResourceT functions on the transformed monad.
runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action =
  runResourceT $
    runReaderT (unEval action) code

{-  Note

- **Primitive Types (LispVal)**: Remember, primitive types like numbers or
  strings evaluate to themselves in Lisp, thanks to the autoquote feature. It's
  a straightforward concept but foundational.

- **The `begin` Function**: It's a way to sequentially evaluate a series of
  S-Expressions. Think of the argument to `begin` as a "body" or
  "body-expression", which can start with `define` statements. This is your
  go-to for executing multiple expressions in order.

- **Using `define`**: This is how you bind values to names. It's like assigning
  a value to a variable in other languages but in the Lisp way, connecting
  evaluated LispVals to Atoms.

- **The `write` Function**: Takes an argument as is (without evaluating it) and
  returns a string representation. It's essentially about converting LispVals to
  readable strings using `showVal`.

- **Conditional `if` Logic**: Works just like in other languages. Evaluate the
  first expression, and based on its truthiness, choose to evaluate either the
  second (if true) or the third (if false).

- **`let` Bindings**: This is a bit tricky. `let` allows for defining local
  bindings by taking a list of atoms and S-Expressions, followed by a "body"
  S-Expression. The catch is the bindings are local, and there's no guaranteed
  order of evaluation for the bindings, so dependencies between them can be
  problematic.

- **Lambda for Anonymous Functions**: This is a powerful concept. `lambda` lets
  you create anonymous functions, taking a list of parameters and a
  body-expression. It's crucial for understanding how functions can be passed
  around and used dynamically in Lisp. The environment and lexical scope
  handling through `EnvCtx` and the `ask` function are key here. Lambda
  essentially embodies the essence of Lambda Calculus, enabling the construction
  of complex functionalities with simple, elegant constructs.

Keep these concepts in mind as they form the backbone of understanding and
working with Scheme and Lisp-like languages. The elegance of Lisp lies in its
simplicity and the power of its constructs like `lambda`, which allows for a
vast range of programming paradigms and techniques. -}

--------- EVAL Function Implementyation !!!!!!!!! ------------

-- Using our aptly named Eval structure, we will define the eval function within Eval.hs as follows:

eval :: LispVal -> Eval LispVal

{- Core of Interpreter (eval Function): The eval function is essentially the
core of our interpreter. It's tasked with pattern matching against every
conceivable valid syntax and special forms in Scheme. This is a hefty
responsibility, indicating the need for a meticulous and structured approach.

Step-by-Step Approach: Given the complexity, we'll dissect the eval function
incrementally, alongside the auxiliary functions that facilitate its operation.
This method might seem a bit fragmented at first glance, but it's the most
effective strategy to demystify the implementation of Scheme's syntax and
semantics within Haskell.

Reference Implementation: For a holistic view of how everything ties together,
refer to Eval.hs. This file will serve as a comprehensive guide and reference
point for understanding the interconnectedness of the components.

Encountering throw: As we delve into the code, you'll encounter several
instances of throw. These are placeholders for exceptions and are elaborated
upon in the subsequent chapter. For the moment, it's important to recognize that
expressions like throw $ LispExceptionConstructor "message-1" are used to return
an Eval LispVal, signaling error states or exceptional conditions within the
evaluation process.

Remember, the journey through the eval function and its supporting cast is
foundational to grasping how Scheme's dynamic and flexible syntax is interpreted
and executed in Haskell. This exploration is not just about implementing an
interpreter but about deeply understanding the mechanics and philosophy behind
programming language design, especially in the context of functional programming
and Lisp dialects -}

-- | Quote is a special form that returns the value of its argument.
--   returns values un-evaluated.
eval (List [Atom "quote", val]) = pure val
--
-- \| Autoquote
eval (Number i) = pure $ Number i
eval (String s) = pure $ String s
eval (Bool b) = pure $ Bool b
eval (List []) = pure Nil
eval Nil = pure Nil
--
{- For List, we have made the evaluation of an empty list go to Nil. This will
be useful for functions that consume the input of a list conditional on the list
having items left.-}
--
-- \| write
--
eval (List [Atom "write", rest]) =
  pure . String . T.pack $ show rest
--
eval (List ((:) (Atom "write") rest)) =
  pure . String . T.pack . show $ List rest
--
{- `write` doesn't evaluate its argument or arguments. Instead, it runs `show`
on them before returning the result as a string. In the second version, we
accept two or more arguments passed to `write` and simply convert them to a
list.-}
--
-- Atom
--
eval n@(Atom _) = getVar n

--
getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
  env <- ask
  case Map.lookup atom env of
    Just x -> pure x
    Nothing -> throw $ UnboundVar atom

{- Now we’re talking! When we evaluate an Atom, we are doing variable lookup.
getVar will do this lookup by getting the EnvCtx via ask, then running a Map
lookup, returning the value if found, else throwing an exception. -}

-- -- IF -- -- !!

eval (List [Atom "if", pred, truExpr, flsExpr]) = do
  ifRes <- eval pred
  case ifRes of
    (Bool True) -> eval truExpr
    (Bool False) -> eval flsExpr
    _ -> throw $ BadSpecialForm "if"

{- Implementing the if Special Form

    Starting with the if Form: We're setting up the well-known if special form.
    The initial step involves evaluating the condition, which we achieve by
    recursively invoking the eval function.\

    Dcision Process: After obtaining the predicate's evaluation result, we
    channel it into a case statement. This is where the decision branches:

        If the result is True, we move on to evaluate the third S-Expression.
        Conversely, if it's False, we evaluate the fourth S-Expression.

    Selective Execution: It's crucial to note that if is designed to evaluate
    only one of its two potential branches (the third or fourth argument), never
    both. This selective execution characteristic underscores the necessity for
    if to be treated as a special form within our implementation.

This methodical approach to implementing if reflects its fundamental role in
conditional logic, ensuring that our interpreter accurately mirrors the behavior
expected from such a special form in Scheme.   -}

--
-- \*LET* --
--

eval (List [Atom "let", List pairs, expr]) = do
  env <- ask
  atoms <- mapM ensureAtom $ getEven pairs
  vals <- mapM eval $ getOdd pairs
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals) <> env
   in local (const env') $ evalBody expr

getEven :: [t] -> [t]
getEven [] = []
getEven (x : xs) = x : getOdd xs

getOdd :: [t] -> [t]
getOdd [] = []
getOdd (x : xs) = getEven xs

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n = throw $ TypeMismatch "atom" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom

--
{- Implementing the `let` Special Form

- **Basics of `let`**: The `let` special form is designed to accept two
  arguments: a list of pairs and an expression. The pairs are structured such
  that an atom occupies the odd positions, and an S-Expression is placed in the
  even positions. An example to illustrate this would be `(let (x 1 y 2) (+ x
  y))`.

- **Creating a New Environment**: When `let` is invoked, it initiates the
  creation of a new environment derived from the current one. This new
  environment is augmented with the bindings specified in the list of pairs.

- **Expression Evaluation**: Within this newly formed environment, the provided
  expression is evaluated as a body-expression. This evaluation process
  leverages the `local` function, which is a part of our monad transformer
  stack.

This setup allows `let` to temporarily introduce new bindings for the duration
of the expression's evaluation, showcasing a powerful mechanism for scoping
within our interpreter. -}

--
-- begin, define & evalBody
--

eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest)) = evalBody $ List rest
eval (List [Atom "define", varExpr, expr]) = do
  varAtom <- ensureAtom varExpr
  evalVal <- eval expr
  env <- ask
  let envFn = const $ Map.insert (extractVar varAtom) evalVal env
   in local envFn $ return varExpr

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
  evalVal <- eval defExpr
  env <- ask
  local (const $ Map.insert var evalVal env) $ eval rest
evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
  evalVal <- eval defExpr
  env <- ask
  let envFn = const $ Map.insert var evalVal env
   in local envFn $ evalBody $ List rest
evalBody x = eval x

{-  --- Understanding the `begin` Special Form and `evalBody` Function

- **`begin` Special Form Overview**: The `begin` special form is versatile,
  designed to handle two types of input structures through pattern matching in
  the evaluation process.

  - **First Form**: It matches a structure like `List[Atom "begin", rest]`,
    where `rest` represents the sequence of expressions to be evaluated and is
    directly passed to `evalBody`.

  - **Second, Expanded Form**: This form matches `List ((:) (Atom "begin")
    rest)`, with `rest` being a list of `LispVal` items. Here, `rest` is
    encapsulated within a `List` data constructor, making it a singular
    `LispVal`, and then it's forwarded to `evalBody`. This flexibility allows
    for the arguments to be provided either as a unified list or as discrete
    arguments, optimizing how single values are handled by ensuring `rest`
    contains two or more values before it's processed by `evalBody`.

- **Integration with `define`**: Beyond these forms, `begin` also prepares for a
  pattern match with `define`, setting the stage for its significance to be
  fully appreciated in the context of `evalBody`.

- **Functionality of `evalBody`**: `evalBody` is pivotal for utilizing the
  `define` statement and evaluating body expressions, which are crucial not just
  for `lambda` and `let`, but also for establishing a standard library. The
  standard library, typically comprised of Scheme files, utilizes numerous
  `define` statements to bind library functions within the environment.

  - **Handling `define`**: `evalBody` discerns lists that contain either a
    single `define` expression followed by at least one other expression, or a
    `define` expression followed by two or more expressions.

    - **For a Single Following Expression**: When `evalBody` encounters a
      `define` followed by one other expression, it matches the `define`,
      updates the environment with the new variable and value, and then
      evaluates the subsequent expression (`rest`) in this updated context, all
      within a `local` function. This serves as the base case.

    - **For Multiple Following Expressions**: If `rest` comprises multiple
      expressions, the process begins similarly with a `define` match and
      environment update. However, instead of a single evaluation, `rest` is
      treated as a list and `evalBody` recursively processes it, allowing for
      the sequential handling of multiple `define` statements.

This structure and processing logic enable the `begin` special form and
`evalBody` function to effectively manage the evaluation of complex expressions
and the integration of `define` statements, foundational for the dynamic
execution of Scheme code and the development of a functional standard library. -}

--
-- lambda & applyLambda

eval (List [Atom "lambda", List params, expr]) = do
  envLocal <- ask
  pure $ Lambda (IFunc $ applyLambda expr params) envLocal
eval (List (Atom "lambda" : _)) = throw $ BadSpecialForm "lambda"

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
  env <- ask
  argEval <- mapM eval args
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) params argEval) <> env
   in local (const env') $ eval expr

-- From LispVal.hs
-- data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

{-

### Implementing the `lambda` Special Form for Function Creation

- **Functionality of `lambda`**: The `lambda` special form is a powerful feature
  that allows users to define their own functions. A key aspect of these
  user-defined functions is lexical scoping, ensuring that variables retain the
  values they were assigned at the time they were captured by the `lambda`.

- **Achieving Lexical Scoping**: To ensure variables within a lambda function
  are lexically scoped, we incorporate a copy of the environment (`EnvCtx`) into
  the `Lambda` data constructor (`Lambda IFunc EnvCtx`). This approach
  guarantees that the function retains access to the environment as it was
  during its definition.

- **Process in `eval`**: When the `lambda` special form is encountered during
  evaluation, the current environment is captured. A `Lambda` data constructor
  is then returned, incorporating both an `IFunc`—which represents the function
  logic via `applyLambda expr params`—and the captured environment.

- **Understanding `applyLambda`**: The `applyLambda` function plays a crucial
  role in this mechanism. It essentially performs partial application of
  function parameters, returning a new function that expects a list of `LispVal`
  arguments. These arguments are then bound to the atomic values specified in
  the parameters list, and the expression (`expr`) passed to `applyLambda` is
  evaluated. This transformation allows us to move from the general form
  `applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal` to a more
  specific form `applyLambda expr params :: [LispVal] -> Eval LispVal`, aligning
  with the expected type for `IFunc`.

- **Lexical Scoping via ReaderT Monad**: The implementation leverages the
  `ReaderT` monad to facilitate lexical scoping. This monadic structure enables
  functions defined with `lambda` to access and manipulate their lexical
  environment, ensuring that variables are correctly scoped according to where
  they were defined, not where the function is called.

This approach to implementing the `lambda` special form not only allows for the
creation of user-defined functions with lexically scoped variables but also
demonstrates the use of functional programming techniques, such as partial
application and monadic structures, to achieve complex scoping mechanisms in a
Haskell-based interpreter.

 -}

--
-- application
--
eval (List ((:) x xs)) = do
  funVar <- eval x
  xVal <- mapM eval xs
  case funVar of
    (Fun (IFunc internalFn)) -> internalFn xVal
    (Lambda (IFunc internalfn) boundenv) ->
      local (const boundenv) $
        internalfn xVal
    _ -> throw $ NotFunction funVar

{-

\*** Understanding Application in Our Interpreter

\**** Application: The Final Form

- **Introduction to Application**: We've reached the concept of application, a
  cornerstone of lambda calculus alongside variables and lambdas. Application
  involves invoking a function with a set of arguments, a fundamental operation
  in functional programming.

- **Pattern Matching for Application**: To perform an application, we start by
  pattern matching on the list's head (the function) and tail (the arguments).
  Both elements are evaluated to determine the function to apply and the
  arguments to pass.

- **Handling Functions**: The function part, identified as `funVar`, is examined
  to determine its nature:

  - **Internal Functions (`Fun`)**: If it's an internal function, we directly
    apply the extracted function (of type `[LispVal] -> Eval LispVal`) to the
    arguments.

  - **User-defined Functions (`Lambda`)**: For a `Lambda`, the function is
    evaluated within its own environment to preserve lexical scoping, ensuring
    variables within the function retain their intended scope.

#### Wrapping Up: The Journey Through Evaluation

- **Reflection**: This exploration into LispVal syntax and Eval semantics has
  been extensive. To see the evaluation mechanism in action, reviewing `Eval.hs`
  is recommended, as it brings together all the concepts discussed.

- **Role of Monad Transformers**: Our implementation leverages monad
  transformers, particularly `ReaderT`, to implement lexical scoping
  effectively. Without monads, managing scope and other functionalities would
  require cumbersome workarounds, such as passing additional arguments through
  every evaluation and helper function.

- **Towards a Practical Interpreter**: While our simple interpreter showcases
  the foundational aspects of language design, optimizing for speed might
  necessitate compilation. However, we've established a solid base for our
  language, potentially paving the way for exploring theoretical properties.

- **Next Steps**: Before diving into primitives and enhancing our interpreter
  with basic operations for data manipulation, we'll address error handling.
  Errors are inevitable in the evaluation process, and our strategy is to throw
  meaningful errors that assist users in troubleshooting.

#### Looking Forward

- **Error Handling**: As we progress, understanding how to effectively
  communicate errors becomes crucial. The goal is to provide users with
  sufficient information to diagnose and resolve issues, ensuring a smoother
  development experience.

- **Anticipation for Primitives**: With the groundwork laid for our interpreter
  and a strategy for error handling in place, we're poised to introduce
  primitives. These will enrich our language with essential operations, moving
  us closer to a practical and interactive programming environment.

This journey through the implementation of an interpreter not only highlights
the intricacies of functional programming and language design but also sets the
stage for further development and exploration.-}

-- [Understanding Check]

-- Implement a delay function as a special form that returns its argument as the
-- body of a lambda expression that accepts no arguments. (delay x) => (lambda
-- () x)

-- You careful read the R5RS standard and are upset to realize we have
-- implemented let incorrectly. Instead of (let (x 1 y 2) (+ x y)) the standard
-- calls for (let ((x 1) (y 2)) (+ x y)). Make this change.

-- GADTs are sometimes used to implement LispVal. How would evaluation change?
-- Implement one of (case, letrec, let*, sequence, do, loop) as a special form.
-- Check the R5RS standard for more information.[Bonus] Find a unique difference
-- in the implementation of special forms between this project and R5RS,
-- implement the special form, then submit a new PR.

-- Alternative Implementations:
-- https://okmij.org/ftp/Haskell/extensible/
-- https://static.aminer.org/pdf/PDF/000/309/870/programming_monads_operationally_with_unimo.pdf
-- https://arxiv.org/pdf/1407.0729.pdf
