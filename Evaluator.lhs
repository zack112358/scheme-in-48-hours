\documentclass{article}
\usepackage{listings}
\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}

\begin{document}

\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{deadcode}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{shell}{\lstset{language=bash,basicstyle=\small}}{}

\begin{code}
module Evaluator where 
import System.Environment
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Parser hiding (readExpr, main)

\end{code}


\begin{deadcode}
main :: IO ()
main = do 
    args <- getArgs
    putStrLn (readExpr (args !! 0))
\end{deadcode}

Currently, we've just been printing out whether or not we recognize the given
program fragment. We're about to take the first steps towards a working Scheme
interpreter: assigning values to program fragments. We'll be starting with baby
steps, but fairly soon you'll be progressing to doing working computations.
Let's start by telling Haskell how to print out a string representation of the
various possible LispVals:

\begin{code}
if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Bool b) = if' b "#t" "#f"
showVal (Number n) = show n
showVal (Atom id) = id
showVal (List list) = "(" ++ unwords(map showVal list) ++ ")"
showVal (DottedList list tail) =
    "(" ++ unwords(map showVal list) ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

\end{code}

\begin{deadcode}
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> "error " ++ show err
    Right val -> "value " ++ show val

\end{deadcode}

Beginnings of an evaluator: Primitives

Now, we start with the beginnings of an evaluator. The purpose of an evaluator
is to map some "code" data type into some "data" data type, the result of the
evaluation. In Lisp, the data types for both code and data are the same, so our
evaluator will return a LispVal. Other languages often have more complicated
code structures, with a variety of syntactic forms.

Evaluating numbers, strings, booleans, and quoted lists is fairly simple: return
the datum itself.

\begin{deadcode} 
eval :: LispVal -> LispVal
eval val@(String _) = val    -- Strings, bools, numbers eval to selves
eval val@(Bool _) = val
eval val@(Number _) = val
eval (List [Atom "quote", val]) = val  -- Quoted lists eval to the quoted thing
\end{deadcode}

Let's integrate eval into our existing code. Start by changing readExpr back so
it returns the expression instead of a string representation of the expression:

\begin{code} 
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val
\end{code}

And we change main to eval before printing

\begin{code}
main :: IO ()
main = getArgs >>= print . eval . readExpr . head
\end{code}

Now we can evaluate values to\ldots themselves! Great! Let's add function
application to make it interesting.

Begin by adding a clause to eval to handle function application. Remember that
all clauses of a function definition must be placed together and are evaluated
in textual order, so this should go after the quoted list clause.

\begin{code}
eval :: LispVal -> LispVal
eval val@(String _) = val    -- Strings, bools, numbers eval to selves
eval val@(Bool _) = val
eval val@(Number _) = val
eval (List [Atom "quote", val]) = val  -- Quoted lists eval to the quoted thing
eval (List (func@(Atom _) : args)) = applyLambda (eval func) $ map eval args
eval (Atom id) = case lookup id context of
                    Nothing -> undefined
                    Just f  -> PrimitiveOp f

applyLambda :: LispVal -> [LispVal] -> LispVal
applyLambda (PrimitiveOp f) args = f args

context :: [(String, [LispVal] -> LispVal)]
context = [("+", foldl1 (+)),
           ("-", foldl1 (-)),
           ("*", foldl1 (+)),
           ("/", foldl1 div),
           ("mod", foldl1 mod),
           ("quotient", foldl1 quot),
           ("remainder", foldl1 rem),
           ("symbol->string", symbolToString),
           ("string->symbol", stringToSymbol)]

symbolToString [(Atom s)] = String s
stringToSymbol [(String s)] = Atom s


\end{code}

Of course, if we're going to go around adding LispVals together we need to make
them members of some classes.

\begin{code}

instance Eq LispVal where
    (==) (Atom l) (Atom r) = (==) l r
    (==) (List l) (List r) = (==) l r
    (==) (Number l) (Number r) = (==) l r
    (==) (String l) (String r) = (==) l r
    (==) (Bool l) (Bool r) = (==) l r

instance Enum LispVal where
    fromEnum (Number x) = fromEnum x
    toEnum x = Number $ toInteger x

instance Ord LispVal where
    compare (Atom l) (Atom r) = compare l r
    compare (List l) (List r) = compare l r
    compare (Number l) (Number r) = compare l r
    compare (String l) (String r) = compare l r
    compare (Bool l) (Bool r) = compare l r

instance Num LispVal where
    (+) (Number l) (Number r) = Number $ (+) l r
    (-) (Number l) (Number r) = Number $ (-) l r
    (*) (Number l) (Number r) = Number $ (*) l r
    fromInteger x = Number x
    abs (Number x) = Number $ abs x
    signum (Number x) = Number $ signum x

instance Real LispVal where
    toRational (Number x) = toRational x  -- Breaks out of LispVal

instance Integral LispVal where
    mod (Number l) (Number r) = Number $ mod l r
    quot (Number l) (Number r) = Number $ quot l r
    rem (Number l) (Number r) = Number $ rem l r
    quotRem l r = (quot l r, rem l r)
    toInteger (Number x) = x   -- Breaks out of LispVal

\end{code}

Now we can throw around LispVals in Haskell as well as lisp, which should make
our code a little easier to write.


Currently, there are a variety of places within the code where we either ignore errors or silently assign "default" values like #f or 0 that make no sense. Some languages – like Perl and PHP – get along fine with this approach. However, it often means that errors pass silently throughout the program until they become big problems, which means rather inconvenient debugging sessions for the programmer. We'd like to signal errors as soon as they happen and immediately break out of execution.
First, we need to import Control.Monad.Error to get access to Haskell's built-in
error functions. Then, we should define a data type to represent an error:

\begin{code}
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               | Unimplemented

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default s)                   = s
showError (Unimplemented)               = "Unimplemented"
 
instance Show LispError where show = showError
\end{code}

Our next step is to make our error type into an instance of Error. This is
necessary for it to work with GHC's built-in error handling functions. Being an
instance of error just means that it must provide functions to create an
instance either from a previous error message or by itself:

\begin{code}
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default
\end{code}

Then we define a type to represent functions that may throw a LispError or
return a value. Remember how parse used an Either data type to represent
exceptions? We take the same approach here:

\begin{code}
type ThrowsError = Either LispError
\end{code}


\end{document}


