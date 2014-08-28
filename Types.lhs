\documentclass{article}
\usepackage{listings}
\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\begin{document}

\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{deadcode}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{shell}{\lstset{language=bash,basicstyle=\small}}{}

Common types used in the interpreter.

\begin{code}

module Types where 
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec

\end{code}
The \verb+LispVal+ type encapsulates values manipulable by the Scheme
environment. We also make \verb+LispVal+ an instance of a variety of nifty
classes below, which allows us to manipulated it "from above" in Haskell much as
we can manipulated it in Scheme.

The departure from the Scheme-in-48-hours recipe is the class membership, as
well as the PrimitiveOp alternate, which is used to bless Haskell functions as
Scheme lambdas.
\begin{code}

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveOp ([LispVal] -> ThrowsError LispVal)

isAtom (Atom _) = True
isAtom _ = False
isList (List _) = True
isList (DottedList _ _) = True
isList _ = False
isNumber (Number _) = True
isNumber _ = False
isString (String _) = True
isString _ = False
isBool (Bool _) = True
isBool _ = False
isPrimitiveOp (PrimitiveOp _) = True
isPrimitiveOp _ = False

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Bool b) = if b then "#t" else "#f"
showVal (Number n) = show n
showVal (Atom id) = id
showVal (List list) = "(" ++ unwords(map showVal list) ++ ")"
showVal (DottedList list tail) =
    "(" ++ unwords(map showVal list) ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

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


data LispError = NumArgs Integer LispVal
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
                                       ++ " args; found values " ++ show found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default s)                   = s
showError (Unimplemented)               = "Unimplemented"
 
instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

\end{code}


\end{document}
