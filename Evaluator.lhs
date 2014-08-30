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
import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Monad.Error
import Data.IORef
import Types
import Env
import Parser

nullEnv :: IO Env
nullEnv = newIORef []

builtins = [numBinPlusOp "+" $ foldl1 (+),
            numBinPlusOp "-" $ foldl1 (-),
            numBinPlusOp "*" $ foldl1 (*),
            numBinPlusOp "/" $ foldl1 div,
            numBinPlusOp "mod" $ foldl1 mod,
            numBinPlusOp "quotient" $ foldl1 quot,
            numBinPlusOp "remainder" $ foldl1 rem,
            ("symbol->string", PrimitiveOp "symbol->string" symbolToString),
            ("string->symbol", PrimitiveOp "string->symbol" stringToSymbol),
            boolBinNumOp "=" (==),
            boolBinNumOp "<" (<),
            boolBinNumOp ">" (>),
            boolBinNumOp "/=" (/=),
            boolBinNumOp ">=" (>=),
            boolBinNumOp "<=" (<=),
            ("&&", PrimitiveOp "&&" opAnd),
            ("||", PrimitiveOp "||" opOr),
            boolBinStrOp "string=?" (==),
            boolBinStrOp "string<?" (<),
            boolBinStrOp "string>?" (>),
            boolBinStrOp "string<=?" (<=),
            boolBinStrOp "string>=?" (>=)]

\end{code}
The typecheck function will check from the given spec of what argument types
should be, whether they are actually that type.

The type argument specifies what type they should be, and should be a predicate
function checking for that.
\begin{code}

typeCheckArgs :: (LispVal -> Bool) -> String -> [LispVal] -> ThrowsError ()
typeCheckArgs _ _ [] = return ()
typeCheckArgs typePred typeName (arg:args) =
  if typePred arg
    then typeCheckArgs typePred typeName args
    else throwError $ TypeMismatch typeName arg

-- Convert an n-ary numerical fun into name, lambda pair
numBinPlusOp :: String -> ([LispVal] -> LispVal) -> (String, LispVal)
numBinPlusOp name f = (name, PrimitiveOp name $
  \args -> case args of [] -> throwError $ NumArgs 2 $ List []
                        [arg] -> throwError $ NumArgs 2 $ List [arg]
                        args -> typeCheckArgs isNumber "Number" args
                                >> (return $ f args))

-- Convert a function that takes args of given type into a name, lambda pair
boolBinOp :: (LispVal -> Bool) -> String -> String -> (LispVal -> LispVal -> Bool) -> (String, LispVal)
boolBinOp typePred typeName name f = (name, PrimitiveOp name $ \args ->
  case args of
    [l, r] -> typeCheckArgs typePred typeName args
              >> (return $ Bool $ f l r)
    _ -> throwError $ TypeMismatch ("(" ++ typeName ++ " " ++ typeName ++ ")")
                    $ List args)

boolBinBoolOp = boolBinOp isBool "Bool"
boolBinNumOp = boolBinOp isNumber "Number"
boolBinStrOp = boolBinOp isString "String"

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [(Atom s)] = return $ String s
symbolToString args = throwError $ TypeMismatch "(Atom)" $ List args

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [(String s)] = return $ Atom s
stringToSymbol args = throwError $ TypeMismatch "(String)" $ List args

opAnd [(Bool l), (Bool r)] = return $ Bool (l && r)
opAand args = throwError $ TypeMismatch "(Bool Bool)" $ List args
opOr [(Bool l), (Bool r)] = return $ Bool (l || r)
opOr args = throwError $ TypeMismatch "(Bool Bool)" $ List args

eval :: Env -> LispVal -> IOThrowsError LispVal

-- Literals eval to themselves
eval env val@(String _) = return val
eval env val@(Bool _) = return val
eval env val@(Number _) = return val

-- Quoted lists eval to the quoted thing
eval env (List [Atom "quote", val]) = return val

-- Define a var
eval env (List [Atom "define", Atom id, expr]) =
  eval env expr >>= defineVar env id

-- Lambda define shorthand
eval env (List [Atom "define", List ((Atom id):formals), body]) =
  defineVar env id (Lambda (List formals) body env)
eval env (List [Atom "define", DottedList ((Atom id):formals) rest, body]) =
  defineVar env id (Lambda (DottedList formals rest) body env)

-- Setting vars to values
eval env (List [Atom "set!", Atom id, expr]) =
  eval env expr >>= setVar env id

-- Branching
eval env (List [Atom "if", cond, positive, negative]) =
  do safeCond <- eval env cond
     if safeCond == (Bool False)
       then eval env negative
       else eval env positive

-- Lambdas
eval env (List [Atom "lambda", formal, body]) =
  return $ Lambda formal body env
eval env (List (func : args)) =
  eval env func
  >>= \safefunc -> mapM (eval env) args
  >>= apply safefunc

-- Variables eval to their values
eval env (Atom id) = getVar env id

-- Anything else is broken
eval env other = throwError $ BadSpecialForm "Unrecognized special form" other

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveOp name f) args = liftThrows $ f args
apply (Lambda formal body env) args = do
  bindings <- liftThrows $ formalBindings formal (List args)
  env <- liftIO nullEnv
  boundEnv <- liftIO $ withBoundVars bindings env
  eval boundEnv body
apply (IOOp name f) args = f args
apply notFun args = throwError $ TypeMismatch "lambda" notFun
  

-- Take the given formal and the given args list and create a list of new
-- bindings to add to the environment
formalBindings :: LispVal -> LispVal -> ThrowsError [(String, LispVal)]
formalBindings (Atom id) arg = return [(id, arg)]
formalBindings (List (formal:formals)) (List (arg:args)) = do
  first <- formalBindings formal arg
  rest <- formalBindings (List formals) (List args) 
  return $ first ++ rest
formalBindings (List []) (List []) = return []
formalBindings (DottedList formals formal)
               (DottedList args arg) = do
  last <- formalBindings formal arg
  rest <- formalBindings (List formals) (List args)
  return $ rest ++ last
formalBindings (DottedList (formal:formals) rest)
               (List (arg:args)) = do
  first <- formalBindings formal arg
  rest <- formalBindings (DottedList formals rest) (List args)
  return $ first ++ rest
formalBindings (DottedList [] rest) arg =
  formalBindings rest arg
formalBindings x@(Number _) y@(Number _) = 
  if x == y then return [] else throwError $ PatternMatch x y
formalBindings x@(String _) y@(String _) = 
  if x == y then return [] else throwError $ PatternMatch x y
formalBindings x@(Bool _) y@(Bool _) = 
  if x == y then return [] else throwError $ PatternMatch x y


formalBindings formals args = throwError $ PatternMatch formals args
    

-- Action to create a new clean environment
newEnv :: IO Env
newEnv = nullEnv >>= withBoundVars builtins

\end{code}


\end{document}


