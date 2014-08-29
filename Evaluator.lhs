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
import Parser hiding (readExpr, main)

nullEnv :: IO Env
nullEnv = newIORef []

builtins = [("+", numBinPlusOp $ foldl1 (+)),
            ("-", numBinPlusOp $ foldl1 (-)),
            ("*", numBinPlusOp $ foldl1 (*)),
            ("/", numBinPlusOp $ foldl1 div),
            ("mod", numBinPlusOp $ foldl1 mod),
            ("quotient", numBinPlusOp $ foldl1 quot),
            ("remainder", numBinPlusOp $ foldl1 rem),
            ("symbol->string", PrimitiveOp symbolToString),
            ("string->symbol", PrimitiveOp stringToSymbol),
            ("=", boolBinNumOp (==)),
            ("<", boolBinNumOp (<)),
            (">", boolBinNumOp (>)),
            ("/=", boolBinNumOp (/=)),
            (">=", boolBinNumOp (>=)),
            ("<=", boolBinNumOp (<=)),
            ("&&", PrimitiveOp opAnd),
            ("||", PrimitiveOp opOr),
            ("string=?", boolBinStrOp (==)),
            ("string<?", boolBinStrOp (<)),
            ("string>?", boolBinStrOp (>)),
            ("string<=?", boolBinStrOp (<=)),
            ("string>=?", boolBinStrOp (>=))]

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

numBinPlusOp :: ([LispVal] -> LispVal) -> LispVal
numBinPlusOp f = PrimitiveOp $
  \args -> case args of [] -> throwError $ NumArgs 2 $ List []
                        [arg] -> throwError $ NumArgs 2 $ List [arg]
                        args -> typeCheckArgs isNumber "Number" args
                                >> (return $ f args)

boolBinOp :: (LispVal -> Bool) -> String -> (LispVal -> LispVal -> Bool) -> LispVal
boolBinOp typePred typeName f = PrimitiveOp $ \args ->
  case args of
    [l, r] -> typeCheckArgs typePred typeName args
              >> (return $ Bool $ f l r)
    _ -> throwError $ TypeMismatch ("(" ++ typeName ++ " " ++ typeName ++ ")")
                    $ List args

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

trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
extractValue (Right result) = result

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val    -- Strings, bools, numbers eval to selves
eval env val@(Bool _) = return val
eval env val@(Number _) = return val
eval env (List [Atom "quote", val]) = return val  -- Quoted lists eval to the quoted thing
eval env (List [Atom "define", Atom id, expr]) =
  eval env expr >>= defineVar env id
eval env (List [Atom "set!", Atom id, expr]) =
  eval env expr >>= setVar env id
eval env (List [Atom "if", cond, positive, negative]) =
  do safeCond <- eval env cond
     if safeCond == (Bool False)
       then eval env negative
       else eval env positive
eval env (List (func@(Atom _) : args)) =
  eval env func
  >>= \safefunc -> mapM (eval env) args
  >>= applyLambda safefunc
eval env (Atom id) = getVar env id
eval env other = throwError $ BadSpecialForm "Unrecognized special form" other

applyLambda :: LispVal -> [LispVal] -> IOThrowsError LispVal
applyLambda (PrimitiveOp f) args = liftThrows $ f args

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

-- Action to create a new clean environment
newEnv :: IO Env
newEnv = nullEnv >>= flip withBoundVars builtins

\end{code}




\end{document}


