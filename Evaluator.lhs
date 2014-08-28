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
import Types
import Parser hiding (readExpr, main)

context :: [(String, LispVal)]

context = [("+", numBinPlusOp $ foldl1 (+)),
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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val    -- Strings, bools, numbers eval to selves
eval val@(Bool _) = return val
eval val@(Number _) = return val
eval (List [Atom "quote", val]) = return val  -- Quoted lists eval to the quoted thing
eval (List (func@(Atom _) : args)) = eval func
                                     >>= \safefunc -> mapM eval args
                                     >>= applyLambda safefunc
eval (Atom id) = case (lookup id context) of
                   Nothing -> throwError $ UnboundVar "No such id" id
                   Just val -> return val
eval other = throwError $ BadSpecialForm "Unrecognized special form" other

applyLambda :: LispVal -> [LispVal] -> ThrowsError LispVal
applyLambda (PrimitiveOp f) = f

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do args <- getArgs
          evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
          putStrLn $ extractValue $ trapError evaled

\end{code}




\end{document}


