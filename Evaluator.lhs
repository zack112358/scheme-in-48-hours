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

context = [("+", binPlusOp $ foldl1 (+)),
           ("-", binPlusOp $ foldl1 (-)),
           ("*", binPlusOp $ foldl1 (*)),
           ("/", binPlusOp $ foldl1 div),
           ("mod", binPlusOp $ foldl1 mod),
           ("quotient", binPlusOp $ foldl1 quot),
           ("remainder", binPlusOp $ foldl1 rem),
           ("symbol->string", PrimitiveOp symbolToString),
           ("string->symbol", PrimitiveOp stringToSymbol)]

binPlusOp :: ([LispVal] -> LispVal) -> LispVal
binPlusOp f = PrimitiveOp $
    \args -> case args of [] -> throwError $ NumArgs 2 $ List []
                          [arg] -> throwError $ NumArgs 2 $ List [arg]
                          args -> return $ f args

symbolToString::[LispVal] -> ThrowsError LispVal
symbolToString [(Atom s)] = return $ String s

stringToSymbol::[LispVal] -> ThrowsError LispVal
stringToSymbol [(String s)] = return $ Atom s

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


