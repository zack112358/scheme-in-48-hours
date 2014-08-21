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
import Text.ParserCombinators.Parsec
import Parser hiding (readExpr, main)

main :: IO ()
main = do 
    args <- getArgs
    putStrLn (readExpr (args !! 0))
\end{code}

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

readExpr input = case parse parseExpr "lisp" input of 
    Left err -> "error " ++ show err
    Right val -> "value " ++ show val

\end{code}

\end{document}
