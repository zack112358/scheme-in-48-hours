\documentclass{article}
\usepackage{listings}
\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}

\begin{document}

\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{deadcode}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{shell}{\lstset{language=bash,basicstyle=\small}}{}

\begin{code}
module Repl where 
import System.Environment
import Control.Monad
import Control.Exception
import Control.Monad.Error
import Types
import Env
import Evaluator hiding (main)
import Parser hiding (readExpr, main)
import System.IO
import System.Console.Readline


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO (Maybe String)
readPrompt prompt = readline prompt

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

evalString :: Env -> String -> IO String
evalString env exprString = 
  runIOThrows $ liftM show $ (liftThrows $ readExpr exprString) >>= eval env

evalPrint :: Env -> String -> IO ()
evalPrint env exprString = evalString env exprString >>= putStrLn

untilEOF :: IO (Maybe a) -> (a -> IO ()) -> IO ()
untilEOF prompt action = do
  result <- prompt
  case result of
    Nothing -> return ()
    Just result -> (action result >> untilEOF prompt action)

runOnce :: String -> IO ()
runOnce expr = newEnv >>= flip evalPrint expr

runRepl :: IO ()
runRepl = newEnv >>= untilEOF (readPrompt "❨-❩ ") . evalPrint

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> (runRepl >> putStrLn "Quit")
            [str] -> runOnce str
            otherwise -> putStrLn "Program takes at most 1 argument"



\end{code}


\end{document}


