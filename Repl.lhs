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
import Evaluator hiding (main)
import Parser hiding (readExpr, main)
import System.IO


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString exprString = 
  return $ extractValue $ trapError (liftM show $ readExpr exprString >>= eval)

evalPrint :: String -> IO ()
evalPrint exprString = evalString exprString >>= putStrLn

untilEOF :: IO a -> (a -> IO ()) -> IO ()
untilEOF prompt action = do
  result <- try prompt
  case result of
    Left err -> do _ <- return $ show (err::IOException)
                   putStrLn ""
                   return ()
    Right result -> (action result >> untilEOF prompt action)

runRepl :: IO ()
runRepl = untilEOF (readPrompt "Scheme> ") evalPrint

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> runRepl
            [str] -> evalPrint str
            otherwise -> putStrLn "Program takes only 0 or 1 argument"


\end{code}


\end{document}


