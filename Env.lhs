\documentclass{article}
\usepackage{listings}
\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\begin{document}

\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{deadcode}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{shell}{\lstset{language=bash,basicstyle=\small}}{}

Common types used in the interpreter.

\begin{code}

module Env where
import Control.Monad
import Control.Monad.Error
import Data.IORef
import Text.ParserCombinators.Parsec
import Types

isBound :: Env -> String -> IO Bool
isBound envRef id = readIORef envRef
  >>= return . maybe False (const True) . lookup id

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef id = (liftIO $ readIORef envRef)
  >>= maybe (throwError $ UnboundVar id)
            (liftIO . readIORef)
      . lookup id

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef id value = (liftIO $ readIORef envRef)
  >>= maybe (throwError $ UnboundVar id)
            (liftIO . (flip writeIORef value))
      . lookup id
  >> return value

alt :: a -> a -> Bool -> a
alt a b True = a
alt a b False = b

bindVar :: Env -> String -> LispVal -> IO LispVal
bindVar envRef id value = do
  env <- readIORef envRef
  valueRef <- newIORef value
  writeIORef envRef ((id, valueRef):env)
  return value

-- Although it would be more performant to define a defineVar that is equivalent
-- to setVar under the appropriate circumstances, our Env type doesn't make it
-- possible for us to tell when we should hide the old var and when we should
-- overwrite it. When this is fixed defineVar can get smarter.
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar e i v = liftIO $ bindVar e i v

-- Create a *new* environment without overwriting the existing ref. Bind the
-- given vars in it.
withBoundVars :: [(String, LispVal)] -> Env -> IO Env
withBoundVars bindings envRef =
  readIORef envRef >>= newIORef
  >>= \env -> mapM (uncurry (bindVar env)) bindings
  >> return env




\end{code}


\end{document}
