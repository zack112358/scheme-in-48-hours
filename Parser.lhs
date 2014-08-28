\documentclass{article}
\usepackage{listings}
\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\begin{document}

\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{deadcode}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{shell}{\lstset{language=bash,basicstyle=\small}}{}

Simple parser using Parsec.

\begin{code}

module Parser where 
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces) -- spaces we define
                                                     -- ourselves
import System.Environment
import Control.Monad
import Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom = (letter <|> symbol) >>= \first ->
            many(letter <|> digit <|> symbol) >>= \rest ->
            let atom = first:rest in 
            return $ case atom of "#t" -> Bool True
                                  "#f" -> Bool False
                                  _    -> Atom atom

parseNumber = many1 digit >>= \digitStr -> return$Number$read digitStr

parseString :: Parser LispVal
parseString = char '"'
              >> many(noneOf "\\\""
                      <|> (char '\\' >> (char '"'
                                         <|> char '\\'
                                         <|> (char 'n' >> return '\n')
                                         <|> (char 'r' >> return '\r')
                                         <|> (char 't' >> return '\t'))))
              >>= \x -> char '"' >> (return $ String x)

-- softSepBy does not require going on to consume the item after consuming the
-- separator.
softSepBy :: Parser a -> Parser b -> Parser [a]
softSepBy item sep = optional sep
                    >> ((item >>= \first -> softSepBy item sep
                              >>= \rest -> return (first:rest))
                         <|> return [])

-- Parse a list or dottedlist.
parseList :: Parser LispVal
parseList = 
    char '('
    >> skipMany space
    >> softSepBy parseExpr spaces  -- ordinary sepBy gets stuck on space in (1 . 2)
    >>= \head -> skipMany space -- Soak any irrelevant space
    >> ((char ')' >> (return$List head)) -- End here or continue into dotted list
        <|> (char '.'
             >> skipMany space
             >> parseExpr 
             >>= \tail -> skipMany space
             >> char ')'
             >> (return$DottedList head tail)))

-- Desugars '(foo) notation
parseQuoted :: Parser LispVal
parseQuoted = char '\''
              >> parseExpr
              >>= \expr -> return$List [Atom "quote", expr]

parseExpr :: Parser LispVal
parseExpr = parseNumber
            <|> parseAtom
            <|> parseString
            <|> parseList
            <|> parseQuoted


-- Testing parse function and main function
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do 
    args <- getArgs
    putStrLn (readExpr (args !! 0))


\end{code}
\end{document}
