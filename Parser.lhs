\documentclass{article}
\usepackage{listings}
\begin{document}
\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small}}{}

\begin{code}
module Main where 
import System.Environment
\end{code}

Now, let's try writing a very simple parser.

\begin{code}
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
\end{code}

This makes the Parsec library functions available to us, except the spaces function, whose name conflicts with a function that we'll be defining later.
Now, we'll define a parser that recognizes one of the symbols allowed in Scheme identifiers:

\begin{code}
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
\end{code}

This is another example of a monad: in this case, the "extra information" that is being hidden is all the info about position in the input stream, backtracking record, first and follow sets, etc. Parsec takes care of all of that for us. We need only use the Parsec library function oneOf, and it'll recognize a single one of any of the characters in the string passed to it. Parsec provides a number of pre-built parsers: for example, letter and digit are library functions. And as you're about to see, you can compose primitive parsers into more sophisticated productions.
Let's define a function to call our parser and handle any possible errors:

\begin{code}
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
\end{code}

As you can see from the type signature, readExpr is a function (->) from a String to a String. We name the parameter input, and pass it, along with the symbol parser we defined above to the Parsec function parse. The second parameter to parse is a name for the input. It is used for error messages.

parse can return either the parsed value or an error, so we need to handle the error case. Following typical Haskell convention, Parsec returns an Either data type, using the Left constructor to indicate an error and the Right one for a normal value.

We use a case...of construction to match the result of parse against these alternatives. If we get a Left value (error), then we bind the error itself to err and return "No match" with the string representation of the error. If we get a Right value, we bind it to val, ignore it, and return the string "Found value".

The case...of construction is an example of pattern matching, which we will see in much greater detail later on.

Finally, we need to change our main function to call readExpr and print out the result (need to add import System.Environment in the beginning of the file now):

\begin{code} 
main :: IO ()
main = do 
    args <- getArgs
    putStrLn (readExpr (args !! 0))
\end{code}

To compile and run this, you need to specify --make on the command line, or else there will be link errors. For example:

\begin{verbatim}
$ ghc --make -o simple_parser listing3.1.hs
$ ./simple_parser $
Found value
$ ./simple_parser a
No match: "lisp" (line 1, column 1):
unexpected "a"
\end{verbatim}

Next, we'll add a series of improvements to our parser that'll let it recognize
progressively more complicated expressions. The current parser chokes if there's
whitespace preceding our symbol:

\begin{verbatim}
$ ./simple_parser "   %"
No match: "lisp" (line 1, column 1):
unexpected " "
\end{verbatim}

Let's fix that, so that we ignore whitespace.
First, let's define a parser that recognizes any number of whitespace characters.
Incidentally, this is why we included the hiding (spaces) clause when we
imported Parsec: there's already a spaces function in that library, but it
doesn't quite do what we want it to. (For that matter, there's also a parser
called lexeme that does exactly what we want, but we'll ignore that for
pedagogical purposes.)

\begin{code}
spaces :: Parser ()
spaces = skipMany1 space
\end{code}

\end{document}
