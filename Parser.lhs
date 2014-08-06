\documentclass{article}
\usepackage{listings}
\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\begin{document}
\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{deadcode}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{shell}{\lstset{language=bash,basicstyle=\small}}{}

\begin{code}
module Main where 
import System.Environment
\end{code}

Now, let's try writing a very simple parser.

\begin{code}
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
\end{code}

This makes the Parsec library functions available to us, except the spaces function, whose name conflicts with a function that we'll be defining later.
Now, we'll define a parser that recognizes one of the symbols allowed in Scheme identifiers:

\begin{code}
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
\end{code}

This is another example of a monad: in this case, the "extra information" that is being hidden is all the info about position in the input stream, backtracking record, first and follow sets, etc. Parsec takes care of all of that for us. We need only use the Parsec library function oneOf, and it'll recognize a single one of any of the characters in the string passed to it. Parsec provides a number of pre-built parsers: for example, letter and digit are library functions. And as you're about to see, you can compose primitive parsers into more sophisticated productions.
Let's define a function to call our parser and handle any possible errors:

\begin{deadcode}
readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
\end{deadcode}

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

\begin{deadcode}
$ ghc --make -o simple_parser listing3.1.hs
$ ./simple_parser $
Found value
$ ./simple_parser a
No match: "lisp" (line 1, column 1):
unexpected "a"
\end{deadcode}

Next, we'll add a series of improvements to our parser that'll let it recognize
progressively more complicated expressions. The current parser chokes if there's
whitespace preceding our symbol:

\begin{deadcode}
$ ./simple_parser "   %"
No match: "lisp" (line 1, column 1):
unexpected " "
\end{deadcode}

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

Just as functions can be passed to functions, so can actions. Here we pass the
Parser action space to the Parser action skipMany1, to get a Parser that will
recognize one or more spaces.
Now, let's edit our parse function so that it uses this new parser. Changes are
no longer in red:

\begin{deadcode}
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
\end{deadcode}

Right now, the parser doesn't do much of anything—it just tells us whether a given string can be recognized or not. Generally, we want something more out of our parsers: we want them to convert the input into a data structure that we can traverse easily. In this section, we learn how to define a data type, and how to modify our parser so that it returns this data type.
First, we need to define a data type that can hold any Lisp value:

\begin{code}
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
\end{code} 

\begin{deadcode}
parseString :: Parser LispVal
parseString = char '"' >> many(noneOf "\"") >>= \x -> char '"' >> (return $ String x)
\end{deadcode}

Now let's test and see what comes out when we use the parser

\begin{deadcode} 
main :: IO ()
main = do 
    args <- getArgs
    putStrLn (readExpr (args !! 0))
    putStrLn (case parse parseString "" "\"asdf\"" of
                  Left err -> "Failed to parse string: " ++ show err
                  Right val -> case val of String s -> "parsed string " ++ s)
\end{deadcode}

\begin{deadcode}
(<screen>pond) 2 scheme-in-48-hours 0 $ tup upd && ./Parser ' %'
[ tup ] [0.000s] Scanning filesystem...
[ tup ] [0.001s] Reading in new environment variables...
[ tup ] [0.001s] No Tupfiles to parse.
[ tup ] [0.001s] No files to delete.
[ tup ] [0.001s] No commands to execute.
[ tup ] [0.001s] Updated.
Found value
parsed string asdf
(<screen>pond) 2 scheme-in-48-hours 0 $ 
\end{deadcode}


Now let's move on to Scheme variables. An atom is a letter or symbol, followed by any number of letters, digits, or symbols

\begin{deadcode} 
parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom
\end{deadcode}

but that's boring so

\begin{code}
parseAtom = (letter <|> symbol) >>= \first ->
            many(letter <|> digit <|> symbol) >>= \rest ->
            let atom = first:rest in 
            return $ case atom of "#t" -> Bool True
                                  "#f" -> Bool False
                                  _    -> Atom atom
\end{code}

Here, we introduce another Parsec combinator, the choice operator <|>. This
tries the first parser, then if it fails, tries the second. If either succeeds,
then it returns the value returned by that parser. The first parser must fail
before it consumes any input: we'll see later how to implement backtracking.
Once we've read the first character and the rest of the atom, we need to put
them together. The "let" statement defines a new variable atom. We use the list
cons operator : for this. Instead of :, we could have used the concatenation
operator ++ like this [first] ++ rest; recall that first is just a single
character, so we convert it into a singleton list by putting brackets around it.
Then we use a case expression to determine which LispVal to create and return,
matching against the literal strings for true and false. The underscore
alternative is a readability trick: case blocks continue until an underscore  case (or fail
any case which also causes the failure of the whole case expression), think of
underscore
as a wildcard. So if the code falls through to the underscore case, it always matches,
and returns the value of atom.

Finally, we create one more parser, for numbers. This shows one more way of
dealing with monadic values:

\begin{deadcode}
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
\end{deadcode}

It's easiest to read this backwards, since both function application (\verb+$+) and
function composition (\verb+.+) associate to the right. The parsec combinator many1
matches one or more of its argument, so here we're matching one or more digits.
We'd like to construct a number LispVal from the resulting string, but we have a
few type mismatches. First, we use the built-in function read to convert that
string into a number. Then we pass the result to Number to get a LispVal. The
function composition operator . creates a function that applies its right
argument and then passes the result to the left argument, so we use that to
combine the two function applications.

Unfortunately, the result of many1 digit is actually a Parser String, so our
combined
\begin{deadcode}
Number . read
\end{deadcode}
still can't operate on it. We need a way to tell it to
just operate on the value inside the monad, giving us back a Parser LispVal. The
standard function liftM does exactly that, so we apply liftM to our 
\begin{deadcode}
Number . read
\end{deadcode}
function, and then apply the result of that to our parser.

This style of programming—relying heavily on function composition, function
application, and passing functions to functions—is very common in Haskell code.
It often lets you express very complicated algorithms in a single line, breaking
down intermediate steps into other functions that can be combined in various
ways. Unfortunately, it means that you often have to read Haskell code from
right-to-left and keep careful track of the types. We'll be seeing many more
examples throughout the rest of the tutorial, so hopefully you'll get pretty
comfortable with it.

Let's create a parser that accepts either a string, a number, or an atom:

\begin{deadcode}
parseExpr :: Parser LispVal
parseExpr = parseNumber <|> parseAtom <|> parseString
\end{deadcode}

And update readExpr to match:

\begin{code}
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
\end{code}


Exercises
\begin{itemize}
    \item Rewrite parseNumber, without \verb+liftM+, using
    \begin{itemize}
        \item do-notation

\begin{deadcode}
parseNumber = do digitStr <- many1 digit
                 return$Number$read digitStr
\end{deadcode}

        \item explicit sequencing with the \verb+>>=+ operator

\begin{code}
parseNumber = many1 digit >>= \digitStr -> return$Number$read digitStr
\end{code}

    \end{itemize}

    \item Our strings aren't quite R5RS compliant, because they don't support
    escaping of internal quotes within the string. Change parseString so that \"
    gives a literal quote character instead of terminating the string. You may
    want to replace noneOf \verb+"\""+ with a new parser action that accepts either a
    non-quote character or a backslash followed by a quote mark.

\begin{deadcode}
parseString :: Parser LispVal
parseString = char '"'
              >> many(noneOf "\\\"" <|> (char '\\' >> char '"'))
              >>= \x -> char '"' >> (return $ String x)
\end{deadcode}

    \item  Modify the previous exercise to support \verb+\n+, \verb+\r+,
    \verb+\t+, \verb+\\+, and any other
    desired escape characters

\begin{code}
parseString :: Parser LispVal
parseString = char '"'
              >> many(noneOf "\\\""
                      <|> (char '\\' >> (char '"'
                                         <|> char '\\'
                                         <|> (char 'n' >> return '\n')
                                         <|> (char 'r' >> return '\r')
                                         <|> (char 't' >> return '\t'))))
              >>= \x -> char '"' >> (return $ String x)
\end{code}

    \item Change parseNumber to support the Scheme standard for different bases.
    You may find the readOct and readHex functions useful.

    \item  Add a Character constructor to LispVal, and create a parser for
    character literals as described in R5RS.

    \item Add a Float constructor to LispVal, and support R5RS syntax for
    decimals. The Haskell function readFloat may be useful.

    \item  Add data types and parsers to support the full numeric tower of
    Scheme numeric types. Haskell has built-in types to represent many of these;
    check the Prelude.  For the others, you can define compound types that
    represent eg. a Rational as a numerator and denominator, or a Complex as a
    real and imaginary part (each itself a Real).
\end{itemize}

Recursive Parsers: Adding lists, dotted lists, and quoted datums[edit]
Next, we add a few more parser actions to our interpreter. Start with the
parenthesized lists that make Lisp famous:

\begin{code}

--trySepBy :: Parser -- like sepBy but backtrack if we consume separator
                   -- and then get stuck
trySepBy :: Parser a -> Parser b -> Parser [a]
trySepBy item sep = item >>= \first -> (try$sep
                                            >> trySepBy item sep
                                            >>= \rest -> return [first])
                                        <|> (return [first])

parseList :: Parser LispVal
parseList = 
    char '('
    >> skipMany space
    >> trySepBy parseExpr spaces  -- ordinary sepBy gets stuck on space in (1 . 2)
    >>= \head -> skipMany space -- Soak any irrelevant space
    >> ((char ')' >> (return$List head)) -- End here or continue into dotted list
        <|> (char '.'
             >> skipMany space
             >> parseExpr 
             >>= \tail -> skipMany space
             >> char ')'
             >> (return$DottedList head tail)))

parseExpr :: Parser LispVal
parseExpr = parseNumber <|> parseAtom <|> parseString <|> parseList
\end{code}





\end{document}
