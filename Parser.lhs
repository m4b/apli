\section{Parser and Evaluator for APL}

The working parser for the interpreter is composed, essentially, of two parts: a symbol map that maps string APL functions to a tuple, containing the unary version and the dyadic version, and the scanner which tokenizes and marks the arity of functions.

\begin{comment}
\begin{code}

{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.ParserCombinators.Parsec

import Interface
import Functions

putLn = T.putStrLn . T.pack

\end{code}
\end{comment}

During the parsing and evaluation stage, the arity of the operator is queried, and the appropriate side of the tuple is returned depending on that arity.

In this way, an evaluation stack is built up, and reduced, when moving across the token list.


\begin{code}

symbolMap sym = 
          case sym of 
          "+" -> ((+.), (+:))
          "−" -> ((−.), (−:))
          "-" -> ((−.), (−:))
          "×" -> ((×.), (×:))
          "÷" -> ((÷.), (÷:))
          "⋆" -> ((⋆.), (⋆:))
          "⌈" -> ((⌈.), (⌈:))
          "⌊" -> ((⌊.), (⌊:))
          "○" -> ((○.), (○:))
          "⍟" -> ((⍟.), (⍟:))
          "⍳" -> ((⍳.), (⍳:))
          "⍴" -> ((⍴.), (⍴:))
          "⌽" -> ((⌽.), (⌽:))
          "∈" -> ((∈.), (∈:))
          "," -> ((^.), (^:))
          "↑" -> ((↑.), (↑:))
          "↓" -> ((↓.), (↓:))
          "∣" -> ((∣.), (∣:))
          "|" -> ((∣.), (∣:))
          "<" -> ((<.), (<:))
          "≤" -> ((≤.), (≤:))
          "=" -> ((=.), (=:))
          "≥" -> ((≥.), (≥:))
          ">" -> ((>.), (>:))
          "≠" -> ((≠.), (≠:))
          s -> error ("Unknown symbol: " ++ s)

\end{code}

The token data structure has the obvious datums: a function which embeds the operator, and its arity; names for identifiers; numbers; left paren and right parens; and the minus sign, which has special precedence.

\begin{code}

data TokenAPL a = 
     Function String Int |
     Name String |
     Number a |
     LParen |
     RParen |
     Minus deriving (Show, Eq, Read)

\end{code}

The scanner uses a few functions to tokenize the string; it checks for whether the current object it is examining is a number, an identifier, or a symbol (neither of those); moreover, it supports vector input as one would expect, i.e.:

\begin{center}
⍴ 1 2 3 4
\end{center}

The current scanner relies upon the Prelude's |words| function; a side effect is that the scanner is space sensitive for operators, which can simply be amended by writing a custom |words|.

\begin{code}

isFloat s = loop s 0 where
        loop [] _                           = True
        loop (s:ss) counter | isDigit s     = loop ss counter
        loop ('.':ss) counter| counter == 0 = loop ss 1
        loop (s:ss) _                       = False


isNum s = (and (map isDigit s)) || isFloat s
isIdent s = and (map isAlpha s)
isSym s = (not . isNum $ s) && (not . isIdent $ s)

-- eats the largest number or vector it can
obtainNumber ns = loop ns where
             loop [] = []
             loop (n:ns) | isNum n = (read n :: Float):loop ns
             loop (n:ns) | and $ map isSpace n = loop ns
             loop (n:ns) = []

scan' []               = []
scan' ("(":ss) = (LParen: scan' ss)
scan' (")":ss) = (RParen: scan' ss)
scan' (s:ss) | isIdent s = ((Name s): scan' ss)
scan' (s1:ss) | (isSym s1) =  (Function s1 1):scan' ss
scan' t@(s:ss) | isNum s =
      let num = obtainNumber t 
          ss' = (drop (length num) t)
            in
          if ss' == [] then -- parens break arity right now
               (Number num): scan' ss'
          else
             let leftarg = Number num
                 next    = head ss'
                 op      = (Function next 2) in
                 leftarg:op:(scan' $ tail ss')
scan' _ = error "Unknown character in expression"

isPunctuation' c = (isPunctuation c) && (not (c=='.'))

isAPLbreak c = (isSpace c) || (isSymbol c) || (isPunctuation' c)

-- hack to parse no spaces between operators, parens, etc.
myWords s =
        case dropWhile isSpace s of
             "" -> []
             s'@(c:cs) -> w:myWords s'' where
                    (w, s'') =  
                     if ((isSymbol c) || (isPunctuation' c))  then
                        ([c],cs)
                     else 
                        break isAPLbreak s'

scan = scan' . myWords

\end{code}

The evaluator is \emph{very} simple in the current version.  In fact, it evaluates directly from the tokens, which is usually considered bad form.  However, the structure is so simple right now, this seemed harmless, and so it wasn't necessary to construct an abstract syntaxt tree for the APL expressions.

Order of evaluation for an APL operator has ``small'' reach to the left (i.e., one datum to the left), and ``long'' reach to the right --- as far as possible to the right.

As a result, one can evaluate as one goes left to right, computing the final result when one reaches the final token.

\begin{code}

eval ((Number i):[]) = (APL i)
eval ((Number i):(Function name arity):tokens) =
     if arity == 2 then
         ((snd . symbolMap $ name)) (APL i) (eval tokens)
     else  
         error "Arity error during parsing; expected arity 2, found 1."
eval ((Function name arity):tokens) =
     if arity == 1 then
        ((fst . symbolMap $ name)) (eval tokens)
     else
        error "Arity error during parsing; expected arity 1, found 2."
eval _ = error "General unmatched parsing error during evaluation."


\end{code}
