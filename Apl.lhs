\documentclass[12pt]{article}

\usepackage{verbatim}
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{latexsym}
\usepackage{fontspec}

%\setmainfont[Ligatures=TeX]{DejaVu}
\setmainfont[Ligatures=TeX]{Linux Libertine O}
\newfontfamily\mathfont{Asana Math}
\newfontfamily\fallbackfont{DejaVu Sans Mono}
\usepackage{newunicodechar}

\newunicodechar{⋈}{\fallbackfont ⋈}
\newunicodechar{⨝}{\fallbackfont ⨝}
\newunicodechar{∣}{\fallbackfont ∣}
\newunicodechar{☠}{\fallbackfont ☠}

\newunicodechar{⌈}{\mathfont ⌈}
\newunicodechar{⌊}{\mathfont ⌊}
\newunicodechar{<}{\mathfont <}
\newunicodechar{≤}{\mathfont ≤}
\newunicodechar{=}{\mathfont =}
\newunicodechar{≥}{\mathfont ≥}
\newunicodechar{>}{\mathfont >}
\newunicodechar{≠}{\mathfont ≠}

\newfontfamily\mathfont{Asana Math}
\newfontfamily\aplfont{DejaVu Sans Mono}
%\newfontfamily\aplfont{APL385}

% \include{format-apl}

%format α       = "\alpha"
%format ω       = "\omega"
%format f1      = "f_{1}"
%format f2      = "f_{2}"
% %format /:      = "\text{ /}_2\text{ }"
%format ⍳.      = "\text{ ⍳ }"
%format ⍳:      = "\text{ ⍳}_2\ "
%format ⋆.      = "\text{ ⋆ }"
%format ⋆:      = "\text{ ⋆}_2\ "
%format ⍴       = "\text{ ⍴ }"
%format ⍴.      = "\text{ ⍴ }"
%format ⍴:      = "\text{ ⍴}_2\text{ }"
%format ⌽.      = "\text{ ⌽ }"
%format ⌽:      = "\text{ ⌽}_2\text{ }"
%format ⌈.      = "\text{ ⌈ }"
%format ⌈:      = "\text{ ⌈}_2\text{ }"
%format ⌊.      = "\text{ ⌊ }"
%format ⌊:      = "\text{ ⌊}_2\text{ }"
%format ∈.      = "\text{ ∈ }"
%format ∈:      = "\text{ ∈}_2\text{ }"
%format ⍟.      = "\text{ ⍟ }"
%format ⍟:      = "\text{ ⍟}_2\text{ }"
%format ○.      = "\text{ ○ }"
%format ○:      = "\text{ ○}_2\text{ }"
%format ↑.      = "\text{ ↑ }"
%format ↑:      = "\text{ ↑}_2\text{ }"
%format ↓.      = "\text{ ↓ }"
%format ↓:      = "\text{ ↓}_2\text{ }"
%format ^.      = "\text{ , }"
%format ^:      = "\text{ ,}_2\text{ }"
%format ∣.      = "\text{ | }"
%format ∣:      = "\text{ |}_2\text{ }"


%auto-gen

%format ⌶.      = "\text{ ⌶ }"
%format ⌶:      = "\text{ ⌶}_2\text{ }"
%format ⌷.      = "\text{ ⌷ }"
%format ⌷:      = "\text{ ⌷}_2\text{ }"
%format ⌸.      = "\text{ ⌸ }"
%format ⌸:      = "\text{ ⌸}_2\text{ }"
%format ⌹.      = "\text{ ⌹ }"
%format ⌹:      = "\text{ ⌹}_2\text{ }"
%format ⌺.      = "\text{ ⌺ }"
%format ⌺:      = "\text{ ⌺}_2\text{ }"
%format ⌻.      = "\text{ ⌻ }"
%format ⌻:      = "\text{ ⌻}_2\text{ }"
%format ⌼.      = "\text{ ⌼ }"
%format ⌼:      = "\text{ ⌼}_2\text{ }"
%format ⌽.      = "\text{ ⌽ }"
%format ⌽:      = "\text{ ⌽}_2\text{ }"
%format ⌾.      = "\text{ ⌾ }"
%format ⌾:      = "\text{ ⌾}_2\text{ }"
%format ⌿.      = "\text{ ⌿ }"
%format ⌿:      = "\text{ ⌿}_2\text{ }"
%format ⍀.      = "\text{ ⍀ }"
%format ⍀:      = "\text{ ⍀}_2\text{ }"
%format ⍁.      = "\text{ ⍁ }"
%format ⍁:      = "\text{ ⍁}_2\text{ }"
%format ⍂.      = "\text{ ⍂ }"
%format ⍂:      = "\text{ ⍂}_2\text{ }"
%format ⍃.      = "\text{ ⍃ }"
%format ⍃:      = "\text{ ⍃}_2\text{ }"
%format ⍄.      = "\text{ ⍄ }"
%format ⍄:      = "\text{ ⍄}_2\text{ }"
%format ⍅.      = "\text{ ⍅ }"
%format ⍅:      = "\text{ ⍅}_2\text{ }"
%format ⍆.      = "\text{ ⍆ }"
%format ⍆:      = "\text{ ⍆}_2\text{ }"
%format ⍇.      = "\text{ ⍇ }"
%format ⍇:      = "\text{ ⍇}_2\text{ }"
%format ⍈.      = "\text{ ⍈ }"
%format ⍈:      = "\text{ ⍈}_2\text{ }"
%format ⍉.      = "\text{ ⍉ }"
%format ⍉:      = "\text{ ⍉}_2\text{ }"
%format ⍊.      = "\text{ ⍊ }"
%format ⍊:      = "\text{ ⍊}_2\text{ }"
%format ⍋.      = "\text{ ⍋ }"
%format ⍋:      = "\text{ ⍋}_2\text{ }"
%format ⍌.      = "\text{ ⍌ }"
%format ⍌:      = "\text{ ⍌}_2\text{ }"
%format ⍍.      = "\text{ ⍍ }"
%format ⍍:      = "\text{ ⍍}_2\text{ }"
%format ⍎.      = "\text{ ⍎ }"
%format ⍎:      = "\text{ ⍎}_2\text{ }"
%format ⍏.      = "\text{ ⍏ }"
%format ⍏:      = "\text{ ⍏}_2\text{ }"
%format ⍐.      = "\text{ ⍐ }"
%format ⍐:      = "\text{ ⍐}_2\text{ }"
%format ⍑.      = "\text{ ⍑ }"
%format ⍑:      = "\text{ ⍑}_2\text{ }"
%format ⍒.      = "\text{ ⍒ }"
%format ⍒:      = "\text{ ⍒}_2\text{ }"
%format ⍓.      = "\text{ ⍓ }"
%format ⍓:      = "\text{ ⍓}_2\text{ }"
%format ⍔.      = "\text{ ⍔ }"
%format ⍔:      = "\text{ ⍔}_2\text{ }"
%format ⍕.      = "\text{ ⍕ }"
%format ⍕:      = "\text{ ⍕}_2\text{ }"
%format ⍖.      = "\text{ ⍖ }"
%format ⍖:      = "\text{ ⍖}_2\text{ }"
%format ⍗.      = "\text{ ⍗ }"
%format ⍗:      = "\text{ ⍗}_2\text{ }"
%format ⍘.      = "\text{ ⍘ }"
%format ⍘:      = "\text{ ⍘}_2\text{ }"
%format ⍙.      = "\text{ ⍙ }"
%format ⍙:      = "\text{ ⍙}_2\text{ }"
%format ⍚.      = "\text{ ⍚ }"
%format ⍚:      = "\text{ ⍚}_2\text{ }"
%format ⍛.      = "\text{ ⍛ }"
%format ⍛:      = "\text{ ⍛}_2\text{ }"
%format ⍜.      = "\text{ ⍜ }"
%format ⍜:      = "\text{ ⍜}_2\text{ }"
%format ⍝.      = "\text{ ⍝ }"
%format ⍝:      = "\text{ ⍝}_2\text{ }"
%format ⍞.      = "\text{ ⍞ }"
%format ⍞:      = "\text{ ⍞}_2\text{ }"
%format ⍟.      = "\text{ ⍟ }"
%format ⍟:      = "\text{ ⍟}_2\text{ }"
%format ⍠.      = "\text{ ⍠ }"
%format ⍠:      = "\text{ ⍠}_2\text{ }"
%format ⍡.      = "\text{ ⍡ }"
%format ⍡:      = "\text{ ⍡}_2\text{ }"
%format ⍢.      = "\text{ ⍢ }"
%format ⍢:      = "\text{ ⍢}_2\text{ }"
%format ⍣.      = "\text{ ⍣ }"
%format ⍣:      = "\text{ ⍣}_2\text{ }"
%format ⍤.      = "\text{ ⍤ }"
%format ⍤:      = "\text{ ⍤}_2\text{ }"
%format ⍥.      = "\text{ ⍥ }"
%format ⍥:      = "\text{ ⍥}_2\text{ }"
%format ⍦.      = "\text{ ⍦ }"
%format ⍦:      = "\text{ ⍦}_2\text{ }"
%format ⍧.      = "\text{ ⍧ }"
%format ⍧:      = "\text{ ⍧}_2\text{ }"
%format ⍨.      = "\text{ ⍨ }"
%format ⍨:      = "\text{ ⍨}_2\text{ }"
%format ⍩.      = "\text{ ⍩ }"
%format ⍩:      = "\text{ ⍩}_2\text{ }"
%format ⍪.      = "\text{ ⍪ }"
%format ⍪:      = "\text{ ⍪}_2\text{ }"
%format ⍫.      = "\text{ ⍫ }"
%format ⍫:      = "\text{ ⍫}_2\text{ }"
%format ⍬.      = "\text{ ⍬ }"
%format ⍬:      = "\text{ ⍬}_2\text{ }"
%format ⍭.      = "\text{ ⍭ }"
%format ⍭:      = "\text{ ⍭}_2\text{ }"
%format ⍮.      = "\text{ ⍮ }"
%format ⍮:      = "\text{ ⍮}_2\text{ }"
%format ⍯.      = "\text{ ⍯ }"
%format ⍯:      = "\text{ ⍯}_2\text{ }"
%format ⍰.      = "\text{ ⍰ }"
%format ⍰:      = "\text{ ⍰}_2\text{ }"
%format ⍱.      = "\text{ ⍱ }"
%format ⍱:      = "\text{ ⍱}_2\text{ }"
%format ⍲.      = "\text{ ⍲ }"
%format ⍲:      = "\text{ ⍲}_2\text{ }"
%format ⍳.      = "\text{ ⍳ }"
%format ⍳:      = "\text{ ⍳}_2\text{ }"
%format ⍴.      = "\text{ ⍴ }"
%format ⍴:      = "\text{ ⍴}_2\text{ }"
%format ⍵.      = "\text{ ⍵ }"
%format ⍵:      = "\text{ ⍵}_2\text{ }"
%format ⍶.      = "\text{ ⍶ }"
%format ⍶:      = "\text{ ⍶}_2\text{ }"
%format ⍷.      = "\text{ ⍷ }"
%format ⍷:      = "\text{ ⍷}_2\text{ }"
%format ⍸.      = "\text{ ⍸ }"
%format ⍸:      = "\text{ ⍸}_2\text{ }"
%format ⍹.      = "\text{ ⍹ }"
%format ⍹:      = "\text{ ⍹}_2\text{ }"
%format ⍺.      = "\text{ ⍺ }"
%format ⍺:      = "\text{ ⍺}_2\text{ }"


%include lhs2TeX.sty
%include polycode.fmt

\input{unicode-char-apl}


%format `union` = "\cup"
%format alpha = "\alpha"
%format gamma = "\gamma"
%format delta = "\delta"
%format capGamma = "\Gamma"
%format tau = "\tau"
%format tau1 = "\tau_{1}"
%format tau2 = "\tau_{2}"
%format tau11 = "\tau_{11}"
%format tau12 = "\tau_{12}"
%format t12 = "t_{12}"
%format t1 = "t_{1}"
%format t1' = "t_{1}^{\prime}"
%format t2 = "t_{2}"
%format t2' = "t_{2}^{\prime}"
%format t3 = "t_{3}"
%format nv1 = "nv_{1}"


\title{APL Interpreter\\☠  ☠}
\author{\textsc{M. Barney}}
\date{\today}


\begin{document}

\maketitle


\section{Introduction}

This is a Haskell implementation of an APL interpreter.  The paper, and code, is divided up into several sections, where I discuss issues in implementation, design decisions, and give function specifications.

\section{Inputting APL}

APL is famous for its non-standard character set, of which a few are:

\begin{center}
⌶ ⌷ ⌸ ⌹ ⌺ ⌻ ⌼ ⌽ ⌾\\ 
⌿ ⍀ ⍁ ⍂ ⍃ ⍄ ⍅ ⍆ ⍇\\
⍈ ⍉ ⍊ ⍋ ⍌ ⍍ ⍎ ⍏ ⍐\\
⍑ ⍒ ⍓ ⍔ ⍕ ⍖ ⍗ ⍘ ⍙\\
⍚ ⍛ ⍜ ⍝ ⍞ ⍟ ⍠ ⍡ ⍢\\
⍣ ⍤ ⍥ ⍦ ⍧ ⍨ ⍩ ⍪ ⍫\\
⍬ ⍭ ⍮ ⍯ ⍰ ⍱ ⍲ ⍳ ⍴\\
⍵ ⍶ ⍷ ⍸ ⍹ ⍺ ← → ⋆
\end{center}

Before beginning to write an APL parser, interpreter, etc., I had to first decide on how to even \emph{input} APL.  My eventual decision was to write an X11 xkb symbol file.  For the uninitiated this is a text file for GNU/Linux Xorg distributions which allow the user to specify different keyboard input methods.

If you select a different keyboard setting via a GUI in gnome or KDE, it is modifying or referencing this file.

This was probably the most tedious part of my project --- it took a substantial amount of time to look up the symbols unicode hexadecimal value; to make sure it was the correct symbol (Unicode is huge, and sometimes symbols look almost the same); and to manually type it into the keyboard configuration.

A typical line in an X11 keyboard configuration looks as follows:

\begin{verbatim}
key <AD10> { [	   p,          P,     U22C6,       U235F ] };
\end{verbatim}

In this case, the keyboard key for `p' (and uppercase `P' with a standard shift) is enhanced with two more possibilities via an {\tt ISO\_level3\_Shift}.  This key is sometimes the right alt key (also known as altgr) on many keyboards, but can be remapped to anything.

Mine is the menu key because I don't even know what that key is for anyway.

Assuming my keyboard configuration has been loaded\footnotemark, in our example above, holding down the {\tt ISO\_level3\_Shift} and pressing the `p' key will result in the symbol `⋆' (the APL function for exponentiation).  Similarly, holding a regular shift and {\tt ISO\_level3\_Shift} and finally the `p' key will result in `⍟' (the APL function for logarithm).\footnotetext{In GNU/Linux, from the commandline, this can usually be accomplished by the command: {\tt setxkbmap -layout us -variant apl}}

Wasn't that fun?

%include Interface.lhs

%include Functions.lhs

%include Parser.lhs

\section{Main}

This is the main module which starts a simple REPL loop that scans, parses and evaluates basic APL expressions, while catching exceptions raised during evaluation.


\begin{comment}
\begin{code}
{- пиздец всему фашизму -}

{-# LANGUAGE 
    OverloadedStrings,
    FlexibleInstances,
    ForeignFunctionInterface #-}

module Main where

import Control.Monad
import Foreign.C
import System.Random
import System.IO
import Control.Exception
import Data.Char (isSpace)

import Interface
import Functions
import Parser

\end{code}
\end{comment}

\begin{code}

simpleIgnoreLoop = do
  putStr "<λ> "
  hFlush stdout
  expr <- getLine
  if (expr == [] || (and $ map isSpace expr)) then
     simpleIgnoreLoop
  else 
     do
       let tokens = scan expr
       result <- try (evaluate (eval tokens))::
        IO (Either SomeException (APL Float))
       case result of
            Left errorMesg -> print errorMesg
            Right answer -> print answer
       simpleIgnoreLoop

main = do
  putStrLn "Astoundingly Profound Language Interpreter, version 0.1"
  simpleIgnoreLoop


\end{code}

\end{document}
