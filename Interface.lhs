\section{Data Interface}

\begin{comment}
\begin{code}

module Interface (APL (..), map2, concatAPL,unwrap) where

{-# LANGUAGE
    FlexibleInstances,
    GeneralizedNewtypeDeriving,
    #-}

import Control.Monad
import Data.Monoid

import Foreign.C 
import System.Random
import Data.List (elem)
import Data.Char (isDigit)

\end{code}
\end{comment}


In this module I introduce the APL data type; essentially a wrapper for a list datum.

\begin{code}

newtype APL a = APL [a] deriving (Read, Eq, Ord)

\end{code}

However, in order to better approximate the distinction that APL does \emph{not} make between scalars and vectors, I install the newtype into a variety of different typeclasses.

I elaborate in more detail my decision to go the install-in-all-the-typeclasses route in the next section.

\subsection{The Bureaucracy}

The first serious issue in implementing an APL interpreter in Haskell, and for any language, is what kind of data structure to use --- and in a roundabout way, how to deal with APL scalars, and how to deal with APL lists/arrays, since no real distinctions are made between them in that venerable language.

For example, consider the following expression:

\[
\text{α} + \text{ω}
\]


If α and ω are scalars, then the type of ``+'' will be a function from scalar → scalar → scalar.  But in APL, α and ω can also be vectors, and more importantly, the behavior of ``+'' is completely different depending on the type.   In particular:

\begin{enumerate}
\item if the two are vectors, ``+'' returns a vector where each element of the first is added pointwise to the second;
\item  if one is a scalar and the other a vector, then that scalar is added to each element of the vector;
\item if both are scalars we have normal addition;
\item and if both are vectors but have different lengths, we have an error.
\end{enumerate}

A potential solution might be to introduce an APL data type which permits either scalars or vectors, i.e.:
\[
\verb+data APL a = Scalar a | Vector [a]+
\]

But this doesn't prevent mixed types from occurring within a vector; i.e., we could have:

\[
\verb+Scalar (Vector [1,2,3])+
\]

which is unusual, and undesirable.

My solution was to install the APL newtype into most of the usual typeclasses; I thereby gained overloaded operators, and a blurred distinction between scalars and vectors (lists).

In effect then, an APL ``scalar'' would just be a list of one element, with the APL constructor acting as a wrapper.  The result in my opinion is quite elegant; indeed, APL\emph{ish} expressions are typeable (and computable) in ghci.  E.g.:

\begin{center}
*Main> (⌽) \$ (APL [1,2,3,4] \(\bind\) (*2)) ×: ((⍳) 4) / (fmap (○.) (4 ⍴: pi))

{\footnotesize 
3.242277876554809 1.82378130556208 0.8105694691387022 0.20264236728467555}

\end{center}

Furthermore, this makes the work of the parsing stage that much less --- essentially it will be a one to one mapping from APL expressions to the internal Haskell versions of those functions.

\begin{code}


undesirables c = (c /= '[') && (c /= ']') && (c /= ',')

putSpaces []     = []
putSpaces (']':',':xs) = '\n':putSpaces xs
putSpaces (']':xs) = putSpaces xs
putSpaces (',':xs) = ' ':putSpaces xs
putSpaces (x:xs)   = x:putSpaces xs
removeUndesirables = (filter undesirables) . putSpaces 

cheat [] = []
cheat ('.':'0':[]) = []
cheat ('.':'0':x:xs) | not . isDigit $ x = x:cheat xs
cheat ('-':xs) = '¯':cheat xs
cheat (x:xs) = x:cheat xs

aplShow (APL l) = cheat . removeUndesirables . show $  l

instance Show a => Show (APL a) where
         show p =  aplShow p

instance Functor APL where
         fmap f1 (APL p) = APL $ map f1 p

unwrap (APL x) = x

-- container aware
aplZip f2 (APL p) (APL q) = APL (loop f2 p q [])
       where 
             loop f2 [] (q:qs) acc      = error "Error: non equal length lists"
             loop f2 (p:ps) [] acc      = error "Error: non equal length lists"
             loop f2 [] [] acc          = reverse acc
             loop f2 (p:ps) (q:qs) acc  = loop f2 ps qs ((p `f2` q):acc)

-- container aware
map2 :: (t -> t -> b) -> APL t -> APL t -> APL b
map2 f2 (APL [p]) (APL [q])    = APL [f2 p q]
map2 f2 (APL [p]) q            = fmap (f2 p) q
map2 f2 p q                    = aplZip f2 p q

instance Num a => Num (APL a) where
         p + q         = map2 (+) p q
         p * q         = map2 (*) p q
         p - q         = map2 (-) p q
         abs p         = fmap abs p
         signum p      = fmap signum p
         fromInteger p = APL [fromInteger p]

instance Real a => Real (APL a) where
         toRational (APL p) = toRational . head $ p

instance Enum a => Enum (APL a) where
         toEnum i   = APL [toEnum i]
         -- oh man this is just bad...
         fromEnum (APL (p:ps)) = fromEnum p
         succ p     = fmap succ p
           

instance (Integral a, Real a, Enum a) => Integral (APL a) where
         quot p q                = map2 (quot) p q
         rem p q                 = map2 (rem) p q
         div p q                 = map2 (div) p q
         mod p q                 = map2 (mod) p q
         quotRem p q             = ((quot p q), (rem p q)) 
         divMod p q              = ((div p q), (mod p q))
         -- truncating vectors for toInteger
         toInteger (APL [])      = error "toInteger: empty APL"
         toInteger (APL (p:ps))  = toInteger p
--         toInteger p             = fmap toInteger p

--instance (RealFrac a, Floating a) => RealFrac (APL a) where
--         truncate (p)          = fromIntegral p


instance Fractional a => Fractional (APL a) where
         p / q          = map2 (/) p q
         recip p        = fmap recip p
         fromRational p = APL [fromRational p]

instance Floating a => Floating (APL a) where
         pi             = APL [pi]
         exp p          = fmap exp p
         sqrt p         = fmap sqrt p
         log p          = fmap log p
         p ** q         = map2 (**) p q
         logBase p q    = map2 (logBase) p q
         sin p          = fmap sin p
         tan p          = fmap tan p
         cos p          = fmap cos p
         asin p         = fmap asin p
         atan p         = fmap atan p
         acos p         = fmap acos p
         sinh p         = fmap sinh p
         tanh p         = fmap tanh p
         cosh p         = fmap cosh p
         asinh p        = fmap asinh p
         atanh p        = fmap atanh p
         acosh p        = fmap acosh p
 

concatAPL (APL p) = APL $ loop p
          where
                loop []  = []
                loop ((APL (x:xs)):ps) =
                     x:xs ++ (loop ps)

instance Monad APL where
         (APL p) >>= f      = concatAPL $ fmap f (APL p)

         (APL p) >> (APL q) = APL (p >> q)
         return p           = APL [p]

instance Monoid (APL a) where
         mempty                  = APL []
         mappend (APL a) (APL b) = APL (a ++ b)

class Boolean a where
      land   :: a -> a -> a
      lor    :: a -> a -> a
      lif    :: a -> a -> a
      lnot   :: a -> a
      a `land` b = lnot ((lnot a) `lor` (lnot b))
      a `lor` b  = lnot ((lnot a) `land` (lnot b))
      a `lif` b  = (lnot a) `lor` b


--instance Foldable APL where
--         foldMap g = mconcat . map g
--         foldr f2 seed (APL [])          = seed
--         foldr f2 seed (APL x:xs)        = f2 seed (foldr f2 seed (APL xs))

foo1 :: Num a => APL a
foo1 = APL [1,2,3,4]
foo2 :: Num a => APL a
foo2 = APL [5,6,7,8]
foo3 :: Fractional a => APL a
foo3 = APL [1.0,2.0,3.0,4.0]
foo4 :: Fractional a => APL a
foo4 = APL [5.0,6.0,7.0,8.0]

foo5 = APL [(APL [1,2,3,4]), (APL [5,6,7,8])]

\end{code}
