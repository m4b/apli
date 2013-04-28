\section{Functions}

It is probably fair to say that the interest (or notoriety) of APL lies in its functions.  There are approximately 100 symbols in APL, each which can typically be interpreted either \emph{monadically} (taking one argument), or \emph{dyadically} (taking two arguments).

Therefore, we have approximately 200 functions to implement for what might be considered the ``prelude'' of APL.

I have separated the functions somewhat according to their behavior, and their ``types'' --- in other words, whether they expect scalars or vectors, operate on vectors, and so on and so forth.

I have also partitioned the functions into their monadic and dyadic counterparts, for easier reference.

\begin{comment}
\begin{code}


{-# LANGUAGE FlexibleContexts #-}

module Functions where

import Foreign.C
import System.Random

import Interface
import Data.Monoid

foreign import ccall "random" c_random :: CUInt -> CUInt
\end{code}
\end{comment}

\subsection{Unary Functions}

\begin{code}
independentFold f (x:[]) = x
independentFold f (x:xs) = inf_it f xs x
                where 
                      inf_it f (x:[]) acc = f x acc
                      inf_it f (x:xs) acc = inf_it f xs (f x acc)

-- + plus operator
(+.) :: Num a => APL a -> APL a
(+.) ω = ω

-- − minus operator
(−.) :: Num a => APL a -> APL a
(−.) ω = fmap (* (-1)) ω

-- × times operator
(×.) :: Num a => APL a -> APL a
(×.) ω = fmap signum ω

-- ÷ division operator
(÷.) :: Fractional b => APL b -> APL b
(÷.) ω = fmap recip ω

-- ⋆ power operator
(⋆.) :: Floating b => APL b -> APL b
(⋆.) ω = exp ω

-- ⌈ ceiling operator
(⌈.) :: (Num b, RealFrac a) => APL a -> APL b
(⌈.) ω = fmap (fromIntegral . ceiling) ω

-- ⌊ floor operator
(⌊.) :: (Num b, RealFrac a) => APL a -> APL b
(⌊.) ω = fmap (fromIntegral . floor) ω

-- ○ multiply by pi
(○.) :: Floating a => APL a -> APL a
(○.) ω = pi * ω

-- ⍟ natural logarithm
(⍟.) :: Floating a => APL a -> APL a
(⍟.) ω = log ω

\end{code}

Vector based functions are as follows.

\begin{code}

atIndex element (x:xs) count | x == element = count
atIndex element (x:xs) count = atIndex element xs (count + 1)

iota :: (Num a, Ord a) => a -> [a]
iota i = reverse . loop $ i where 
          loop i = 
           if i <= 0 then []
           else i:(loop (i-1))

        
(⍳) ω = APL . iota $ ω

(⍳.) :: (Num a, Ord a) => APL a -> APL a
(⍳.) (APL (ω:rest)) = APL $ loop ω 1 where
     loop ω counter = 
          if counter >= (ω + 1)then []
          else counter:(loop ω (counter +1))

-- ⍴ rho 
-- X ←→ X⍴X⍴Y
(⍴.) :: Num a => APL b -> APL a
(⍴.) (APL ω) = fromIntegral (length ω)

-- ``monadic reversal...''
(⌽.) :: APL a -> APL a
(⌽.) (APL ω) = APL . reverse $ ω

(?.) :: (Num b, Random b) => b -> IO b
(?.) ω = do
     r <- randomRIO (0, ω)
     return r

-- problematic -- will have type:
-- turns something into a vector
(∈.) :: APL a -> APL a
(∈.) (APL ω) = undefined

-- equally problematic -- will have type: 
-- \verb+ (^.) :: APL (APL a) -> APL a+
-- ... muurder
(^.) ω = undefined

-- head of vector
(↑.) ω = 1 ↑: ω

-- tail of vector
(↓.) ω = 1 ↓: ω


\end{code}

\subsection{Dyadic Functions}

Basic dyadic arithmetic and trigonometric functions.

\begin{code}

(+:) :: Num a => APL a -> APL a -> APL a
α +: ω = α + ω

(−:) :: Num a => APL a -> APL a -> APL a
α −: ω = α - ω

(×:) :: Num a => APL a -> APL a -> APL a
α ×: ω = α * ω

(÷:) :: Fractional b => APL b -> APL b -> APL b
α ÷: ω = α / ω

(⋆:) :: Floating b => APL b -> APL b -> APL b
α ⋆: ω = α ** ω

(⌈:) :: (Ord b) => APL b -> APL b -> APL b
α ⌈: ω = map2 max α ω

(⌊:) :: (Ord b) => APL b -> APL b -> APL b
α ⌊: ω = map2 min α ω

(○:) :: (Eq t, Floating a, Num t) => APL t -> APL a -> APL a
α ○: ω = 
  case α of
  APL [1] -> sin ω
  APL [2] -> cos ω
  APL [3] -> tan ω
  -- returning ω when no case match
  _ -> ω

-- caught non-commutativity bug in map2 after implementing ⍟
(⍟:) :: Floating a => APL a -> APL a -> APL a
α ⍟: ω = logBase α ω

\end{code}

Dyadic vector functions.

\begin{code}

-- container aware, will error if ⍴ ω > 1
(⍳:) :: (Eq b1, Num b, Ord b) => APL b1 -> APL b1 -> APL b
α@(APL ls) ⍳: (APL [ω]) = 
  if ω `elem` ls then
     atIndex ω ls 0
  else
     1 + ((⌈:) /* ((⍳.) $ (⍴.) α))

-- ``The symbol ⍴ used for the dyadic function of
-- \emph{replication}\ldots'' (pg. 350, notation as thought)
-- does not create array of shape \(\alpha\) with data \(\omega\)
-- single dimension, container aware
(⍴:) :: (Num a, Ord a) => APL a -> APL a1 -> APL a1
α ⍴: (APL (ω:_)) = loop α ω [] where
     loop α ω acc = 
       if α <= 0 then 
          (⌽.) . APL $ (reverse acc )
       else 
          loop (α - 1) ω (ω:acc)

rotate :: (Num a, Ord a) => APL a1 -> APL a -> APL a1
rotate xs n = if n >= 0 then
                  (n ↓: xs) `mappend` (n ↑: xs)
              else let l = (((⍴.) xs) + n) in
                   (l ↑: xs) `mappend` (l ↓: xs)

-- ``dyadic rotation''
-- 2 ⌽ ⍳ 5 ←→ 3 4 5 1 2
-- ¯2 ⌽ ⍳ 5 ←→ 4 5 1 2 3
(⌽:) :: (Num a, Ord a) => APL a -> APL a -> APL a
α ⌽: ω = rotate ω α

(?:) :: (Num a, Random a) => Int -> a -> IO [a]
α ?: ω = sequence . (replicate α) $ (?.) ω

roll :: (Num a, Random a) => a -> IO a
roll x = (?.) x

-- 1 for elements of α presnt in ω; 0 otherwise
(∈:) :: (Eq a, Num b) => APL a -> APL a -> APL b
α ∈: (APL ω) = fmap (\x -> if x `elem` ω then 1 else 0) α

-- , (comma) because , is reserved
(^:) :: APL a -> APL a -> APL a
α ^: ω = α `mappend` ω

-- float based indexing --- not awesome
(↑:) :: (Num a1, Ord a1) => APL a1 -> APL a -> APL a
α ↑: ω = APL $ loop α ω where
      loop n foo@(APL (p:ps)) = 
           if n <= 0 then []
           else p:(loop (n-1) (APL ps))

-- ...at all
(↓:) :: (Num a1, Ord a1) => APL a1 -> APL a -> APL a
α ↓: (APL (ω:ωs)) = APL $ loop α (ω:ωs) where
  loop α (ω:ωs) =
    if α <= 0 then (ω:ωs)
    else loop (α-1) (ωs)

--α /: ω 

\end{code}

\subsection{Special Functions: Operators}

The special functions are operators when their left operand is a function.  They are: ``/'' <add others>

\begin{code}

f /* (APL ω) = independentFold f ω 

\end{code}

\subsection{Example and Unit Tests}

\begin{code}

foo1 :: Num a => APL a
foo1 = APL [1,2,3,4]
foo2 :: Num a => APL a
foo2 = APL [5,6,7,8]
foo3 :: Fractional a => APL a
foo3 = APL [1.0,2.0,3.0,4.0]
foo4 :: Fractional a => APL a
foo4 = APL [5.0,6.0,7.0,8.0]

\end{code}

