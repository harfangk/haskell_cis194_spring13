{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where 

import Data.Monoid

import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single m x)
  | i == 0 = Just x
indexJ i (Append m jl1 jl2)
  | i < 0 || i > (getSize . size $ m) = Nothing
  | i < size1 = indexJ i jl1
  | otherwise = indexJ (i - size1) jl2
    where size1 = getSize . size . tag $ jl1
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl@(Single _ _)
  | n <= 0 = jl
  | otherwise = Empty
dropJ n jl@(Append m jl1 jl2)
  | n >= size0 = Empty
  | n >= size1 = dropJ (n - size1) jl2
  | n > 0 = dropJ n jl1 +++ jl2
  | otherwise = Empty
  where size0 = getSize . size . tag $ jl
        size1 = getSize . size . tag $ jl1
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl@(Single _ _)
  | n >= 1 = jl
takeJ n jl@(Append m jl1 jl2)
  | n > size0 = jl
  | n > size1 = jl1 +++ (takeJ (n - size1) jl2)
  | n > 0 = takeJ n jl1
    where size0 = getSize . size $ m
          size1 = getSize . size . tag $ jl1
takeJ _ _ = Empty

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = []
  toString (Single _ a) = a
  toString (Append _ jl1 jl2) = toString jl1 ++ toString jl2
  fromString = foldl (\jl str -> jl +++ (Single (scoreString str, 1) str)) Empty . lines
  line = indexJ
  replaceLine n ln jl = takeJ n jl +++ fromString ln +++ dropJ (n+1) jl
  numLines = getSize . snd . tag
  value = getScore . fst . tag

main = runEditor editor (fromString . unlines $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: (JoinList (Score, Size) String))
