{-# language UnboxedTuples, UnboxedSums #-}
module Utils where
import Sig.Bool
import Sig.Option

pattern None :: Option a
pattern None <- (option N S -> N) where None = none
pattern Some :: a -> Option a
pattern Some a <- (option N S -> S a) where Some = some
{-# complete None,Some #-}

isNone,isSome :: Option a -> Bool
isNone = option true (\_ -> false)
isSome = option false (\_ -> true)

fromOption :: a -> Option a -> a
fromOption a0 = option a0 (\a -> a)

map :: (a -> b) -> Option a -> Option b
map f = option none (\a -> some (f a))

head' :: [a] -> Option a
head' = \case {[] -> none; a:_ -> some a}

toList :: Option a -> [a]
toList = option [] (:[])

-- | Internal type for pattern matching
data O a = N | S a
