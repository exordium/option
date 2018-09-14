module OptionCore where

newtype Option a = Option (forall r. r -> (a -> r) -> r)

option :: r -> (a -> r) -> Option a -> r
option r0 ar (Option k) = k r0 ar

none :: Option a
none = Option \r0 _ -> r0

some :: a -> Option a
some a = Option \_ ar -> ar a
