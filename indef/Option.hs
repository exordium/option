module Option where
import Prelude (Show(..))
import Elem
import Sig.Bool

data Option = None | Some {-# unpack #-} !T
deriving instance Show T => Show Option
option :: r -> (T -> r) -> Option -> r
option r tr = \case {None -> r; Some t -> tr t}
none :: Option
none = None
some :: T -> Option
some = Some

orElse :: Option -> Option -> Option
orElse a b = case a of {Some{} -> a; None -> b}

map :: (T -> T) -> Option -> Option
map f = \case {None -> None; Some t -> Some (f t)}

isSome :: Option -> Bool
isSome = \case {Some{} -> true; None -> false}

isNone :: Option -> Bool
isNone = \case {Some{} -> false; None -> true}

fromOption :: T -> Option -> T
fromOption t0 = \case {None -> t0; Some t -> t}

head' :: [T] -> Option
head' = \case {[] -> None; t:_ -> Some t}

toList :: Option -> [T]
toList = \case {None -> []; Some t -> [t]}
