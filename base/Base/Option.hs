module Base.Option (module Base.Option,Bool) where
import Base.Bool
import Data.Maybe
import Prelude

type Option = Maybe
none = Nothing
some = Just
option :: r -> (a -> r) -> Option a -> r
option = maybe

isNone,isSome :: Option a -> Bool
isNone = isNothing
isSome = isJust
