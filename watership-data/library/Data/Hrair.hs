{-# LANGUAGE DerivingStrategies, DeriveFunctor #-}

import Data.Foldable (fold)

-- | Rabbits can count up to one. Any number above
-- one is Hrair -- "a lot," or "a thousand."
data One a = Zero | One a | Hrair
  deriving stock (Eq, Ord, Show, Functor)

instance Semigroup (One a) where
  Zero <> x = x
  x <> Zero = x
  _ <> _ = Hrair

instance Monoid (One a) where
  mempty = Zero

test :: [Rabbit String] -> IO ()
test = print . fold

main =
  do
    test [zero, zero, zero, zero]
    test [zero, one "Hazel", zero, zero]
    test [zero, one "Hazel", zero, one "Bigwig"]
