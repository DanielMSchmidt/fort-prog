module A5_FunList where

  type FunList a = [a] -> [a]

  -- Konversion
  fromList :: [a] -> FunList a
  toList   :: FunList a -> [a]

  -- Konstruktion
  empty     :: FunList a
  singleton :: a -> FunList a
  cons      :: a -> FunList a -> FunList a
  snoc      :: FunList a -> a -> FunList a
  append    :: FunList a -> FunList a -> FunList a
  concatF   :: [FunList a] -> FunList a

  -- Mutation
  foldrF :: (a -> b -> b) -> b -> FunList a -> b
  mapF   :: (a -> b) -> FunList a -> FunList b

  -- Selektion, Test
  headF   :: FunList a -> a
  tailF   :: FunList a -> FunList a
  isEmpty :: FunList a -> Bool