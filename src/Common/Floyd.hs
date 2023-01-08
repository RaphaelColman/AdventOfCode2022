module Common.Floyd where

hareAndTortoise :: (Eq b) =>
  (a -> a) -> --Function move from one node to the next
  a -> --Initial state
  (a -> b) -> --Function to compare if two nodes are equal
  Maybe (Int, Int, a) -- (Index of beginning of cycle, Length of cycle, f(a) for beginning of cycle)
hareAndTortoise f state eqFun = do
  let hare = map eqFun $ iterate (f . f) state
  let tortoise = map eqFun $ iterate f state
  let pairs = zip hare tortoise
  pure undefined

