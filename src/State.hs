module State where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State h) = State $ \s -> let (result, s') = h s in (f result, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  (State h) <*> (State g) = State $ \s ->
    let (f, s') = h s
        (result, s'') = g s'
     in (f result, s'')

instance Monad (State s) where
  return = pure
  (State h) >>= f = State $ \s ->
    let (result, s') = h s
        (State g) = f result
     in g s'

evalState :: State s a -> s -> a
evalState h s = fst $ runState h s