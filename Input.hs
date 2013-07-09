{-# LANGUAGE GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , UndecidableInstances
           , FlexibleInstances
           , MultiParamTypeClasses #-}

module Input(
      InputT
    , runInputT
    , input
    , Input
    ) where

import Control.Monad.State
import Control.Monad.Identity

newtype InputT s m a = InputT (StateT [s] m a)
    deriving (Monad, MonadTrans, Functor, MonadFix, MonadPlus, MonadIO)

runInputT :: InputT s m a -> [s] -> m (a, [s])
runInputT (InputT m) = runStateT m

instance (MonadState s m) => MonadState s (InputT c m) where
    get = lift get
    put = lift . put

input :: Monad m => InputT s m s
input = do
    (x:xs) <- InputT get
    InputT (put xs)
    return x

type Input s = InputT s Identity
