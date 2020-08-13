module  Envisage.Logger where

import Prelude
import Data.Tuple(Tuple(..))

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console


main :: Effect Unit
main = Console.log $ show $ runLoggerT $ loggerT ([] :: Array String) (Just Nothing :: Maybe (Maybe Int))

data LoggerT md a x = LoggerT md (a x)

instance functorLogger :: (Functor a) => Functor (LoggerT md a) where
  map f (LoggerT logs aM) = LoggerT logs (map f aM)

instance applyLogger :: (Semigroup md, Apply a) => Apply (LoggerT md a) where
  apply (LoggerT logsA fM) (LoggerT logsB vM) = LoggerT (logsA <> logsB) (fM <*> vM)

instance applicativeLogger :: (Monoid md, Applicative a) => Applicative (LoggerT md a) where
  pure x = LoggerT mempty $ pure x

loggerT :: forall w f a. w -> f a -> LoggerT w f a
loggerT = LoggerT

runLoggerT :: forall w f a. LoggerT w f a -> Tuple w (f a)
runLoggerT (LoggerT w fa) = Tuple w fa

-- FIXME can generalise as LoggerT with sub-type of reader? (rename above to Logger)?
data ReaderLoggerT env md a x = ReaderLoggerT (env -> LoggerT md a x)

instance functorReaderLogger :: (Functor a) => Functor (ReaderLoggerT env md a) where
  map f (ReaderLoggerT r) = ReaderLoggerT $ \env -> map f (r env)

instance applyReaderLogger :: (Semigroup md, Apply a) => Apply (ReaderLoggerT env md a) where
  apply (ReaderLoggerT ff) (ReaderLoggerT g) = ReaderLoggerT $ \env -> (ff env) <*> (g env)

instance applicativeReaderLogger :: (Monoid md, Applicative a) => Applicative (ReaderLoggerT env md a) where
  pure x = ReaderLoggerT $ const (pure x)

runReaderLoggerT :: forall env md a x. env -> ReaderLoggerT env md a x -> Tuple md (a x)
runReaderLoggerT env (ReaderLoggerT r) = runLoggerT (r env)

readerLoggerT :: forall env md a x. (env -> LoggerT md a x) -> ReaderLoggerT env md a x
readerLoggerT = ReaderLoggerT
