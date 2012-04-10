module Utility.Concurrent where

import Control.Concurrent.STM.TMVar
import Control.Monad.STM ( STM )

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Exception.Synchronous as Exc
import Data.Monoid ( Monoid )

import Control.Functor.HT ( void )


writeTMVar :: TMVar a -> a -> STM ()
writeTMVar var a =
    clearTMVar var >> putTMVar var a

clearTMVar :: TMVar a -> STM ()
clearTMVar var =
    void $ tryTakeTMVar var


class Monad m => MonadSTM m where
    liftSTM :: STM a -> m a

instance MonadSTM STM where
    liftSTM = id

instance (MonadSTM m, Monoid w) => MonadSTM (MW.WriterT w m) where
    liftSTM = MT.lift . liftSTM

instance MonadSTM m => MonadSTM (MS.StateT s m) where
    liftSTM = MT.lift . liftSTM

instance MonadSTM m => MonadSTM (Exc.ExceptionalT e m) where
    liftSTM = MT.lift . liftSTM
