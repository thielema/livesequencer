module Utility.Concurrent where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Monad.STM ( STM )
import qualified Control.Monad.STM as STM

import Utility ( void )


writeTMVar :: TMVar a -> a -> STM ()
writeTMVar var a =
    clearTMVar var >> putTMVar var a

clearTMVar :: TMVar a -> STM ()
clearTMVar var =
    void $ tryTakeTMVar var

writeTChanIO :: TChan a -> a -> IO ()
writeTChanIO chan a =
    STM.atomically $ writeTChan chan a
