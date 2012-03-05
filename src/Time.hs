{-# LANGUAGE EmptyDataDecls #-}
module Time where

import Control.Concurrent ( threadDelay )
import qualified Data.Monoid as Mn


newtype Time factor a = Time a
    deriving (Eq, Ord, Show)

instance Functor (Time factor) where
    fmap f (Time a) = Time (f a)


instance (Num a) => Mn.Monoid (Time factor a) where
    mempty = Time 0
    mappend (Time x) (Time y) = Time (x+y)

sub :: Num a => Time factor a -> Time factor a -> Time factor a
sub (Time x) (Time y) = Time (x-y)


data One
data EM3 a

type Milli = EM3 One
type Micro = EM3 Milli
type Nano  = EM3 Micro

type Milliseconds a = Time Milli a  -- unit in Wait constructor
type Microseconds a = Time Micro a  -- unit of threadDelay
type Nanoseconds  a = Time Nano  a  -- unit of ALSA realtime


up :: Num a => Time factor a -> Time (EM3 factor) a
up (Time a) = Time (1000*a)


class Factor factor where
    seconds :: Num a => a -> Time factor a

instance Factor One where
    seconds = Time

instance Factor factor => Factor (EM3 factor) where
    seconds = up . seconds


mul3 :: Time factor a -> Time (EM3 factor) a
mul3 (Time t) = Time t

milliseconds ::
    (Factor factor, Num a) =>
    a -> Time (EM3 factor) a
milliseconds =
    mul3 . seconds

nanoseconds ::
    (Factor factor, Num a) =>
    a -> Time (EM3 (EM3 (EM3 factor))) a
nanoseconds =
    mul3 . mul3 . mul3 . seconds


pause :: Time Micro Int -> IO ()
pause (Time t) = threadDelay t
