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

type Seconds      a = Time One   a
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



-- | we check by the types whether we can format the time value or not
class Format factor where
    formatUnit :: Time factor a -> String

instance Format One where
    formatUnit = const "s"


class Format1 factor where
    formatUnit1 :: Time (EM3 factor) a -> String

instance Format1 One where
    formatUnit1 = const "ms"

instance Format1 factor => Format (EM3 factor) where
    formatUnit = formatUnit1


class Format2 factor where
    formatUnit2 :: Time (EM3 (EM3 factor)) a -> String

instance Format2 One where
    formatUnit2 = const "us"

instance Format2 factor => Format1 (EM3 factor) where
    formatUnit1 = formatUnit2


class Format3 factor where
    formatUnit3 :: Time (EM3 (EM3 (EM3 factor))) a -> String

instance Format3 One where
    formatUnit3 = const "ns"

instance Format3 factor => Format2 (EM3 factor) where
    formatUnit2 = formatUnit3


format :: (Format factor, Show a) => Time factor a -> String
format time@(Time t) = show t ++ formatUnit time
