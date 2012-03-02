module Midi (
    Time,
    Velocity,
    Program,
    Controller,
    Chan,
    Event(..),
    Channel(..),
    Message(..),

    note,
    rest,
    program,
    controller,
    channel,
    transpose, transposeEvent,
    controlCurve,
    normalVelocity,
    emphasize,

    (+:+),
    merge, (=:=),
    mergeWait,
    mergeMany,
    ) where

import Function
import Pitch ( Pitch )


type Time = Integer ;
type Velocity = Integer ;
type Program = Integer ;
type Controller = Integer ;
type Chan = Integer ;

data Event a = Wait Time | Say String | Event a ;

data Channel a = Channel Integer a ;

data Message =
     PgmChange Program
   | Controller Controller Integer
   | On Pitch Velocity
   | Off Pitch Velocity ;

{- |
This function is strict in the pitch
and thus asserts that the pitch for NoteOn and NoteOff
are evaluated at the same time to the same value.
This way we assert that a pressed note
will be released later.
-}
note :: Time -> Pitch -> [Event Message] ;
note duration = applyStrict (noteLazy duration) ;

noteLazy :: Time -> Pitch -> [Event Message] ;
noteLazy duration pitch =
  [ Event (On pitch normalVelocity)
  , Wait duration
  , Event (Off pitch normalVelocity)
  ] ;

rest :: Time -> [Event a] ;
rest duration =
  [ Wait duration ] ;

program :: Program -> [Event Message] ;
program n =
  [ Event ( PgmChange n ) ] ;

controller :: Controller -> Integer -> [Event Message] ;
controller cc x =
  [ Event ( Controller cc x ) ] ;

channel :: Chan -> [Event a] -> [Event (Channel a)] ;
channel chan = map ( channelEvent chan ) ;

channelEvent :: Chan -> Event a -> Event (Channel a) ;
channelEvent chan (Event event) = Event (Channel chan event) ;
channelEvent _chan (Wait duration) = Wait duration ;
channelEvent _chan (Say text) = Say text ;


transpose :: Integer -> [Event Message] -> [Event Message] ;
transpose d = map ( transposeEvent d ) ;

transposeEvent :: Integer -> Event Message -> Event Message ;
transposeEvent d (Event (On pitch velocity)) = Event (On (pitch+d) velocity) ;
transposeEvent d (Event (Off pitch velocity)) = Event (Off (pitch+d) velocity) ;
transposeEvent _d event = event ;


controlCurve :: Time -> Controller -> [Integer] -> [Event Message] ;
controlCurve _d _cc [] = [] ;
controlCurve d cc (x : xs) =
    Event (Controller cc x) : Wait d : controlCurve d cc xs ;

normalVelocity :: Velocity ;
normalVelocity = 64 ;

emphasize :: Integer -> [Event Message] -> [Event Message] ;
emphasize v = map ( emphasizeEvent v ) ;

{-
We only alter the start velocity.
In most cases NoteOff velocity is the normal velocity
and this is handled more efficiently by the MIDI message encoding.
-}
emphasizeEvent :: Integer -> Event Message -> Event Message ;
emphasizeEvent v (Event (On pitch velocity)) = Event (On pitch (velocity+v)) ;
emphasizeEvent _v event = event ;


infixr 7 +:+ ;  {- like multiplication -}
infixr 6 =:= ;  {- like addition -}

(+:+) :: [Midi.Event a] -> [Midi.Event a] -> [Midi.Event a] ;
xs +:+ ys  =  xs ++ ys ;

merge, (=:=) :: [Midi.Event a] -> [Midi.Event a] -> [Midi.Event a] ;
xs =:= ys  =  merge xs ys ;

merge (Wait a : xs) (Wait b : ys) =
  mergeWait (a<b) (a-b) a xs b ys ;
merge (Wait a : xs) (y : ys) =
  y : merge (Wait a : xs) ys ;
merge (x : xs) ys = x : merge xs ys ;
merge [] ys = ys ;

{-
This looks a bit cumbersome,
but it is necessary for avoiding stacks of unevaluated subtractions.
We use or abuse the way of how the interpreter performs pattern matching.
By matching against 0 we force the evaluation of the difference d.
The evaluated difference is hold throughout the matching of all patterns.
It is important that the match against 0 is really performed
and is not shadowed by a failing preceding match, say, against the result of (a<b).
-}
mergeWait ::
  Bool -> Midi.Time ->
  Midi.Time -> [Midi.Event a] ->
  Midi.Time -> [Midi.Event a] ->
  [Midi.Event a] ;
mergeWait _eq 0 a xs _b ys =
  Wait a : merge xs ys ;
mergeWait True d a xs _b ys =
  Wait a : merge xs (Wait (negate d) : ys) ;
mergeWait False d _a xs b ys =
  Wait b : merge (Wait d : xs) ys ;

mergeMany :: [[Midi.Event a]] -> [Midi.Event a] ;
mergeMany = foldl merge [] ;
