module Midi where

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
