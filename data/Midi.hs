module Midi where


data Event a = Wait Integer | Say String | Event a ;

data Channel a = Channel Integer a ;

data Message =
     PgmChange Integer
   | Controller Integer Integer
   | On Integer Integer
   | Off Integer Integer ;

note :: Integer -> Integer -> [Event Message] ;
note duration pitch =
  [ Event (On pitch normalVelocity)
  , Wait duration
  , Event (Off pitch normalVelocity)
  ] ;

rest :: Integer -> [Event a] ;
rest duration =
  [ Wait duration ] ;

program :: Integer -> [Event Message] ;
program n =
  [ Event ( PgmChange n ) ] ;

controller :: Integer -> Integer -> [Event Message] ;
controller cc x =
  [ Event ( Controller cc x ) ] ;

channel :: Integer -> [Event a] -> [Event (Channel a)] ;
channel chan = map ( channelEvent chan ) ;

channelEvent :: Integer -> Event a -> Event (Channel a) ;
channelEvent chan (Event event) = Event (Channel chan event) ;
channelEvent _chan (Wait duration) = Wait duration ;
channelEvent _chan (Say text) = Say text ;


transpose :: Integer -> [Event Message] -> [Event Message] ;
transpose d = map ( transposeEvent d ) ;

transposeEvent :: Integer -> Event Message -> Event Message ;
transposeEvent d (Event (On pitch velocity)) = Event (On (pitch+d) velocity) ;
transposeEvent d (Event (Off pitch velocity)) = Event (Off (pitch+d) velocity) ;
transposeEvent _d event = event ;


controlCurve :: Integer -> Integer -> [Integer] -> [Event Message] ;
controlCurve _d _cc [] = [] ;
controlCurve d cc (x : xs) =
    Event (Controller cc x) : Wait d : controlCurve d cc xs ;

normalVelocity :: Integer ;
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
