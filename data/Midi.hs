module Midi where


data Event a = Wait Integer | Event a ;

data Channel a = Channel Integer a ;

data Message =
     PgmChange Integer
   | Controller Integer Integer
   | On Integer Integer
   | Off Integer Integer ;

note duration pitch =
  [ Event (On pitch normalVelocity)
  , Wait duration
  , Event (Off pitch normalVelocity)
  ] ;

rest duration =
  [ Wait duration ] ;

program n =
  [ Event ( PgmChange n ) ] ;

controller cc x =
  [ Event ( Controller cc x ) ] ;

-- this is just (map (channelEvent chan))
channel chan [] = [] ;
channel chan (x : xs) =
   channelEvent chan x : channel chan xs ;

channelEvent chan (Event event) = Event (Channel chan event) ;
channelEvent chan (Wait duration) = Wait duration ;


-- this is just (map (tranposeEvent d))
transpose d [] = [] ;
transpose d (x : xs) =
   transposeEvent d x : transpose d xs ;

transposeEvent d (Event (On pitch velocity)) = Event (On (pitch+d) velocity) ;
transposeEvent d (Event (Off pitch velocity)) = Event (Off (pitch+d) velocity) ;
transposeEvent d event = event ;


controlCurve d cc [] = [] ;
controlCurve d cc (x : xs) =
    Event (Controller cc x) : Wait d : controlCurve d cc xs ;

normalVelocity = 64 ;

emphasize v [] = [] ;
emphasize v (x : xs) =
   emphasizeEvent v x : emphasize v xs ;

{-
We only alter the start velocity.
In most cases NoteOff velocity is the normal velocity
and this is handled more efficiently by the MIDI message encoding.
-}
emphasizeEvent v (Event (On pitch velocity)) = Event (On pitch (velocity+v)) ;
emphasizeEvent v event = event ;
