module Midi where


data Event a = Wait Integer | Event a ;

data Channel a = Channel Integer a ;

data Message =
     PgmChange Integer
   | Controller Integer Integer
   | On Integer Integer
   | Off Integer Integer ;

note duration pitch velocity =
  [ Event (On pitch velocity)
  , Wait duration
  , Event (Off pitch velocity)
  ] ;

rest duration =
  [ Wait duration ] ;

program n =
  [ Event ( PgmChange n ) ] ;

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
