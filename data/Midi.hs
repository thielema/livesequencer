module Midi where

import List

data Event a = Wait Integer | Event a ;

data Channel a = Channel Integer a ;

data Message =
     PgmChange Integer
   | On Integer Integer
   | Off Integer Integer ;

note duration pitch velocity =
  [ Event (On pitch velocity)
  , Wait duration
  , Event (Off pitch velocity)
  ] ;

program n =
  [ Event ( PgmChange n ) ] ;

-- this is just (map (channelEvent chan))
channel chan Nil = Nil ;
channel chan (Cons x xs) =
   Cons (channelEvent chan x) (channel chan xs) ;

channelEvent chan (Event event) = Event (Channel chan event) ;
channelEvent chan (Wait duration) = Wait duration ;
