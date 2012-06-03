module Distributed.Bass where

import Midi
import Pitch
import List
import Prelude ( Int, (*), (-), (.), ($) )


bassTrack :: Time -> [[Pitch]] -> [Event (Channel Message)] ;
bassTrack dur set =
    channel 2 $ emphasize 10 $
        program 24 ++
        concat (
            zipWith
                ( applyBass dur )
                bassLoop
                ( concat $
                  map ( replicate ( length bassPattern ) ) $
                  cycle set )
        ) ;

bassNote :: Time -> Int -> Time -> [Pitch.Pitch] -> [Event Message] ;
bassNote relDur i baseDur set =
    note (relDur*baseDur) (set!!i) ;

bassRest :: Time -> Time -> t -> [Event a] ;
bassRest relDur baseDur _set =
    rest (relDur*baseDur) ;

applyBass :: time -> (time -> set -> a) -> set -> a ;
applyBass baseDur consBass set =
    consBass baseDur set ;

bassPattern, bassLoop :: [Time -> [Pitch.Pitch] -> [Event Message]] ;

--------------------------------
bassPattern =
    [ bassNote 1 0, bassRest 3,
      bassNote 2 0, bassNote 4 1, bassNote 2 2 ] ;

bassLoop =
    bassPattern ++ bassLoop ;
