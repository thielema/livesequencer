module Distributed.Bass where

import Distributed.Utility
import List
import Midi
import Prelude ( (*), (-), (.), ($) )

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

bassNote relDur i baseDur set =
    note (relDur*baseDur) (set!!i) ;

bassRest relDur baseDur set =
    rest (relDur*baseDur) ;

applyBass baseDur b set =
    b baseDur set ;

--------------------------------

bassPattern =
    [ bassNote 1 0, bassRest 3,
      bassNote 2 0, bassNote 4 1, bassNote 2 2 ] ;

bassLoop =
    bassPattern ++ bassLoop ;
