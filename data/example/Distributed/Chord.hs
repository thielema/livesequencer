module Distributed.Chord where

import Distributed.Utility
import Pitch
import List
import Midi
import Prelude ( Int, (*), (-), (.), ($) )

chordTrack :: Time -> [[Pitch]] -> [Event (Channel Message)] ;
chordTrack dur harmonies =
    channel 1 $ emphasize (0-30) $
        program 0 ++
        concat (
            map ( mergeMany . map ( note dur ) ) $
            zipWith multiIndex
                ( concat $ map ( replicate 12 ) $
                  cycle harmonies )
                chordLoop
        ) ;

chordLoop :: [[Int]] ;

--------------------------------
chordLoop =
    [[0], [1, 2], [1, 3], [1, 2]]
    ++
    chordLoop ;
