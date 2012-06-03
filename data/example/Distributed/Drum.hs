module Distributed.Drum where

import List
import Midi
import Drum
import Prelude ( (*), (-), (.), ($) )

drumTrack :: Time -> [Event (Channel Message)] ;
drumTrack dur =
    drumChannel $
        program 0 ++
        mergeMany [
            concat $ map (applyDrum dur bassDrum1)     bassDrumLoop,
            concat $ map (applyDrum dur acousticSnare) snareDrumLoop,
            concat $ map (applyDrum dur openHiHat)     hihatLoop
        ] ;


x, o :: Drum -> Time -> [Event Message] ;
x drm dur =
    drum drm dur ;

o _drm dur =
    rest dur ;

applyDrum :: time -> drum -> (drum -> time -> a) -> a ;
applyDrum dur drm f =
    f drm dur ;


bassDrumPattern, bassDrumLoop,
    snareDrumPattern, snareDrumLoop,
    hihatPattern, hihatLoop :: [Drum -> Time -> [Event Message]] ;

--------------------------------
bassDrumPattern =
    [ x, o, o, o, x, o, o, o, x, o, o, o ] ;

snareDrumPattern =
    [ o, o, x, o, o, o, x, o, o, o, x, o ] ;

hihatPattern =
    [ x, x, o, x, x, x, o, x, x, x, o, x ] ;


bassDrumLoop = bassDrumPattern ++ bassDrumLoop ;

snareDrumLoop = snareDrumPattern ++ snareDrumLoop ;

hihatLoop = hihatPattern ++ hihatLoop ;
