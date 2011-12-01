module DrumTrack where

import List
import Midi
import Drum
import Prelude ( (*), (-), (.), ($) )

drumTrack dur =
    drumChannel $
        program 0 ++
        mergeMany [
            concat $ map (applyDrum dur bassDrum1)     bassDrumLoop,
            concat $ map (applyDrum dur acousticSnare) snareDrumLoop,
            concat $ map (applyDrum dur openHiHat)     hihatLoop
        ] ;


x drm dur =
    drum drm dur ;

o drm dur =
    rest dur ;

applyDrum dur drm f =
    f drm dur ;

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
