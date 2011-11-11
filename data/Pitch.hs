module Pitch where

{- cf.
http://en.wikipedia.org/wiki/Scientific_pitch_notation
http://en.wikipedia.org/wiki/MIDI_Tuning_Standard
-}
pitch cls octave = cls + (octave+1)*12 ;

cb =  0 ; c = pitch cb ; cs = pitch (cb+1) ; cf = pitch (cb-1) ;
db =  2 ; d = pitch db ; ds = pitch (db+1) ; df = pitch (db-1) ;
eb =  4 ; e = pitch eb ; es = pitch (eb+1) ; ef = pitch (eb-1) ;
fb =  5 ; f = pitch fb ; fs = pitch (fb+1) ; ff = pitch (fb-1) ;
gb =  7 ; g = pitch gb ; gs = pitch (gb+1) ; gf = pitch (gb-1) ;
ab =  9 ; a = pitch ab ; as = pitch (ab+1) ; af = pitch (ab-1) ;
bb = 11 ; b = pitch bb ; bs = pitch (bb+1) ; bf = pitch (bb-1) ;
