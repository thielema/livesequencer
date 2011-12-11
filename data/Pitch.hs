module Pitch where

{- cf.
http://en.wikipedia.org/wiki/Scientific_pitch_notation
http://en.wikipedia.org/wiki/MIDI_Tuning_Standard
-}
pitch cls octave = cls + (octave+1)*12 ;

cb =  0 ; c = pitch cb ; cis = cs ; ces = cf ;
db =  2 ; d = pitch db ; dis = ds ; des = df ;
eb =  4 ; e = pitch eb ; eis = es ; ees = ef ;
fb =  5 ; f = pitch fb ; fis = fs ; fes = ff ;
gb =  7 ; g = pitch gb ; gis = gs ; ges = gf ;
ab =  9 ; a = pitch ab ; ais = as ; aes = af ;
bb = 11 ; b = pitch bb ; bis = bs ; bes = bf ;

cs = pitch (cb+1) ; cf = pitch (cb-1) ;
ds = pitch (db+1) ; df = pitch (db-1) ;
es = pitch (eb+1) ; ef = pitch (eb-1) ;
fs = pitch (fb+1) ; ff = pitch (fb-1) ;
gs = pitch (gb+1) ; gf = pitch (gb-1) ;
as = pitch (ab+1) ; af = pitch (ab-1) ;
bs = pitch (bb+1) ; bf = pitch (bb-1) ;
