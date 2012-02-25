module Music where

import Midi ( Event(Wait, Event, Say), Message, Channel(Channel),
              note, transposeEvent )
import List ( map, replicate, repeat, concat, mergeMany, afterEach )
import Prelude ( (*), div, (.), ($), negate, Int, Integer, Integral, String )


c, cf, cs, df, d, ds, ef, e, es, ff, f, fs,
  gf, g, gs, af, a, as, bf, b, bs ::
    Music (Event Message) ;

quarter :: Integer ;
quarter = 240 ;

p :: Music (Event a) ;
p = Atom ( Wait quarter ) ;

c = Seq $ map Atom $ note quarter 36 ;
cf = down 1 c ; cs = up 1 c ;
df = cs ; d = up 2 c ; ds = up 3 c ;
ef = ds ; e = up 4 c ; es = f ;
ff = e ; f = up 5 c ; fs = up 6 c ;
gf = fs ; g = up 7 c ; gs = up 8 c ;
af = gs ; a = up 9 c ; as = up 10 c ;
bf = as ; b = up 11 c ; bs = up 12 c ;

slow, speed :: Integer -> Music (Event a) -> Music (Event a) ;

slowdown, speedup :: (Integral a) => a -> a -> a ;

slow k = wmap ( slowdown k ) ; slowdown k w = w * k ;
speed k = wmap ( speedup k ) ; speedup k w = div w k ;

up, down ::
  Integer ->
  Music (Event Message) ->
  Music (Event Message) ;
up dif s = tr dif s ; down dif s = tr ( negate dif ) s ;

chan ::
  Integer ->
  Music (Event a) ->
  Music (Event (Midi.Channel a)) ;
chan cn = emap ( Channel cn ) ;

tr ::
  Integer ->
  Music (Event Message) ->
  Music (Event Message) ;
tr dif = amap ( transposeEvent dif ) ;

says :: [String] -> Music (Event a) ;
says ws = Seq ( afterEach p ( map (Atom . Say) ws ) ) ;

major, minor, minor7 ::
  Music (Event Message) -> Music (Event Message) ;
major s = Par [ s, up 4 s, up 7 s ] ;
minor s = Par [ s, up 3 s, up 7 s ] ;
minor7 s = Par [ s, up 3 s, up 7 s, up 11 s ] ;

times :: Integer -> Music a -> Music a ;
times k s = Seq ( replicate k s ) ;

emap :: (a -> b) -> Music (Event a) -> Music (Event b) ;
emap fn ( Atom ( Event ev ) ) = Atom ( Event ( fn ev ) ) ;
emap _  ( Atom ( Wait w ) ) = Atom ( Wait w ) ;
emap _  ( Atom ( Say s ) ) = Atom ( Say s ) ;
emap fn ( Par xs ) = Par ( map ( emap fn ) xs );
emap fn ( Seq xs ) = Seq ( map ( emap fn ) xs );

wmap ::
  (Integer -> Integer) ->
  Music (Event a) -> Music (Event a) ;
wmap _  ( Atom ( Event ev ) ) = Atom ( Event ev ) ;
wmap fn ( Atom ( Wait w ) ) = Atom ( Wait ( fn w ) ) ;
wmap _  ( Atom ( Say s ) ) = Atom ( Say s ) ;
wmap fn ( Par xs ) = Par ( map ( wmap fn ) xs );
wmap fn ( Seq xs ) = Seq ( map ( wmap fn ) xs );

amap :: (a -> b) -> Music a -> Music b ;
amap fn ( Atom atom ) = Atom ( fn atom ) ;
amap fn ( Par xs ) = Par ( map ( amap fn ) xs );
amap fn ( Seq xs ) = Seq ( map ( amap fn ) xs );


forever :: Music a -> Music a ;
forever s = Seq ( repeat s ) ;

data Music a =
         Atom a
       | Par [Music a]
       | Seq [Music a] ;


play :: Music (Event a) -> [Event a] ;
play (Par xs) = mergeMany ( map play xs ) ;
play (Seq xs) = concat ( map play xs ) ;
play (Atom atom) = [ atom ] ;
