module Stream where

import Midi ( Event(Wait, Event, Say), note, Channel(Channel), transposeEvent )
import List ( map, replicate, repeat, concat, mergeMany, afterEach )
import Prelude ( (*), div, (.), ($), negate )


quarter = 240 ;

p = Atom ( Wait quarter ) ;

c = Seq $ map Atom $ note quarter 36 ;
cf = down 1 c ; cs = up 1 c ;
df = cs ; d = up 2 c ; ds = up 3 c ;
ef = ds ; e = up 4 c ; es = f ;
ff = e ; f = up 5 c ; fs = up 6 c ;
gf = fs ; g = up 7 c ; gs = up 8 c ;
af = gs ; a = up 9 c ; as = up 10 c ;
bf = as ; b = up 11 c ; bs = up 12 c ;

slow f = wmap ( slowdown f ) ; slowdown f w = w * f ;
speed f = wmap ( speedup f ) ; speedup f w = div w f ;

up d s = tr d s ; down d s = tr ( negate d ) s ;

chan c = emap ( Channel c ) ;
tr   d = amap ( transposeEvent d ) ;

says ws = Seq ( afterEach p ( map (Atom . Say) ws ) ) ;

major s = Par [ s, up 4 s, up 7 s ] ;
minor s = Par [ s, up 3 s, up 7 s ] ;
minor7 s = Par [ s, up 3 s, up 7 s, up 11 s ] ;

times k s = Seq ( replicate k s ) ;

emap f ( Atom ( Event e ) ) = Atom ( Event ( f e ) ) ;
emap f ( Atom ( Wait w ) ) = Atom ( Wait w ) ;
emap f ( Atom ( Say s ) ) = Atom ( Say s ) ;
emap f ( Par xs ) = Par ( map ( emap f ) xs );
emap f ( Seq xs ) = Seq ( map ( emap f ) xs );

wmap f ( Atom ( Event e ) ) = Atom ( Event e ) ;
wmap f ( Atom ( Wait w ) ) = Atom ( Wait ( f w ) ) ;
wmap f ( Atom ( Say s ) ) = Atom ( Say s ) ;
wmap f ( Par xs ) = Par ( map ( wmap f ) xs );
wmap f ( Seq xs ) = Seq ( map ( wmap f ) xs );

amap f ( Atom a ) = Atom ( f a ) ;
amap f ( Par xs ) = Par ( map ( amap f ) xs );
amap f ( Seq xs ) = Seq ( map ( amap f ) xs );


forever s = Seq ( repeat s ) ;

data Stream a =
         Atom a
       | Par [Stream a]
       | Seq [Stream a] ;


play (Par xs) = mergeMany ( map play xs ) ;
play (Seq xs) = concat ( map play xs ) ;
play (Atom e) = [ e ] ;
