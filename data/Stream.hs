module Stream where

import Midi
import List

quarter = 240 ;

p = Wait quarter ;

c = Seq ( note quarter 36 ) ; 
cf = down 1 c ; cs = up 1 c ; 
df = cs ; d = up 2 c ; ds = up 3 c ;
ef = ds ; e = up 4 c ; es = f ;
ff = e ; f = up 5 c ; fs = up 6 c ;
gf = fs ; g = up 7 c ; gs = up 8 c ;
af = gs ; a = up 9 c ; as = up 10 c ;
bf = as ; b = up 11 c ; bs = up 12 c ;

slow f = wmap ( slowdown f ) ; slowdown f ( Wait w ) = Wait (  w * f ) ;
speed f = wmap ( speedup f ) ; speedup f ( Wait w ) = Wait (  w / f ) ;

up d s = tr d s ; down d s = tr ( negate d ) s ;

chan c = emap (channelEvent c) ;
tr   d = emap ( transposeEvent d ) ;

says [] = Seq [] ;
says ( w:ws ) = Seq [ Say w , p, says ws ] ;

major s = Par [ s, up 4 s, up 7 s ] ;
minor s = Par [ s, up 3 s, up 7 s ] ;
minor7 s = Par [ s, up 3 s, up 7 s, up 11 s ] ;

times 0 s = Seq [] ;
times k s = Seq [ s, times (k-1) s ] ;

emap f ( Event e ) = f ( Event e ) ;
emap f ( Par xs ) = Par ( map ( emap f ) xs );
emap f ( Seq xs ) = Seq ( map ( emap f ) xs );
emap f x = x ;

wmap f ( Wait w ) = f ( Wait w ) ;
wmap f ( Par xs ) = Par ( map ( wmap f ) xs );
wmap f ( Seq xs ) = Seq ( map ( wmap f ) xs );
wmap f x = x ;

forever s = Seq [ s , forever s ] ;

data Stream a = Event a 
       | Par [Stream] 
       | Seq [Stream] ;


play ( Par [] ) = [] ; play ( Par [x] ) = play x ;
play (Par (x:xs)) =  merge ( play x ) ( play (Par xs )) ;
play (Seq [] ) = [] ; play ( Seq [x] ) = play x ;
play ( Seq (x:xs)) = append ( play x ) ( play (Seq xs)) ;
play e = [ e ] ;




