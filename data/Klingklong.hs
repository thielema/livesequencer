module Klingklong where

import Pitch
import Midi
import List
import Prelude ( (*) )


main = channel 0 loop0 ;

en = 150 ;
qn = 2 * en ;
hn = 2 * qn ;


loop0 = append ( note qn (c 4) ) loop0 ;

loop1 = append ( concat [
  note qn (e 4), note qn (c 4),
  note qn (e 4), note qn (c 4),
  note qn (g 4), note en (f 4), note en (e 4),
  note en (d 4), note en (e 4), note en (f 4), note en (d 4) ] ) loop1 ;

loop2 = append ( concat [
  note qn (e 4), note qn (a 4),
  note qn (e 4), note qn (a 4),
  note qn (a 4), note en (b 4), note en (c 5),
  note en (d 5), note en (c 5), note en (b 4), note en (a 4) ] ) loop2 ;

loop3 = append ( concat [
  note qn (e 4), note qn (a 4),
  note qn (e 4), note qn (a 4),
  note qn (a 4), note en (b 4), note en (c 5),
  note en (d 5), note en (c 5), note en (b 4), note en (g 4) ] ) loop1 ;
