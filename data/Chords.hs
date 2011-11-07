module Chords where

import Midi
import List

chord4 dur p0 p1 p2 p3 =
   mergeMany [
      note dur p0,
      note dur p1,
      note dur p2,
      note dur p3
   ] ;

major dur base =
   chord4 dur base (base + 4) (base + 7) (base + 12) ;

major7 dur base =
   chord4 dur base (base + 4) (base + 7) (base + 10) ;

minor dur base =
   chord4 dur base (base + 3) (base + 7) (base + 12) ;

minor7 dur base =
   chord4 dur base (base + 3) (base + 7) (base + 10) ;
