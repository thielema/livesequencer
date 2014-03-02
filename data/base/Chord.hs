module Chord where

import Midi
import Pitch ( Pitch )


chord ::
   Time -> [Pitch] ->
   [Midi.Event Midi.Message] ;
chord dur =
   mergeMany . map (note dur) ;


chord3 ::
   Time ->
   Pitch -> Pitch -> Pitch ->
   [Midi.Event Midi.Message] ;
chord3 dur p0 p1 p2 = chord dur [p0, p1, p2] ;

chord4 ::
   Time ->
   Pitch -> Pitch -> Pitch -> Pitch ->
   [Midi.Event Midi.Message] ;
chord4 dur p0 p1 p2 p3 = chord dur [p0, p1, p2, p3] ;


major, major7, minor, minor7 ::
   Time -> Pitch -> [Midi.Event Midi.Message] ;

major dur base =
   chord4 dur base (base + 4) (base + 7) (base + 12) ;

major7 dur base =
   chord4 dur base (base + 4) (base + 7) (base + 10) ;

minor dur base =
   chord4 dur base (base + 3) (base + 7) (base + 12) ;

minor7 dur base =
   chord4 dur base (base + 3) (base + 7) (base + 10) ;
