module Introduction where

import Instrument
import Midi
import List
import Prelude

-- * basic functions

{-
Send a single MIDI message (and some AllNotesOff).
Verify with 'aseqdump'.
-}
single = Event ( PgmChange acousticGrandPiano ) : [] ;

{-
Press the middle C key.
-}
oneNoteOn = Event ( On 60 64 ) : [] ;

{-
Press the middle C key, wait and release it.
-}
oneNote = Event ( On 60 64 ) : Wait 1000 : Event ( Off 60 64 ) : [] ;

{-
Use functions from the Midi module.
-}
noteWithFunc = noteOn 60 : Wait 1000 : noteOff 60 : [] ;

{-
Use functions from the Pitch module.
-}
noteWithPitch = noteOn (c 4) : Wait 1000 : noteOff (c 4) : [] ;

{-
Play notes one after another.
-}
noteSequence =
    noteOn (c 4) : Wait 1000 : noteOff (c 4) :
    noteOn (e 4) : Wait 1000 : noteOff (e 4) :
    noteOn (g 4) : Wait 1000 : noteOff (g 4) :
    noteOn (c 5) : Wait 1000 : noteOff (c 5) :
    [] ;

{-
Play notes simultaneously.
-}
noteParallel =
    noteOn  (c 4) : noteOn  (e 4) : noteOn  (g 4) : noteOn  (c 5) :
    Wait 1000 :
    noteOff (c 4) : noteOff (e 4) : noteOff (g 4) : noteOff (c 5) :
    [] ;


dur = 1000 ;

{-
Use 'note' function.
-}
noteFunc = noteLazy dur (c 4) ;

{-
Play notes one after another using (++).
-}
noteFuncSequence =
    noteLazy dur (c 4) ++ noteLazy dur (e 4) ;

{-
Repeat a note by append to itself.
-}
noteFuncLoop =
    noteLazy dur (c 4) ++ noteFuncLoop ;

{-
Play notes simultaneously using (=:=).
-}
noteFuncParallel =
    noteLazy dur (c 4) =:= noteLazy dur (e 4) ;

noteFuncSequenceParallel =
    ( noteLazy dur (c 4) ++ noteLazy dur (g 4) )
    =:=
    noteLazy (2*dur) (e 4) ;


-- * laws

identitySequence = note dur (c 4) ;
leftIdentitySequence = [] ++ note dur (c 4) ;
rightIdentitySequence = note dur (c 4) ++ [] ;

leftAssociativeSequence =
   ( note dur (c 4) ++ note dur (e 4) ) ++ note dur (g 4) ;
rightAssociativeSequence =
   note dur (c 4) ++ ( note dur (e 4) ++ note dur (g 4) ) ;


identityParallel = note dur (c 4) ;
leftIdentityParallel = [] =:= note dur (c 4) ;
rightIdentityParallel = note dur (c 4) =:= [] ;

leftAssociativeParallel =
   ( note dur (c 4) =:= note dur (e 4) ) =:= note dur (g 4) ;
rightAssociativeParallel =
   note dur (c 4) =:= ( note dur (e 4) =:= note dur (g 4) ) ;

commutative0Parallel =
   note dur (c 4) =:= note dur (e 4) ;
commutative1Parallel =
   note dur (e 4) =:= note dur (c 4) ;

distributive0 =
   ( note dur (c 4) =:= note dur (g 4) )
   ++
   ( note dur (e 4) =:= note dur (c 5) ) ;

distributive1 =
   ( note dur (c 4) ++ note dur (e 4) )
   =:=
   ( note dur (g 4) ++ note dur (c 5) ) ;
