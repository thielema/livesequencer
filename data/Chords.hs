module Chords where

import Midi
import List

dur delta base vel =
   merge (note delta base vel)
       (merge (note delta (base + 4) vel)
           (merge (note delta (base + 7) vel)
                  ( note delta (base + 12) vel)))  ;

dur7 delta base vel =
   merge (note delta base vel)
       (merge (note delta (base + 4) vel)
           (merge (note delta (base + 7) vel)
                  ( note delta (base + 10) vel)))  ;

moll delta base vel =
   merge (note delta base vel)
       (merge (note delta (base + 3) vel)
           (merge (note delta (base + 7) vel)
                  ( note delta (base + 12) vel)))  ;

moll7 delta base vel =
   merge (note delta base vel)
       (merge (note delta (base + 3) vel)
           (merge (note delta (base + 7) vel)
                  ( note delta (base + 10) vel)))  ;
