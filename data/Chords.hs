module Chords where

import Midi
import List

major delta base vel =
   merge (note delta base vel)
       (merge (note delta (base + 4) vel)
           (merge (note delta (base + 7) vel)
                  ( note delta (base + 12) vel)))  ;

major7 delta base vel =
   merge (note delta base vel)
       (merge (note delta (base + 4) vel)
           (merge (note delta (base + 7) vel)
                  ( note delta (base + 10) vel)))  ;

minor delta base vel =
   merge (note delta base vel)
       (merge (note delta (base + 3) vel)
           (merge (note delta (base + 7) vel)
                  ( note delta (base + 12) vel)))  ;

minor7 delta base vel =
   merge (note delta base vel)
       (merge (note delta (base + 3) vel)
           (merge (note delta (base + 7) vel)
                  ( note delta (base + 10) vel)))  ;
