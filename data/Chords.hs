module Chords where

import Midi
import List

major delta base =
   merge (note delta base)
       (merge (note delta (base + 4))
           (merge (note delta (base + 7))
                  ( note delta (base + 12))))  ;

major7 delta base =
   merge (note delta base)
       (merge (note delta (base + 4))
           (merge (note delta (base + 7))
                  ( note delta (base + 10))))  ;

minor delta base =
   merge (note delta base)
       (merge (note delta (base + 3))
           (merge (note delta (base + 7))
                  ( note delta (base + 12))))  ;

minor7 delta base =
   merge (note delta base)
       (merge (note delta (base + 3))
           (merge (note delta (base + 7))
                  ( note delta (base + 10))))  ;
