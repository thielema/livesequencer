c = 60 ; d = 62 ; e = 64 ; f = 65 ; g = 67 ; a = 69 ; h = 71 ;

dur delta base vel =
   merge (note delta base vel)
       (merge (note delta (plus base 4) vel)
           (merge (note delta (plus base 7) vel)
                  ( note delta (plus base 12) vel)))  ;

dur7 delta base vel =
   merge (note delta base vel)
       (merge (note delta (plus base 4) vel)
           (merge (note delta (plus base 7) vel)
                  ( note delta (plus base 10) vel)))  ;

moll delta base vel =
   merge (note delta base vel)
       (merge (note delta (plus base 3) vel)
           (merge (note delta (plus base 7) vel)
                  ( note delta (plus base 12) vel)))  ;

moll7 delta base vel =
   merge (note delta base vel)
       (merge (note delta (plus base 3) vel)
           (merge (note delta (plus base 7) vel)
                  ( note delta (plus base 10) vel)))  ;
