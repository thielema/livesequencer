module Controls where
{-
Do not alter this module!
The live-sequencer relies on the module content as it is.
-}

checkBox :: String -> Bool -> Bool ;
checkBox _name deflt = deflt ;

slider :: String -> Int -> Int -> Int -> Int ;
slider _name _lower _upper deflt = deflt ;
