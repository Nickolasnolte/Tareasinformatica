module Main exposing (..)

ifilterdivision z xs = case xs of
     [] -> []
     b::bs -> if modBy z b == 0  then b::ifilterdivision z bs else ifilterdivision z bs 

filter2 f xs = case xs of    
     [] -> []
     b::bs -> if f b == True  then b::filter2 f bs else filter2 f bs