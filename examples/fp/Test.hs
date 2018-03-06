module Test where

{-@ inv0 :: cl:Int -> xl:Int -> yl:Int ->
            cr:Int -> xr:Int -> yr:Int ->
            {v:Bool | v <=> (cl == cr && yl == yr)} @-}
inv0 :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
inv0 cl 0  0  cr 0  0  = cl == cr -- initial state
inv0 cl xl yl cr xr yr = 
  (inv0 cl 1 0 cr 1 0) ||         -- tag reset
  (let cl' = cl
       cr' = cr
       xl' = xl
       xr' = xr
       yl' = if cl >= 1 then xl else yl
       yr' = if cr >= 1 then xr else yr
   in  inv0 cl' xl' yl' cr' xr' yr') -- transition relation

inv0_prop :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
inv0_prop cl xl yl cr xr yr = 
  if   inv0 cl xl yl cr xr yr && yl > 0 && yr <= 0
  then error "inv0_prop fail"
  else True
