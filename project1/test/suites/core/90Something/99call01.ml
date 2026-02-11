(*!tests!
*
* { "output": ["14"] }
*
*)
let rec f x y = x + y
and g x = f x x 
and h x = g x + 1 
and i x = h x * 2 ;;

i 3 ;;