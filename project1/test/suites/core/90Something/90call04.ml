(*!tests!
*
* { "output": ["19"] }
*
*)
let rec f x = x + 1
and g x y = x + y ;;
g (f 7) (f (f 9)) ;;
