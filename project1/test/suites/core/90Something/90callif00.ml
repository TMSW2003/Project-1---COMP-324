(*!tests!
 *
 * { "output" : ["0"] }
 *
 *)

let rec f x = if x = 0 then 0 else f (x - 1) ;;
f 10 ;;
 
