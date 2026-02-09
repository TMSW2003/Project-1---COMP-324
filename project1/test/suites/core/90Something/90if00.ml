(*!tests!
 *
 * { "output" : ["0"] }
 *
 *)

let x = if true then 0 else 1 ;;
let rec f x = if x = 0 then 0 else f (x - 1) ;;
 
