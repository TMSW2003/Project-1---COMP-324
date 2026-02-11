(*!tests!
 *
 * { "output" : ["10"] }
 *
 *)

let rec f x = x * x and g x = f x + 1 ;;
let y = 3 in
if y < 4 then g y else 0 ;;
