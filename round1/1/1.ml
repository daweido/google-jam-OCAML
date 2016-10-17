
let stdib = Scanf.Scanning.stdib    
let nb_case = Scanf.bscanf stdib "%d\n" (fun x -> x)

let rec insert x = function
  | a :: s when a < x -> a :: (insert x s)
  | a -> x :: a

let rec rev_insert x = function
  | a :: s when a > x -> a :: (rev_insert x s)
  | a -> x :: a

let input_vectors () =
  let nb_n = Scanf.bscanf stdib "%d\n" (fun x -> x) in
  let rec aux1 accu n = 
    if n = 0 then accu
    else Scanf.bscanf stdib "%d " (fun x -> aux1 (insert x accu) (n-1)) 
  in 
  let rec aux2 accu n = 
    if n = 0 then accu
    else Scanf.bscanf stdib "%d " (fun x -> aux2 (rev_insert x accu) (n-1)) 
  in
    (aux1 [] nb_n, aux2 [] nb_n)

let rec scalar accu = 
  function
  | ([],[]) -> accu
  | (a :: s, b ::t) -> scalar (a * b + accu) (s,t)

let main () = 
  let rec aux i =
    if i = nb_case then ()
    else
    (
      Printf.printf "Case #%d: %d\n" (i+1) (scalar 0 (input_vectors()));
      aux (i+1)
    )
  in
    aux 0

    
let _ = main ()
