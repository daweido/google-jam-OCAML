
let rec jam_rec flec algolec algopb nblignes total = match nblignes with
 0 -> ""
|n -> let line = (algolec flec) in
	"Case #"^(string_of_int(total-nblignes+1))^": "^(algopb line)^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) ;;

let jam fin fout algolec algopb =
	let fecr = open_out fout in
	let flec = open_in fin in
	let nb = (int_of_string(input_line flec)) in
	begin
		output_string fecr (jam_rec flec algolec algopb nb nb);
		close_in flec;
		close_out fecr;
	end;;

let lecture f =
	let a = input_line f in
	a;;

#load "str.cma";;

let jam1 s =
	let sortieParam s =
		let ssli s = Str.split (Str.regexp " ") s in
		let strToInt l = List.map int_of_string l in
		let l = strToInt (ssli s) in
		match l with
			[a1;a2;b1;b2] -> (a1,a2,b1,b2)
			|_ -> failwith "Erreur création de couple"
	in
	let n = 1000000 in
	let d = Array.make (n + 1) 0 in
	let l = Array.make (n + 1) n in
	let _ =
		d.(1) <- 1;
		d.(2) <- 2;
		d.(3) <- 2;
		for i = 4 to n do
		let xd = ref ((i + 1) / 2)
		and xl = ref (i - 1) in
   while !xd < !xl - 1 do
	 let n = (!xd + !xl) / 2 in
	   if d.(n) > i - n
	   then xl := n
	   else xd := n
   done;
   d.(i) <- !xl
	  done;
	  for i = 1 to n do
		l.(d.(i)) <- i
	  done
	in
	let compt x y =
	  if x < d.(y)
	  then x
	  else if x <= l.(y)
	  then d.(y) - 1
	  else x - (l.(y) - d.(y) + 1)
	in
	let possi = ref 0 in
	let resu (a1,a2,b1,b2) =
	 	for i = a1 to a2 do
			possi := !possi + compt b2 i - compt (b1 - 1) i
		done;
		!possi
	in
	string_of_int(resu (sortieParam s));;

(* Exemple d'utilisation *)
(* jam "C-large-practice.in" "toto.out" lecture jam1;;*)
