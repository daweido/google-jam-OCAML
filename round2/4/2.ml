(*
 * nom : jam
 * pre-conditions : fin,fout  2 noms de fichiers
 * pre-conditions : fonction algolec (unit_channel -> `a),
 *                  qui lit le contenu d'un cas de fin,
 * pre-conditions : fonction algopp (`a -> string),
 *                  qui renvoie la solution du cas passé en paramêtre
 * post-conditions : retourne le type unit (rien) après avoir
 *                   écrit le résultat de l'exécution de l'algopb sur tous
 *                   les cas de fin dans fout
 * auteur : EISTI
 * date création : 07/10/15
 *)

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
	let b = input_line f in
	let c = input_line f in
	(b,c);;

#load "str.cma";;

let extract_param (b,c) =
	let ssli s = Str.split (Str.regexp " ") s in
	let strToInt l = List.map int_of_string l in
	let rec listeCoupList l = match l with
		[] -> []
		|x::y::tl -> (x,y)::(listeCoupList tl)
		|_ -> failwith "Erreur" in
	(listeCoupList (strToInt (ssli b)),listeCoupList (strToInt (ssli c)));;


let assemble l1 l2 =
	let rec ar acc l1 l2 = match l1, l2 with
		| [], _ | _, [] -> acc
		| (v1, t1) :: tl1, (v2, t2) :: tl2 when t1 = t2 ->
	(* Si t1 = t2, on ajoute à l'accumulateur le nombre maximum
	d'assemblages qu'on peut faire, donc le minimum entre v1 et v2
	Par contre, attention, il faut qu'on garde le reste parce qu'on
	peut toujours s'en resservir.*)
			let acc, l1, l2 = if v1 = v2 then acc + v1, tl1, tl2
				else if v1 > v2 then acc + v2, (v1 - v2, t1) :: tl1, tl2
				else acc + v1, tl1, (v2 - v1, t2) :: tl2 in
			ar acc l1 l2
		| (v1, t1) :: tl1, (v2, t2) :: tl2 ->
	    (* Dans ce cas, on doit tester, soit on jette les boîtes
	    soit on jette les jouets et on regarde ce qui nous renvoie le
	    résultat maximal *)
	    (* On jette le couple (v1, t1) *)
	    let r1 = ar acc tl1 l2 in
	    (* On jette le couple (v2, t2) *)
	    let r2 = ar acc l1 tl2 in
	    max r1 r2 in
		ar 0 l1 l2;;

let jamC (b,c) =
	let (l1,l2) = extract_param (b,c) in
	string_of_int (assemble l1 l2);;


(* Exemple d'utilisation *)
(* jam "C-large-practice.in" "toto.out" lecture jamC;;*)
