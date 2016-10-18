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
	(a,b);;

#load "str.cma";;

let jam2 (a,b) =
	let ssli s = Str.split (Str.regexp " ") s in
	let rec supp l n =
		match l with
			[] -> []
			|x::r -> if x = n then r
					else x::(supp r n) in
	let ssliInt l = List.map int_of_string l in
	let rec ajouteFin l a = match l with
		[] -> [a]
		|x::r -> x::ajouteFin r a in
	let rec recupVal l = match l with
		[] -> []
		|x::r -> x::recupVal (supp r ((x*4)/3)) in
	let sortieString l = String.concat " " (List.map string_of_int l) in
	sortieString (recupVal (ssliInt (ssli b)));;




(* Exemple d'utilisation *)
(* jam "A-large-practice.in" "toto.out" lecture jam2;;*)
