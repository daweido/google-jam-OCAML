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
	a;;
#load "str.cma";;
let jam3 a =
	let ssli s = Str.split (Str.regexp " ") s in
	let sortieParam l = match l with
		[x;y] -> ((float (int_of_string x)),(float (int_of_string y)))
		|_ -> failwith "Erreur" in
	let g = 9.8 in
	let pi = 4. *. atan 1. in
	let c3 (v,d) g = (g*.d)/.(v*.v) in
	let teta (v,d) g = (asin ((c3 (v,d) g)))/. 2.0 in
	let calcul (v,d) g pi = (teta (v,d) g) *. 180. /. (pi) in
	string_of_float ((calcul (sortieParam (ssli a))) g pi);;




(* Exemple d'utilisation *)
(* jam "B-small-practice.in" "toto.out" lecture jam3;;*)
