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
		output_string fecr (jam_rec flec algolec algopb  nb nb);
		close_in flec;
		close_out fecr;
	end;;
#load "str.cma";;

let lecture f =
	let a = input_line f in
	a;;

let rec jamLoop n fich i  lb la =
	let lecDavid s = match s with
		s when String.sub s (String.length s - 3) 3 = "=()" -> String.sub s 0 (String.length s - 3)
		|_ -> s in
	let ligne = input_line fich in
	if i = n then
		if la = [] then "GOOD" else "BAD"
	(*remplacer lb et la par les fonctions d'olivier, les appeller*)
	else jamLoop n fich (i+1) lb la;;

let jamEval a =
	let lb = [] in
	let la = [] in
	let fich = open_in "C-small-practice.in" in
	let n = int_of_string (input_line fich) in
	jamLoop n fich 0 lb la;;




(* Exemple d'utilisation *)
(* jam "C-small-practice.in" "toto.out" lecture jamMountain;;*)
