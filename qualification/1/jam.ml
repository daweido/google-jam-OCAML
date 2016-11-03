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

let jam1 (a,b) =
	let ioto10 c = if (c = 'O') then '0' else '1' in
	let exclu10 s = String.map ioto10 s in
	let l = String.length (exclu10 b) in
	let rec decoupe s =
		if s = "" then []
		else (String.sub s 0 8)::decoupe(String.sub s 8 ((String.length s) -8))
	in
	let hohib s = "0b"^s in
	let hohi l = List.map hohib l in
	let mappe l = List.map int_of_string l in
	let renduchar i = Char.chr i in
	let mapChar l = List.map renduchar l in
	let ajoutChar cl = String.concat "" (List.map (String.make 1) cl) in

	ajoutChar (mapChar (mappe (hohi (decoupe (exclu10 b)))))
	;;


(* Exemple d'utilisation *)
(* jam "A-small-practice.in" "toto.out" lecture jam1;;*)
