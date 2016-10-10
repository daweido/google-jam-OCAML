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

let jam1 i =
	let lecNbBytes = read_int () in
	let lecIO = read_line () in
(*Ajout du test si le string est tout en uppercase*)
	let testIO lecIO =
		let l = String.length lecIO in
		let noIO c = if (c <> 'O') || (c <> 'I') then failwith "String entered incorrect. Please check that it is exclusively made of Is and Os." in
		if (l*8 <> lecNbBytes) then failwith "String entered incorrect. Please check that it is the correct length."
		String.iter noIO lecIO
	in

	if (i < 1) || (i >100) then failwith "T Number of test cases value incorrect. (1<=T<=100)"
	if (lecNbBytes < 1) || (lecNbBytes > 1000) then failwith "B Number of Bytes in the string value incorrect. (1<=B<=1000)"
	testIO lecIO
	let ioto10 c = if(c='O') then '0' else '1' in
	let exclu10 = String.map ioto10 lecIO in
	exclu10;;


(* Exemple d'utilisation *)
(* jam "A-small-practice.in" "toto.out" input_line (function x -> x);;*)
