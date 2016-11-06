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

let ssli s = Str.split (Str.regexp " ") s;;

let lecture f =
	let a = input_line f in
	let sortiParam s =
		let strToInt l = List.map int_of_string l in
		let sortiCouple l = match l with
			|[x;y] -> (x,y)
			|_ -> failwith "Erreur" in
	sortiCouple (strToInt (ssli s)) in
	let mat = Array.make_matrix 30 4 0.0 in
	let jamLoop q tab2 f =
		let strToFloat l = List.map float_of_string l in
		let sortieTab4 l = Array.of_list l in
		for i=0 to (q-1) do
			for j=0 to 3 do tab2.(j).(i) <- (sortieTab4 (strToFloat (ssli (input_line f)))).(j) done
		done;
		tab2 in
	((sortiParam a),jamLoop (snd (sortiParam a)) mat f);;

(*let rec jamLoop q tab2 i =
	let fich = open_in "C-small-practice.in" in
	let n = input_line fich in
	let strToFloat l = List.map float_of_string l in
	let sortieTab4 l = Array.of_list l in
	if i = q then tab2
	else begin
		for j=0 to 3 do tab2.(j).(q) <- (sortieTab4 (strToFloat (ssli n))).(j) done;
		jamLoop q tab2 (i+1)
	end;;*)

let rec sort = function
	| [] -> []
	| x :: l -> insert x (sort l)

and insert elem = function
	| [] -> [elem]
	| x :: l ->
		if elem < x then elem :: x :: l else x :: insert elem l;;

let calcProba prob proba q m =
	let chngTab i k j = proba.(i+(k+1)*m) <- proba.(i) *. prob.(k).(j) in
	let ssArray = Array.sub proba m (m*5) in
	for j=0 to (q-1) do begin
		for i=0 to (m-1) do
			for k=0 to 3 do chngTab i k j done
		done;
		for l=0 to ((m*5)-1) do proba.(l+m) <- (Array.of_list (sort (Array.to_list ssArray))).(l) done;
		for i=0 to (m-1) do proba.(i) <- proba.(i+4*m) done
	end
	done;
	proba;;

let creaProba =
	let pro = Array.make 50000 (0.0) in
	pro.(0) <- 1.0;
	pro;;

let jamProba ((m,q),prob) =
	let proba = creaProba in
	let probs = Array.sub (calcProba prob proba q m) 0 (m-1) in
	string_of_float (Array.fold_left (+.) 0.0 probs);;

(* Exemple d'utilisation *)
(* jam "C-small-practice.in" "toto.out" lecture jamProba;;*)
