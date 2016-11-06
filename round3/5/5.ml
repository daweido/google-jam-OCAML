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

let rec supprime l a = match l with
	[]-> []
	|x::r when x = a -> List.tl l
	|x::r -> supprime r a;;

let rec ajoutefin a l = match l with
	[] -> [a]
	|x::r -> x::ajoutefin a r;;

let rec jamLoop n fich i  lb la va =
	let indiceEgal s = String.index s '=' in
	let indice1Par s = String.index s '(' in
	let ssv s j = String.sub s j+1 ((String.length s)-j-2) in (*Sortie du string des variables*)
	let ssn s i = String.sub s 0 i in (*Sortie du nom de la fonction en string*)
	let listVar s = if String.contains s ',' then Str.split (Str.regexp ",") s else Str.split (Str.regexp "") s i (*Si les variables comportent des virgules, les separer avec la virgule comme séparateur sinon juste mettre le string sous forme d'un element de la liste*)
	let lecVarDir s fb = match s with
		s when String.sub s (String.length s - 3) 3 = "=()" -> ajoutefin (String.sub s 0 (String.length s - 3)) fb (*Direct mettre dans FB si aucune variable*)
		|_ -> s in
	let ligne = input_line fich in

	let rec verifVar fb l fa va s = match l with
		[] -> ajoutefin s fb
		|x::r when List.mem x fb -> verifVar fb l fa va s (*Si variable dans la liste fb faire appelle reecursif et verifie autres elements de la liste*)
		|_ -> let rec ajoutDiffLis fa va l s = match l with (*Ajout dans les differentes listes*)
			x::r when List.mem x va -> ajoutDiffLis fa va r s (*Vérifie si une variable est dans la liste des fb si oui il fait le test avec touts les autres variables*)
			|x::r -> ajoutefin x va;ajoutefin () (*C'est ici qu'il faut vérifier si les variables sont deja dans la liste VA si oui on passe à la prochaine sinon on ajoute dans la liste et à la fin il faut ajouter le couple (fonct,[vars]) dans la liste FA*)


	if i = n then
		if la = [] then "GOOD" else "BAD"
	(*remplacer lb et la par les fonctions d'olivier, les appeller*)
	else jamLoop n fich (i+1) lb la;;

let jamEval a =
	let fb = [] in
	let fa = [] in
	let va = [] in
	let fich = open_in "C-small-practice.in" in
	let n = int_of_string (input_line fich) in
	jamLoop n fich 0 fb fa va;;




(* Exemple d'utilisation *)
(* jam "C-small-practice.in" "toto.out" lecture jamMountain;;*)
