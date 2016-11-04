#load "str.cma" ;;

let separe_egal s = Str.split (Str.regexp "=") s	;;		(*On prend ce qu'il y a avant la virgule pour voir si il est définit*)

let separe_virg s = Str.split (Str.regexp ",") s;;

let separe_par s = Str.split (Str.regexp "(") s ;;

let separe s = Str.split (Str.regexp "") s;;

let l = ['z';'a';'(';')';];;
let b = ['a';'b';'c'];;

let lecDavid l = match l with
	[] -> failwith "Erreur"
	|x::f when f = ["=";"(";")"] -> String.concat "" x
	| _ -> String.concat "" l;;

let rec lec l = match l with
	[] -> false
	|x::r when (x ="(") && ((List.hd r )= ")") -> true
	|x::r -> lec r;;

let rec ajoutefin a l = match l with
	[] -> [a]
	|x::r -> x::ajoutefin a r;;

let cre_LB l listegood listeatt = if lec (separe l) = true then ajoutefin (List.hd ( separe_egal l)) listegood else ajoutefin (List.hd (List.rev ( separe_par l))) listeatt;;		(*Liste des variables définis*)

let rec supprime l a = match l with
	[]-> []
	|x::r when x = a -> List.tl l
	|x::r -> supprime r a;;

let lecture_bis listegood listeatt ligne = match ligne with
	[] -> []
	x::r when let rec ba listegood x = match x with
		[] -> false
		|a::r when a = x -> true
		|a::r -> ba r x in
		ba = true -> supprime x listeatt;;




(*let cherche_virg ligne = *)
