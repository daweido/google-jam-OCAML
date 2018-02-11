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

#load "str.cma" ;;

let separe s = List.map int_of_string(Str.split (Str.regexp " ") s);;

let lisse d b = if b > d then d else b;; (* si le nombre d'oeufs cassés est supérieur au nbre de lancés cela équivaut a d = b*)

let  listesauv = ref [];;

let sauvegarde d b =
	let rec save d b l  = match l with
                             []-> None
                             |x::y::z::r when x=d && y=b-> Some z
                             |x::y::z::r -> save d b r in
                             save d b !listesauv;;
let rec etage d b =
	if (d >= 32) && (b >=32) then -1 else
	if b = 1 then d else
	if d = 1 then 1 else

                    let x1 = match sauvegarde (d-1) (b-1)    with
                                None -> etage(d-1) (b-1)
                              | Some z -> z in
                    let x2= match sauvegarde (d-1) b  with
                              None -> etage(d-1) b
                              |Some z -> z in
                     listesauv :=d::b::(x1+x2+1)::!listesauv;
                     (x1+x2+1);;


let rec drop f b x = if (etage x b )>= f then x else drop f b (x+1);;

let rec break f d x = if (etage d x ) >= f then x else break f d (x+1);;

let final f d b = string_of_int(etage d b)^" "^string_of_int(drop f b f)^" "^string_of_int(break f d f);;

let fin x = match x with
	f::d::b::[] when (etage d (lisse d b)) > 4294967296 -> "-1 "^string_of_int(drop f b 1)^" "^string_of_int(break f d 1)
	|f::d::b::[]-> string_of_int(etage d (lisse d b))^" "^string_of_int(drop f b 1)^" "^string_of_int(break f d 1)
	|_ -> failwith "Erreur";;

let jamEgg x = fin  (separe x);;


(* Exemple d'utilisation *)
(* jam "C-small-practice.in" "toto.out" lecture jamEgg;;*)
