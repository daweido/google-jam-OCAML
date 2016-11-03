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
#load "str.cma";;

let lecture f =
	let a = input_line f in
	let b = input_line f in
	let extract_param b =
		let ssli s = Str.split (Str.regexp " ") s in
		let strToInt l = List.map int_of_string l in
		let min1 l = List.map (fun x -> x-1) l in
		min1 (strToInt (ssli b)) in
	extract_param b;;

let plushaut = 100*1000*1000

exception Impossible

let peaks isee =
  let n = Array.length isee +1 in
  let seenby = Array.make n [] in
  let pos = Array.make n (-1) in
  let last_who_sees = Array.make n (-1) in
  let gen = Array.make n 0 in
  Array.iteri (fun i s -> seenby.(s) <-  i::seenby.(s)) isee;
  pos.(n-1) <- plushaut;
  for i = n-1 downto 0 do
    (* On place tous ceux qui me voient *)
    if i <> n-1 then
      last_who_sees.(isee.(i)) <- i;
    if i <> n-1 && isee.(i) <> n-1 && last_who_sees.(isee.(isee.(i))) <> isee.(i)
    then raise Impossible;
    List.iter
      (fun j ->
        pos.(j) <- pos.(i) + gen.(i) * (j - i) - 1;
        gen.(j) <- gen.(i) + 1
    ) seenby.(i)
  done;
  Array.iteri (fun i s ->
    for j = i+1 to n-1 do
      let k = isee.(i) in
      if j <> isee.(i) && (pos.(k) - pos.(i))*(j-i) < (pos.(j) - pos.(i))*(k-i)
      then raise Impossible
    done
  ) isee;
  pos;;


let jamMountain b =
    let isee = Array.of_list b in
    (*let heights = peaks isee in
    String.concat " " (List.map string_of_int (Array.to_list heights))*)
    try
      let heights = peaks isee in
      String.concat " " (List.map string_of_int (Array.to_list heights))
    with _ -> "Impossible"
;;

(* Exemple d'utilisation *)
(* jam "C-small-practice.in" "toto.out" lecture jamMountain;;*)
