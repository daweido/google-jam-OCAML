let read_int () = Scanf.scanf " %d" (fun x -> x)

let highest = 1000*1000*100

exception Impossible

let peaks isee = 
  let n = Array.length isee +1 in
  let seenby = Array.make n [] in
  let pos = Array.make n (-1) in
  let last_who_sees = Array.make n (-1) in
  let gen = Array.make n 0 in
  Array.iteri (fun i s -> seenby.(s) <-  i::seenby.(s)) isee;
  pos.(n-1) <- highest;
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
  pos
    

let _ =
  let ncases = read_int () in
  for i = 1 to ncases do
    let npeaks = read_int () in
    let isee = Array.init (npeaks-1) (fun _ -> read_int () - 1) in
    Printf.printf "Case #%d: " i;
    try
      let heights = peaks isee in
      Array.iter (fun h -> Printf.printf "%d " h) heights;
      Printf.printf "\n"
    with _ -> Printf.printf "Impossible\n"
  done
    








