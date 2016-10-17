let _ =
  let sb = Scanf.Scanning.stdib in
  let m = 1000000 in
  let l = Array.make (m + 1) 0 in
  let r = Array.make (m + 1) m in
  let _ =
    l.(1) <- 1;
    l.(2) <- 2;
    l.(3) <- 2;
    for i = 4 to m do
      let xl = ref ((i + 1) / 2)
      and xr = ref (i - 1) in
	while !xl < !xr - 1 do
	  let m = (!xl + !xr) / 2 in
	    if l.(m) > i - m
	    then xr := m
	    else xl := m
	done;
	l.(i) <- !xr
    done;
    for i = 1 to m do
      r.(l.(i)) <- i
    done
  in
  let count x y =
    if x < l.(y)
    then x
    else if x <= r.(y)
    then l.(y) - 1
    else x - (r.(y) - l.(y) + 1)
  in
  let cases = Scanf.bscanf sb "%d " (fun s -> s) in
    for ca = 1 to cases do
      let a1 = Scanf.bscanf sb "%d " (fun s -> s) in
      let a2 = Scanf.bscanf sb "%d " (fun s -> s) in
      let b1 = Scanf.bscanf sb "%d " (fun s -> s) in
      let b2 = Scanf.bscanf sb "%d " (fun s -> s) in
      let res = ref 0 in
      let _ =
	for i = a1 to a2 do
	  res := !res + count b2 i - count (b1 - 1) i
	done;
      in
	Printf.printf "Case #%d: %d\n" ca !res;
    done
