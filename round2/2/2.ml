
open Printf
open Scanf
open ExtLib

let count a =
  let c = ref 0 in
  for i = 0 to Array.length a - 1 do
    let (ai,bi) = a.(i) in
    Array.iter (fun (a,b) -> if compare a ai <> compare b bi then incr c) a
  done;
  !c / 2

let () =
  sscanf (input_line stdin) "%u" begin fun t ->
    for i = 1 to t do
      sscanf (input_line stdin) "%u" begin fun n ->
        let a = Array.init n (fun _ ->
          sscanf (input_line stdin) "%u %u" begin fun a b -> a,b end) in
        printf "Case #%d: %d\n" i (count a)
      end
    done
  end
