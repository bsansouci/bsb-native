
module PairComparator = Belt_Id.MakeComparable(struct
  type t = int * int
  let cmp (a0, a1) (b0, b1) =
    match Pervasives.compare a0 b0 with
    | 0 -> Pervasives.compare a1 b1
    | c -> c
end)

let myMap = Belt_Map.make ~id:(module PairComparator)
let myMap2 = Belt_Map.set myMap (1, 2) "myValue"

let _ = match Belt_Map.get myMap2 (1, 2) with 
| None -> print_endline "FUCK"
| Some n -> print_endline n

(* type coolRecord = {
  mutable bla: string
}[@@bs.deriving abstract]

let _ = 
  let a = Belt_Array.makeUninitializedUnsafeBen 10 in
  for i = 0 to 9 do
    Belt_Array.setUnsafe a i (coolRecord ~bla:"fuck")
  done;
  let _ = Belt_Array.map a (fun v -> 
      blaSet v ((v |. bla) ^ " you");
      v
    ) in
  ignore @@  Belt_Array.map a (fun v -> print_endline (v |. bla))
 *)
