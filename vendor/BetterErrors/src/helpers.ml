open OcamlRe

(* Batteries library substitutes *)
let listDrop n lst =
  let lst = ref lst in
  for i = 1 to n do lst := List.tl !lst done;
  !lst

let listDropWhile f lst =
  let lst = ref lst in
  while f (List.hd !lst) do lst := List.tl !lst done;
  !lst

let listTake n lst =
  let result = ref [] in
  let lst = ref lst in
  for i = 1 to n do
    result := (List.hd !lst) :: !result;
    lst := (List.tl !lst)
  done;
  List.rev !result

let listTakeWhile f lst =
  let result = ref [] in
  let lst = ref lst in
  while f (List.hd !lst) do
    result := (List.hd !lst) :: !result;
    lst := List.tl !lst
  done;
  List.rev !result

let optionGet a =
  match a with
  | Some n -> n
  | None -> raise (Invalid_argument "optionGet")

let optionMap f a =
  match a with
  | Some a' -> Some (f a')
  | None -> None

let listFilterMap f lst =
  List.map f lst
  |> List.filter (function | Some a -> true | None -> false)
  |> List.map optionGet

let listFindMap f lst =
  lst
  |> List.find (fun a -> match f a with | Some x -> true | None -> false)
  |> f
  |> optionGet

let stringSlice ?(first=0) ?last str =
  let last = match last with
  | Some l -> min l (String.length str)
  | None -> String.length str
  in
  if last <= first then ""
  else String.sub str first (last - first)

let stringFind str part =
  let rec find' str part idx =
    if String.length str < String.length part then raise Not_found
    else if stringSlice str ~last:(String.length part) = part then idx
    else find' (stringSlice str ~first:1) part (idx + 1)
  in
    find' str part 0

let stringNsplit str ~by =
  if String.length str = 0 then
    raise (Invalid_argument "stringNSplit: empty str not allowed")
  else if str = "" then
    []
  else
    let rec split' str ~by accum curr =
      let lengthBy = String.length by in
      let lengthStr = String.length str in
      if lengthStr < lengthBy then
        (curr ^ str) :: accum
      else
        if (String.sub str 0 lengthBy) = by then
          split'
            (String.sub str lengthBy (lengthStr - lengthBy))
            ~by
            (curr :: accum)
            ""
        else
          split'
            (String.sub str 1 (lengthStr - 1))
            ~by
            accum
            (curr ^ (String.sub str 0 1))
    in (split' str ~by [] "") |> List.rev

let stringSplit str ~by =
  if by = "" then
    ("", str)
  else if str = "" then
    raise Not_found
  else
    match stringNsplit str ~by with
    | [] | [_] -> raise Not_found
    | x :: xs -> (x, String.concat by xs)

let linesOfChannelExn chan =
  let lines = ref [] in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let fileLinesOfExn filePath = linesOfChannelExn (open_in filePath)


(* ============ *)

let get_match_n n pat str =
  let rex = Re_pcre.regexp pat in
  Re_pcre.get_substring (Re_pcre.exec ~rex str) n
(* get the first (presumably only) match in a string *)
let get_match = get_match_n 1

let get_match_maybe pat str =
  let rex = Re_pcre.regexp pat in
  try Some (Re_pcre.get_substring (Re_pcre.exec ~rex str) 1)
  with Not_found -> None

let get_match_n_maybe n pat str =
  let rex = Re_pcre.regexp pat in
  try Some (Re_pcre.get_substring (Re_pcre.exec ~rex str) n)
  with _ -> None

let execMaybe pat str =
  let rex = Re_pcre.regexp pat in
  try Some (Re_pcre.exec ~rex str)
  with Not_found -> None

let getSubstringMaybe result n =
  try Some (Re_pcre.get_substring result n)
  with Not_found -> None

let split sep str =
  let rex = Re_pcre.regexp sep in
  Re_pcre.split ~rex str

let rec splitInto ~chunckSize (l: 'a list): 'a list list =
  if List.length l <= chunckSize || chunckSize = 0 then [l]
  else (listTake chunckSize l) :: (splitInto ~chunckSize (listDrop chunckSize l))

let resetANSI = "\027[0m"
let red s = "\027[31m" ^ s ^ resetANSI
let redUnderlined s = "\027[31;4m" ^ s ^ resetANSI
let yellow s = "\027[33m" ^ s ^ resetANSI
let yellowUnderlined s = "\027[33;4m" ^ s ^ resetANSI
let green s = "\027[32m" ^ s ^ resetANSI
let cyan s = "\027[36m" ^ s ^ resetANSI

let mapcat sep f l = String.concat sep (List.map f l)

let sp = Printf.sprintf

let highlight ?(color=red) ?(first=0) ?(last=99999) str =
  (stringSlice ~last:first str)
    ^ (color @@ stringSlice ~first ~last str)
    ^ (stringSlice ~first:last str)
