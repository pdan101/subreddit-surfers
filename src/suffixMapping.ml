(*Once Lauren is done, factor out all of this code into another module
  and open it in both suffixMapping and wordProcessor so that they can
  both access them.*)
let tail word = String.sub word 1 (String.length word - 1)

let vowels = "aeiouy"

let is_vowel character =
  let lowercase_char = Char.lowercase_ascii character in
  String.contains vowels lowercase_char

let rec create_units (word : string) =
  let length = String.length word in
  if length > 0 then
    if is_vowel (String.get word 0) then "V" ^ create_units (tail word)
    else "C" ^ create_units (tail word)
  else ""

let rec calc_vc char_string =
  let first_char =
    try String.get char_string 0 with
    | _ -> '-'
  in
  let second_char =
    try String.get char_string 1 with
    | _ -> '-'
  in
  if first_char = 'V' && second_char = 'C' then
    1 + calc_vc (tail char_string)
  else if first_char = '-' || second_char = '-' then 0
  else calc_vc (tail char_string)

(*Down to here*)
let step2_3data = Yojson.Basic.from_file "src/step2_3.json"

let step4data = Yojson.Basic.from_file "src/step4.json"

let json_to_assoc_list data =
  data |> Yojson.Basic.Util.to_assoc
  |> List.map (fun (x, y) -> (x, y |> Yojson.Basic.Util.to_string))

let hashtbl_step2_3 = Hashtbl.create 27

let hashtbl_step4 = Hashtbl.create 19

let rec add_pairs_to_table table (lst : (string * string) list) =
  match lst with
  | [] -> ()
  | (x, y) :: tail_lst ->
      Hashtbl.add table x y;
      add_pairs_to_table table tail_lst

let assoc_list_to_hashtbl tbl (data : (string * string) list) =
  add_pairs_to_table tbl data

let build_tables =
  step2_3data |> json_to_assoc_list
  |> assoc_list_to_hashtbl hashtbl_step2_3;
  step4data |> json_to_assoc_list |> assoc_list_to_hashtbl hashtbl_step4

let rec find_suffix_binding tbl word remainder m =
  if String.length word < 2 then ("", 0)
  else
    match Hashtbl.find_opt tbl (word |> String.uppercase_ascii) with
    | Some x when remainder |> create_units |> calc_vc > m ->
        (x |> String.lowercase_ascii, String.length word)
    | _ ->
        find_suffix_binding tbl
          (String.sub word 1 (String.length word - 1))
          (remainder ^ String.sub word 0 1)
          m
