open Stemmer

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
