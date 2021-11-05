open Stemmer
open SuffixMapping

(*Making sure this gets copied*)
type stemmed_word = {
  original_word : string;
  units : string;
  num_vcs : int;
  stemmed : string;
}

type vocabulary = stemmed_word list

type text_block = {
  original_text : string;
  stemmed_text : string;
}

let rec remove_punc s =
  if String.length s = 0 then s
  else
    let current_char = s.[0] in
    let is_char =
      (current_char |> Char.uppercase_ascii |> Char.code)
      - ('a' |> Char.uppercase_ascii |> Char.code)
    in

    if is_char >= 0 && is_char <= 25 then
      String.make 1 current_char
      ^ (String.sub s 1 (String.length s - 1) |> remove_punc)
    else String.sub s 1 (String.length s - 1) |> remove_punc

let parse (text : string) =
  String.split_on_char ' ' text
  |> List.filter (fun x -> String.length x > 0)
  |> List.map (fun x -> String.trim x)
  |> List.map (fun x -> remove_punc x)

let replace_suffix word =
  let complete23, len23 =
    if calc_vc word > 0 then
      let replacement = find_suffix_binding hashtbl_step2_3 word "" 0 in
      let new_string =
        String.sub word 0 (String.length word - snd replacement)
      in
      (new_string ^ fst replacement, snd replacement)
    else (word, 0)
  in
  let complete4, len4 =
    if calc_vc word > 1 then
      let replacement2 = find_suffix_binding hashtbl_step4 word "" 1 in
      let new_string2 =
        String.sub word 0 (String.length word - snd replacement2)
      in
      (new_string2 ^ fst replacement2, snd replacement2)
    else (word, 0)
  in
  if len23 > len4 then complete23 else complete4

let stemmer (word : string) =
  let units = create_simplified_units (create_units word) "" in
  let vcs = calc_vc word in

  let step_1a = remove_plurals word in
  let step_1b = remove_past_participles step_1a in

  let apply_final = apply_finalize step_1a in
  let final_step1ab =
    if apply_final then finalize_plurals_past_participles step_1b
    else step_1b
  in
  let step_1c = fix_y final_step1ab in
  let step_234 = replace_suffix step_1c in
  let step_5a = remove_e step_234 in
  let stemmed = check_double_consonant step_5a in
  { original_word = word; units; num_vcs = vcs; stemmed }

exception Unsupported_sentence_format

let rec split_sentence_list (sep : Str.split_result list) : string list
    =
  match sep with
  | [] -> []
  | [ Text x ] -> [ x ]
  | Text sentence :: Delim punc :: t ->
      (String.trim sentence ^ punc) :: split_sentence_list t
  | Delim x :: t -> raise Unsupported_sentence_format
  | Text _ :: Text _ :: t -> raise Unsupported_sentence_format

let parse_sentence (text : string) =
  match
    try
      split_sentence_list (Str.full_split (Str.regexp "[.!?]") text)
    with
    | Unsupported_sentence_format -> [ text ]
  with
  | x -> x

let stem_word_list (words : string list) =
  List.map (fun x -> stemmer x) words

let extract_stemmed (stemmed_words : stemmed_word list) =
  List.map (fun x -> x.stemmed) stemmed_words

let make_sentence (delim : string) (sep_sentence : string list) =
  let sentence =
    List.fold_right (fun acc w -> acc ^ " " ^ w) sep_sentence ""
  in
  String.sub sentence 0 (String.length sentence - 1) ^ delim

let process_sentence (sentence : string) =
  let sentence_delimiter = sentence.[String.length sentence - 1] in
  sentence |> String.trim |> parse |> stem_word_list |> extract_stemmed
  |> make_sentence (String.make 1 sentence_delimiter)

let parse (text : string) =
  String.split_on_char ' ' text
  |> List.filter (fun x -> String.length x > 0)
  |> List.map (fun x -> String.trim x)
  |> List.map (fun x -> remove_punc x)

(**let word = Yojson.Basic.from_file "src/word.json"

   let json_to_assoc_list data = data |> Yojson.Basic.Util.to_assoc |>
   List.map (fun (x, y) -> (x, y |> Yojson.Basic.Util.to_string))

   let word_data = word |> json_to_assoc_list

   let remove_stop_words word_data = List.filter_map (fun x -> match x
   with | word_data -> None | _ -> Some x) *)

let replace_suffix word =
  let complete23, len23 =
    if calc_vc word > 0 then
      let replacement = find_suffix_binding hashtbl_step2_3 word "" 0 in
      let new_string =
        String.sub word 0 (String.length word - snd replacement)
      in
      (new_string ^ fst replacement, snd replacement)
    else (word, 0)
  in
  let complete4, len4 =
    if calc_vc word > 1 then
      let replacement2 = find_suffix_binding hashtbl_step4 word "" 1 in
      let new_string2 =
        String.sub word 0 (String.length word - snd replacement2)
      in
      (new_string2 ^ fst replacement2, snd replacement2)
    else (word, 0)
  in
  if len23 > len4 then complete23 else complete4

let make_paragraph (sentences : string list) =
  List.fold_right (fun acc w -> acc ^ " " ^ w) sentences ""

let stem_paragraph (paragraph : string) =
  paragraph |> parse_sentence
  |> List.map process_sentence
  |> make_paragraph

let make_text_block (text : string) =
  { original_text = text; stemmed_text = stem_paragraph text }

let stemmed_text_block block = block.stemmed_text
