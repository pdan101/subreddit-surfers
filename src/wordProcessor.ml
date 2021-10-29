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

let consonants = "bcdfghjklmnpqrstwvxz"

let vowels = "aeiouy"

let tail word = String.sub word 1 (String.length word - 1)

let is_vowel character =
  let lowercase_char = Char.lowercase_ascii character in
  String.contains vowels lowercase_char

let get_last word num = String.sub word (String.length word - num) num

let remove_last word num = String.sub word 0 (String.length word - num)

let rec create_units (word : string) =
  let length = String.length word in
  if length > 0 then
    if is_vowel (String.get word 0) then "V" ^ create_units (tail word)
    else "C" ^ create_units (tail word)
  else ""

let rec create_simplified_units raw_units acc =
  if String.length raw_units > 0 then
    let first_character = Char.escaped (String.get raw_units 0) in
    if String.length acc > 0 && get_last acc 1 = first_character then
      create_simplified_units (tail raw_units) acc
    else create_simplified_units (tail raw_units) (acc ^ first_character)
  else acc

(*TODO make this method better- should not be using try/catch*)
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

let remove_plurals word =
  let len = String.length word in
  if len >= 4 && get_last word 4 = "sses" then remove_last word 2
  else if len >= 3 && get_last word 3 = "ies" then remove_last word 2
  else if len >= 2 && get_last word 2 = "ss" then word
  else if len >= 2 && get_last word 1 = "s" then remove_last word 1
  else word

let rec contains_vowel word =
  if String.length word = 0 then false
  else if String.contains vowels (String.get word 0) then true
  else contains_vowel (tail word)

let remove_past_participles word num_vc =
  let len = String.length word in
  if num_vc > 0 && len >= 3 && get_last word 3 = "eed" then
    remove_last word 1
  else if len >= 3 && get_last word 3 = "eed" then word
  else if
    len >= 2
    && get_last word 2 = "ed"
    && contains_vowel (remove_last word 2)
  then remove_last word 2
  else if
    len >= 3
    && get_last word 3 = "ing"
    && contains_vowel (remove_last word 3)
  then remove_last word 3
  else word

let apply_finalize word =
  let len = String.length word in
  if
    len >= 2
    && get_last word 2 = "ed"
    && contains_vowel (remove_last word 2)
  then true
  else if
    len >= 3
    && get_last word 3 = "ing"
    && contains_vowel (remove_last word 3)
  then true
  else false

let end_cvc word =
  let units = create_units word in
  if String.length units >= 3 then
    let last_three = get_last units 3 in
    let last_letter = get_last word 1 in
    if
      last_three = "CVC" && last_letter <> "w" && last_letter <> "x"
      && last_letter <> "y"
    then true
    else false
  else false

let double_consonant word =
  let last_two = get_last word 2 in
  let second_to_last = remove_last last_two 1 in
  let last = get_last word 1 in
  second_to_last = last

let finalize_plurals_past_participles word num_vc =
  let len = String.length word in
  if end_cvc word && num_vc = 1 then word ^ "e"
  else if len >= 2 then
    let last_two = get_last word 2 in
    if last_two = "at" || last_two = "bl" || last_two = "iz" then
      word ^ "e"
    else if double_consonant word then remove_last word 1
    else word
  else word

let fix_y word =
  if contains_vowel word && get_last word 1 = "y" then
    remove_last word 1 ^ "i"
  else word

let remove_e word num_vc =
  if num_vc > 1 then remove_last word 1
  else if num_vc = 1 && end_cvc word = false then remove_last word 1
  else word

let check_double_consonant word num_vc =
  if num_vc > 1 && double_consonant word then remove_last word 1
  else word

let replace_suffix word =
  let complete23, len23 =
    if calc_vc (word |> create_units) > 0 then
      let replacement = find_suffix_binding hashtbl_step2_3 word "" 0 in
      let new_string =
        String.sub word 0 (String.length word - snd replacement)
      in
      (new_string ^ fst replacement, snd replacement)
    else (word, 0)
  in
  let complete4, len4 =
    if calc_vc (word |> create_units) > 1 then
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
  let vcs = calc_vc units in

  let step_1a = remove_plurals word in
  let step_1b = remove_past_participles step_1a vcs in

  let apply_final = apply_finalize step_1a in
  let final_step1ab =
    if apply_final then finalize_plurals_past_participles step_1b vcs
    else step_1b
  in
  let step_1c = fix_y final_step1ab in
  let step_234 = replace_suffix step_1c in
  let step_5a = remove_e step_234 vcs in
  let stemmed = check_double_consonant step_5a vcs in
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

let remove_stop_words (words : string list) = [ "" ]

let make_paragraph (sentences : string list) =
  List.fold_right (fun acc w -> acc ^ " " ^ w) sentences ""

let stem_paragraph (paragraph : string) =
  paragraph |> parse_sentence
  |> List.map process_sentence
  |> make_paragraph

let make_text_block (text : string) =
  { original_text = text; stemmed_text = stem_paragraph text }

let stemmed_text_block block = block.stemmed_text
