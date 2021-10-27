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

let rec find_group word type_char =
  let lowercase_word = String.lowercase_ascii word in
  if
    String.length word > 0
    && String.contains type_char (String.get lowercase_word 0)
  then find_group (tail lowercase_word) type_char
  else word

let rec create_units (word : string) =
  if String.length word >= 1 then
    let remove_vowels = find_group word vowels in
    let remove_consonants = find_group word consonants in
    if remove_vowels <> word then "V" ^ create_units remove_vowels
    else "C" ^ create_units remove_consonants
  else ""

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

let get_last word num = String.sub word (String.length word - num) num

let remove_last word num = String.sub word 0 (String.length word - num)

let remove_plurals word =
  let len = String.length word in
  if len >= 4 && get_last word 4 = "sses" then remove_last word 2
  else if len >= 3 && get_last word 3 = "ies" then remove_last word 2
  else if len >= 2 && get_last word 2 = "ss" then remove_last word 2
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

let end_cvc word =
  let units = create_units word in
  if String.length units >= 3 then
    let last_three = get_last units 3 in
    let last_letter = get_last word 1 in
    if last_three = "CVC" && last_letter <> "w" || last_letter <> "x" || last_letter <> "y" then true
    else false
  else false

let double_consonant word =
  let last_two = get_last word 2 in
  let second_to_last = remove_last last_two 1 in
  let last = get_last word 1 in second_to_last = last

let finalize_plurals_past_participles word num_vcs =
  let len = String.length word in
  if len >= 3 && end_cvc word && num_vcs = 1 then word ^ "e" 
  else if len >= 2 then
    let last_two = get_last word 2 in
    if last_two = "at" || last_two = "bl" || last_two = "iz" then word ^ "e"
    else if double_consonant word then remove_last word 1
    else word
  else word

let stemmer (word : string) =
  let units = create_units word in
  let vcs = calc_vc units in
  let stemmed = vcs |> remove_past_participles word |> remove_plurals in
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
