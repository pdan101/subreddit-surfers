type word_data = {
  word : string;
  occurences : int;
  stemmed : string;
  meaning : string;
}

type vocabulary = word_data list

let rec remove_punc s =
  if String.length s = 0 then s
  else
    let is_char =
      (s.[0] |> Char.uppercase_ascii |> Char.code)
      - ('a' |> Char.uppercase_ascii |> Char.code)
    in
    if is_char >= 0 && is_char <= 25 then
      String.make 1 s.[0]
      ^ (String.sub s 1 (String.length s - 1) |> remove_punc)
    else String.sub s 1 (String.length s - 1) |> remove_punc

let parse (text : string) =
  String.split_on_char ' ' text
  |> List.filter (fun x -> String.length x > 0)
  |> List.map (fun x -> String.trim x)
  |> List.map (fun x -> remove_punc x)

let consonants = "bcdfghjklmnpqrstwxz"

let vowels = "aeiouy"

let tail word = String.sub word 1 (String.length word - 1)

let rec find_group word type_char =
  if
    String.length word > 0
    && String.contains type_char (String.get word 0)
  then find_group (tail word) type_char
  else word

let rec create_units (word : string) =
  if String.length word >= 1 then
    let remove_vowels = find_group word vowels in
    let remove_consonants = find_group word consonants in
    if remove_vowels <> word then "V" ^ create_units remove_vowels
    else "C" ^ create_units remove_consonants
  else ""

(*TODO make this method better- should not be using try/catch*)
let rec calc_m char_string =
  let first_char =
    try String.get char_string 0 with
    | _ -> '-'
  in
  let second_char =
    try String.get char_string 1 with
    | _ -> '-'
  in
  if first_char = 'V' && second_char = 'C' then
    1 + calc_m (tail char_string)
  else if first_char = '-' || second_char = '-' then 0
  else calc_m (tail char_string)

let stem (word : string) = raise (Failure "Unimplemented")
