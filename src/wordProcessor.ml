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

let vowels = "aeiouy"

let consonants = "bcdfghjklmnpqrstwxz"

let check_same type_char word =
  if
    String.contains type_char (String.get word 0)
    && String.contains type_char (String.get word 1)
  then true
  else false

let get_tail num_remove word =
  String.sub word num_remove (String.length word - num_remove)

let rec create_units (word : string) =
  if String.length word >= 2 then
    if check_same vowels word then 'V' :: create_units (get_tail 2 word)
    else if check_same consonants word then
      'C' :: create_units (get_tail 2 word)
    else if String.contains vowels (String.get word 0) then
      'V' :: create_units (get_tail 1 word)
    else 'C' :: create_units (get_tail 1 word)
  else if String.length word = 1 then
    if String.contains vowels (String.get word 0) then [ 'V' ]
    else [ 'C' ]
  else []

let stem (word : string) = raise (Failure "Unimplemented")
