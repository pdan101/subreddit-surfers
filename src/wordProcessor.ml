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
    let current_char = s.[0] in
    let current_char_code = current_char |> Char.code in
    let is_char =
      (current_char |> Char.uppercase_ascii |> Char.code)
      - ('a' |> Char.uppercase_ascii |> Char.code)
    in
    if
      (is_char >= 0 && is_char <= 25)
      || current_char_code = ('\'' |> Char.code)
      || current_char_code = ("’".[0] |> Char.code)
    then
      String.make 1 current_char
      ^ (String.sub s 1 (String.length s - 1) |> remove_punc)
    else String.sub s 1 (String.length s - 1) |> remove_punc

let parse (text : string) =
  String.split_on_char ' ' text
  |> List.filter (fun x -> String.length x > 0)
  |> List.map (fun x -> String.trim x)
  |> List.map (fun x -> remove_punc x)

let stem (word : string) = raise (Failure "Unimplemented")
