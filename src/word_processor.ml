type word_data = {
  word : string;
  occurences : int;
  stemmed : string;
  meaning : string;
}

type vocabulary = word_data list

let parse (text : string) = String.split_on_char ' ' text

let stem (word : string) = raise (Failure "Unimplemented")
