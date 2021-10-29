open WordProcessor
open Intake
open Yojson.Basic

let subreddit_json_to_words subreddit_json : string list =
  let subreddit = from_json subreddit_json in
  let all_words =
    List.fold_right
      (fun post acc -> (post |> selftext |> parse) @ acc)
      (posts subreddit) []
  in
  List.sort_uniq compare all_words

let subreddit_json_to_stemmed_words subreddit_json : string list =
  let unstemmed = subreddit_json_to_words subreddit_json in
  unstemmed |> stem_word_list |> extract_stemmed

let rec write_words_to_json (file : out_channel) (words : string list) :
    unit =
  match words with
  | [] -> output_string file "}"
  | [ h ] ->
      output_string file ("\"" ^ h ^ "\":\"\"\n");
      write_words_to_json file []
  | h :: t ->
      if pos_out file < 1 then (
        output_string file "{\n";
        write_words_to_json file (h :: t))
      else (
        output_string file ("\"" ^ h ^ "\":\"\",\n");
        write_words_to_json file t)

let subreddit_json_to_word_json processor_function subreddit_json : unit
    =
  let subreddit = from_json subreddit_json in
  let words = processor_function subreddit_json in
  let filename = subreddit |> recent_post |> subreddit_name in
  let filepath = "data/subredditVocabJsons/" ^ filename ^ ".json" in
  let file = open_out filepath in
  write_words_to_json file words
