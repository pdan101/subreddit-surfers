open WordProcessor
open Intake
open Yojson.Basic.Util

let subreddit_json_to_words subreddit_json : string list =
  let subreddit = from_json subreddit_json in
  let all_words =
    List.fold_right
      (fun post acc ->
        (post |> selftext |> parse |> List.map String.lowercase_ascii)
        @ acc)
      (posts subreddit) []
  in
  List.sort_uniq compare all_words

let subreddit_json_to_stemmed_words subreddit_json : string list =
  let unstemmed = subreddit_json_to_words subreddit_json in
  unstemmed |> stem_word_list |> extract_stemmed

let rec write_words_to_json (file : out_channel) (words : string list) :
    unit =
  match words with
  | [] -> output_string file "\t]\n}"
  | [ h ] ->
      output_string file ("\t\t\"" ^ h ^ "\"\n");
      write_words_to_json file []
  | h :: t ->
      if pos_out file < 1 then (
        output_string file "{\n\t\"words\": [\n";
        write_words_to_json file (h :: t))
      else (
        output_string file ("\t\t\"" ^ h ^ "\",\n");
        write_words_to_json file t)

let subreddit_json_to_word_json processor_function subreddit_json : unit
    =
  let subreddit = from_json subreddit_json in
  let words = processor_function subreddit_json in
  let filename = subreddit |> recent_post |> subreddit_name in
  let filepath = "data/subredditVocabJsons/" ^ filename ^ ".json" in
  let file = open_out filepath in
  write_words_to_json file words

let word_json_to_array (word_json : Yojson.Basic.t) : string array =
  word_json |> member "words" |> to_list |> List.map to_string
  |> Array.of_list

let rec find_index (current_index : int) (x : 'a) (arr : 'a array) : int
    =
  if x = arr.(current_index) then current_index
  else find_index (current_index + 1) x arr

let encode_post
    (vocab_json : Yojson.Basic.t)
    (processor_function : string -> string list)
    (post : string) : int array =
  let vocab_array = word_json_to_array vocab_json in
  let post_array = Array.of_list (processor_function post) in
  let encoded_post = Array.make 1 0 in
  Array.iteri
    (fun index x ->
      let word_index = find_index 0 x vocab_array in
      let old_value = encoded_post.(word_index) in
      encoded_post.(word_index) <- 1 + old_value)
    post_array;
  encoded_post

let encode_subreddit
    (vocab_json : Yojson.Basic.t)
    (processor_function : string -> string list)
    (subreddit_json : Yojson.Basic.t) : int array list =
  let subreddit = from_json subreddit_json in
  let posts = Intake.posts subreddit in
  let post_texts = List.map Intake.selftext posts in
  List.fold_left
    (fun acc elt ->
      encode_post vocab_json processor_function elt :: acc)
    [] post_texts
