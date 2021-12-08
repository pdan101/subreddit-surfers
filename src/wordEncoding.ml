open WordProcessor
open Intake
open Yojson.Basic.Util

let post_text post = title post ^ selftext post

let subreddit_json_to_words subreddit_json : string list =
  let subreddit = from_json subreddit_json in
  let all_words =
    List.fold_right
      (fun post acc ->
        (post |> post_text |> parse |> List.map String.lowercase_ascii)
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

let subreddit_json_to_word_json
    processor_function
    subreddit_json
    filename : unit =
  let words = processor_function subreddit_json in
  let filepath =
    "data" ^ Filename.dir_sep ^ "subredditVocabJsons" ^ Filename.dir_sep
    ^ (filename |> String.lowercase_ascii)
    ^ ".json"
  in
  let file = open_out filepath in
  write_words_to_json file words

let word_json_to_array (word_json : Yojson.Basic.t) : string array =
  word_json |> member "words" |> to_list |> List.map to_string
  |> List.map String.lowercase_ascii
  |> Array.of_list

exception Element_Not_Found

let rec find_index (current_index : int) (x : 'a) (arr : 'a array) : int
    =
  if current_index >= Array.length arr then raise Element_Not_Found
  else if x = arr.(current_index) then current_index
  else find_index (current_index + 1) x arr

let encode_post
    (vocab_json : Yojson.Basic.t)
    (processor_function : string -> string list)
    (post : Intake.post)
    (numerical_data : post -> int) : int array =
  let vocab_array = word_json_to_array vocab_json in
  let post_array =
    Array.of_list (processor_function (post_text post))
  in
  let encoded_post = Array.make (Array.length vocab_array + 1) 0 in
  Array.iteri
    (fun index x ->
      let x2 = String.lowercase_ascii x in
      let word_index =
        match find_index 0 x2 vocab_array with
        | exception Element_Not_Found -> Array.length vocab_array
        | x2 -> x2
      in
      if word_index = Array.length vocab_array then
        encoded_post.(0) <- encoded_post.(0)
      else
        let old_value = encoded_post.(word_index) in
        encoded_post.(word_index) <- 1 + old_value)
    post_array;
  encoded_post.(Array.length encoded_post - 1) <- upvotes post;
  encoded_post

let encode_subreddit
    (vocab_json : Yojson.Basic.t)
    (processor_function : string -> string list)
    (subreddit_json : Yojson.Basic.t)
    (numerical_data_retriever : post -> int) : int array list =
  let subreddit = from_json subreddit_json in
  let posts = Intake.posts subreddit in
  let post_texts =
    posts |> List.filter (fun x -> String.length (post_text x) > 0)
  in
  List.fold_left
    (fun acc elt ->
      encode_post vocab_json processor_function elt
        numerical_data_retriever
      :: acc)
    [] post_texts

let rec find_map key map =
  match map with
  | [] -> None
  | (a_key, value) :: tail ->
      if key = a_key then Some value else find_map key tail

let insert key value map =
  if List.mem_assoc key map then
    let old_value = List.assoc key map in
    let map = List.remove_assoc key map in
    (key, value + old_value) :: map
  else (key, value) :: map

let compare_keys (key1, val1) (key2, val2) =
  if val1 = val2 then 0 else if val1 > val2 then -1 else 1

let find_frequencies
    (word_json : Yojson.Basic.t)
    (matrix : int array array) =
  let vocab_array = word_json_to_array word_json in

  let frequency_map = ref [] in
  for col = 0 to Array.length vocab_array - 1 do
    frequency_map := insert vocab_array.(col) 0 !frequency_map
  done;

  for row = 0 to Array.length matrix - 1 do
    for col = 0 to Array.length matrix.(0) - 2 do
      frequency_map :=
        insert vocab_array.(col) matrix.(row).(col) !frequency_map
    done
  done;
  List.sort compare_keys !frequency_map
