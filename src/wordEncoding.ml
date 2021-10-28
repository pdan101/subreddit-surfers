open WordProcessor
open Intake
open Yojson

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

let write_words_to_json (file : out_channel) (words : string list) :
    unit =
  List.iter
    (fun x ->
      output_string file ("\t" ^ "\"" ^ x ^ "\"" ^ ":" ^ "\"" ^ "\",\n"))
    words

(* let subreddit_json_to_word_json subreddit_json : unit = let subreddit
   = from_json subreddit_json in let filename = subreddit |> recent_post
   |> subreddit_name in *)
