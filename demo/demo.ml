open Analyzer
open WordEncoding
open Intake
open WordProcessor
open CustomRegression
open Owl

(*Type of command to execute.*)
type command =
  | Frequencies
  | Stemmer
  | Encoder
  | Popularity
  | Prediction
  | NA

(*Extracts the Intake.subreddit data structure of a given subreddit
  name, retries in command line if the file is not found.*)
let rec sub subreddit_name =
  try
    Yojson.Basic.from_file ("data/" ^ subreddit_name ^ ".json")
    |> from_json
  with
  | Sys_error _ ->
      print_endline "Invalid subreddit name. Try again.";
      print_string "> ";
      sub (read_line ())

(*Converts text command into our command type.*)
let rec get_command () =
  print_endline "Enter one of the following commands:";
  print_endline "Frequencies";
  print_endline "Stemmer";
  print_endline "Encoder";
  print_endline "Popularity";
  print_endline "Prediction";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> NA
  | command when command |> String.lowercase_ascii = "frequencies" ->
      Frequencies
  | command when command |> String.lowercase_ascii = "stemmer" ->
      Stemmer
  | command when command |> String.lowercase_ascii = "encoder" ->
      Encoder
  | command when command |> String.lowercase_ascii = "popularity" ->
      Popularity
  | command when command |> String.lowercase_ascii = "prediction" ->
      Prediction
  | _ ->
      print_endline "Did not recognize command. Please try again.\n";
      get_command ()

(*Current representation of 1 instance of a word in a subreddit for the
  text based graphic.*)
let big_string = "---"

(*Extracts the top 5 words appearing in a subreddit.*)
let rec extract_top5 lst count =
  if count >= 5 then ()
  else
    match lst with
    | [] -> print_newline ()
    | (k, v) :: t ->
        print_string (k ^ " (" ^ string_of_int v ^ ")" ^ ": ");
        for i = 1 to v do
          print_string big_string
        done;
        print_newline ();
        extract_top5 t (count + 1)

(*Prints the top 5 users in a subreddit, and their total upvotes.*)
let rec print_users lst count =
  if count >= 5 then ()
  else
    match lst with
    | [] -> print_newline ()
    | (k, v) :: t ->
        print_endline
          ("User Name: " ^ k ^ "\nTotal Upvotes: " ^ string_of_int v
         ^ "\n");
        print_users t (count + 1)

(*Builds a descending order sorted association list of users and their
  total upvotes.*)
let rec top_users post_list =
  let user_tbl = Hashtbl.create (List.length post_list) in
  List.iter
    (fun x ->
      let author = x |> author in
      let upvotes = x |> upvotes in
      match Hashtbl.find_opt user_tbl author with
      | None -> Hashtbl.add user_tbl author upvotes
      | Some value -> Hashtbl.add user_tbl author (value + upvotes))
    post_list;
  print_endline "Finding top users in this subreddit: \n";
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) user_tbl []
  |> List.sort_uniq (fun x y -> snd y - snd x)

(*Prints the most frequent words in a subreddit.*)
let print_frequencies subreddit_name =
  let json =
    Yojson.Basic.from_file
      ("data/subredditVocabJsons/" ^ subreddit_name ^ ".json")
  in
  let encoded_matrix =
    encode_subreddit
      ("data/subredditVocabJsons/" ^ subreddit_name ^ ".json"
      |> Yojson.Basic.from_file)
      stem_text
      (Yojson.Basic.from_file ("data/" ^ subreddit_name ^ ".json"))
      upvotes
  in
  let frequency_list =
    find_frequencies json (Array.of_list encoded_matrix)
  in
  print_endline ("Finding the most frequent words r/" ^ subreddit_name);
  extract_top5 frequency_list 0

(*Prints the stemmed text of the most recent post in a subreddit.*)
let print_stemmer post =
  let original_text = post |> selftext in
  let text_block = original_text |> WordProcessor.make_text_block in
  let stemmed_text = text_block |> WordProcessor.stemmed_text_block in
  print_endline
    ("Stemming the text of most recent post from r/"
    ^ (post |> subreddit_name)
    ^ "\n");
  print_endline ("Original text: " ^ original_text ^ "\n");
  print_endline ("Stemmed text: " ^ stemmed_text)

(*Prints the encoded matrix for all the non-empty posts in the
  subreddit.*)
let print_encoder subreddit_name =
  let encoded_matrix =
    encode_subreddit
      ("data/subredditVocabJsons/" ^ subreddit_name ^ ".json"
      |> Yojson.Basic.from_file)
      WordProcessor.stem_text
      ("data/" ^ subreddit_name ^ ".json" |> Yojson.Basic.from_file)
      upvotes
  in
  print_endline
    ("Encoding r/" ^ subreddit_name
   ^ " based on all seen vocabulary in the subreddit\n");
  encoded_matrix |> Array.of_list
  |> Array.iter (fun x ->
         Array.iter print_int x;
         print_newline ();
         print_newline ());
  print_newline ()

let print_prediction subreddit_name =
  let input_text =
    print_endline
      "Enter text to predict how many upvotes it would receive: ";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ""
    | text -> text
  in
  let converted_to_post =
    Intake.post_of_text (input_text |> WordProcessor.stem_paragraph)
  in
  let vocab_json =
    "data/subredditVocabJsons/" ^ subreddit_name ^ ".json"
    |> Yojson.Basic.from_file
  in
  let encode_temp =
    WordEncoding.encode_post vocab_json WordProcessor.stem_text
      converted_to_post Intake.upvotes
  in
  let encoded_arr =
    Array.sub encode_temp 0 (Array.length encode_temp - 1)
  in
  let encoded_subreddit =
    WordEncoding.encode_subreddit
      ("data/subredditVocabJsons/" ^ subreddit_name ^ ".json"
      |> Yojson.Basic.from_file)
      WordProcessor.stem_text
      (Yojson.Basic.from_file ("data/" ^ subreddit_name ^ ".json"))
      upvotes
  in
  let weights =
    CustomRegression.train_test_model encoded_subreddit 0.75 OLS
  in
  let float_array = Array.map (fun x -> float_of_int x) encoded_arr in
  let upvotes =
    CustomRegression.calc_upvotes
      (Mat.of_array float_array 1 (Array.length float_array))
      weights
  in
  print_endline
    ("This post would get "
    ^ string_of_int (int_of_float upvotes.(0))
    ^ " upvotes!")

(*Runs the terminal interface that gets a command after specifying
  subreddit.*)
let run subreddit_name =
  let subreddit = sub subreddit_name in
  match get_command () with
  | Frequencies ->
      print_frequencies (subreddit_name |> String.lowercase_ascii)
  | Stemmer -> print_stemmer (subreddit |> recent_post)
  | Encoder -> print_encoder (subreddit_name |> String.lowercase_ascii)
  | Popularity -> print_users (subreddit |> posts |> top_users) 0
  | Prediction ->
      print_prediction (subreddit_name |> String.lowercase_ascii)
  | NA -> exit 0

(*Runs the initial terminal that allows a subreddit to be selected.*)
let terminal () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWelcome to our NLP project.\n";
  print_endline "Enter the name of desired subreddit (excluding r/)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | subreddit_name -> run (subreddit_name |> String.lowercase_ascii)

(*Starts the executable program.*)
let () = terminal ()