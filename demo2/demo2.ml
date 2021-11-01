open Analyzer

type command =
  | Frequencies
  | Stemmer
  | Encoder
  | NA

let rec sub subreddit_name =
  try
    Yojson.Basic.from_file ("data/" ^ subreddit_name ^ ".json")
    |> Intake.from_json
  with
  | Sys_error _ ->
      print_endline "Invalid subreddit name. Try again.";
      print_string "> ";
      sub (read_line ())

let rec get_command () =
  print_endline "Enter one of the following commands:";
  print_endline "Frequencies";
  print_endline "Stemmer";
  print_endline "Encoder";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> NA
  | command when command |> String.lowercase_ascii = "frequencies" ->
      Frequencies
  | command when command |> String.lowercase_ascii = "stemmer" ->
      Stemmer
  | command when command |> String.lowercase_ascii = "encoder" ->
      Encoder
  | _ ->
      print_endline "Did not recognize command. Please try again.\n";
      get_command ()

(* let rec string_of_array arr pos = if pos < Array.length arr then
   string_of_int arr.(pos) ^ " " ^ string_of_array arr (pos + 1) else ""

   let rec string_matrix (mat : int array array) : string = match mat
   with | [||] -> "" | [| one |] -> string_of_array one 0 | _ ->
   string_of_array mat.(0) 0 ^ "\n" ^ string_matrix (Array.sub mat 1
   (Array.length mat - 1)) *)
let print_frequencies post =
  print_endline
    ("Finding the most frequent words in the most recent post in r/"
    ^ (post |> Intake.subreddit_name))

let print_stemmer post =
  let original_text = post |> Intake.selftext in
  let text_block = original_text |> WordProcessor.make_text_block in
  let stemmed_text = text_block |> WordProcessor.stemmed_text_block in
  print_endline
    ("Stemming the text of most recent post from r/"
    ^ (post |> Intake.subreddit_name)
    ^ "\n");
  print_endline ("Original text: " ^ original_text ^ "\n");
  print_endline ("Stemmed text: " ^ stemmed_text)

let print_encoder subreddit post =
  let encoded_matrix =
    WordEncoding.create_encoded_matrix subreddit
      (*(post |> Intake.selftext)*)
      "business"
  in
  print_endline
    ("Encoding the most recent post from r/"
    ^ (post |> Intake.subreddit_name)
    ^ " based on all seen vocabulary in the subreddit\n");
  encoded_matrix |> Array.iter (fun x -> Array.iter print_int x; print_newline ());
  print_newline ()

let run subreddit_name =
  let subreddit = sub subreddit_name in
  match get_command () with
  | Frequencies -> print_frequencies (subreddit |> Intake.recent_post)
  | Stemmer -> print_stemmer (subreddit |> Intake.recent_post)
  | Encoder ->
      print_encoder
        ("data/subredditVocabJsons/" ^ subreddit_name ^ ".json"
        |> Yojson.Basic.from_file)
        (subreddit |> Intake.recent_post)
  | NA -> exit 0

let terminal () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWelcome to our NLP project.\n";
  print_endline "Enter the name of desired subreddit (excluding r/)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | subreddit_name -> run (subreddit_name |> String.lowercase_ascii)

let () = terminal ()