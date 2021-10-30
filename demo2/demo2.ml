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

let print_encoder post =
  print_endline
    ("Encoding the most recent post from r/"
    ^ (post |> Intake.subreddit_name)
    ^ " based on all seen vocabulary in the subreddit")

let run subreddit_name =
  let subreddit = sub subreddit_name in
  match get_command () with
  | Frequencies -> print_frequencies (subreddit |> Intake.recent_post)
  | Stemmer -> print_stemmer (subreddit |> Intake.recent_post)
  | Encoder -> print_encoder (subreddit |> Intake.recent_post)
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