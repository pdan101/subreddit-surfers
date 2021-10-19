open Analyzer
open WordProcessor

let rec sub subreddit_name =
  try
    Yojson.Basic.from_file ("data/" ^ subreddit_name ^ ".json")
    |> Intake.from_json
  with
  | Sys_error _ ->
      print_endline "Invalid subreddit name. Try again.";
      print_string "> ";
      sub (read_line ())

let print_graph score =
  let pos = String.make (23. *. score |> int_of_float) ' ' in
  let arrow_up = pos ^ "^" in
  let score_up =
    String.sub pos 2 (String.length pos - 2) ^ string_of_float score
  in
  print_endline "       SENTIMENT       ";
  print_endline "|----------|----------|";
  print_endline "Neg.    Neutral      Pos.";
  print_endline arrow_up;
  print_endline score_up

let run_analysis subreddit_name =
  let intake_sub = sub subreddit_name in
  let recent_post_text =
    intake_sub |> Intake.recent_post |> Intake.selftext
    (*ADD ANOTHER PIPELINE FOR PROCESSED TEXT*)
  in
  let processed_post_text =
    recent_post_text |> make_text_block |> stemmed_text_block
  in
  let () =
    print_endline
      ("Pulling the text from the most recent hot post in r/"
     ^ subreddit_name ^ ": \n")
  in
  let () = print_endline recent_post_text in
  let () = print_newline () in
  let () =
    print_endline ("Processed Post Text: \n" ^ processed_post_text)
  in
  let () = print_newline () in
  let () = print_graph (Sentiment.polarity_score recent_post_text) in
  ()

let terminal () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nSending GET request to reddit for a specific subreddit.\n";
  print_endline "Enter the name of desired subreddit (excluding r/)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | subreddit_name ->
      run_analysis (subreddit_name |> String.lowercase_ascii)

let () = terminal ()
