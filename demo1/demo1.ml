open Analyzer

let rec sub subreddit_name =
  try
    Yojson.Basic.from_file ("data/" ^ subreddit_name ^ ".json")
    |> Intake.from_json
  with
  | Sys_error _ ->
      print_endline "Invalid subreddit name. Try again.";
      print_string "> ";
      sub (read_line ())

let run_analysis subreddit_name =
  let intake_sub = sub subreddit_name in
  let recent_post_text =
    intake_sub |> Intake.recent_post |> Intake.selftext
  in
  let () = print_endline recent_post_text in
  let () = print_float (Sentiment.polarity_score recent_post_text) in
  let () = print_newline () in
  ()

let terminal () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nSending GET request to reddit for a specific subreddit.\n";
  print_endline "Enter the name of desired subreddit (excluding r/)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | subreddit_name -> run_analysis subreddit_name

let () = terminal ()
