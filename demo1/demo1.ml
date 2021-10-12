open Analyzer

let sub subreddit_name =
  Yojson.Basic.from_file (subreddit_name ^ ".json") |> Intake.from_json

let terminal () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nSending GET request to reddit for a specific subreddit.\n";
  print_endline "Enter the name of desired subreddit (excluding r/)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | subreddit_name -> print_endline subreddit_name

let () = terminal ()
