open Lwt
open Cohttp
open Cohttp_lwt_unix

let body subreddit_name =
  Client.get
    (Uri.of_string
       ("https://www.reddit.com/r/" ^ subreddit_name ^ "/hot.json"))
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n"
    (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let run_api subreddit_name =
  let body = Lwt_main.run (body subreddit_name) in
  print_endline ("Received body\n" ^ body)

let terminal () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nSending GET request to reddit for a specific subreddit.\n";
  print_endline "Enter the name of desired subreddit (excluding r/)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | subreddit_name -> run_api subreddit_name

let () = terminal ()