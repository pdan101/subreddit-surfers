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
  | UPrediction
  | ThemeAnalysis
  | ThemePrediction
  | NA

let print_text_file filename color =
  let input = open_in filename in
  try
    while true do
      ANSITerminal.print_string [ color ] (input_line input);
      print_newline ()
    done
  with
  | _ -> close_in input

(*Extracts the Intake.subreddit data structure of a given subreddit
  name, retries in command line if the file is not found.*)
let rec sub subreddit_name =
  try
    Yojson.Basic.from_file
      ("data" ^ Filename.dir_sep ^ subreddit_name ^ ".json")
    |> from_json
  with
  | Sys_error _ ->
      print_endline "Invalid subreddit name. Try again.";
      print_string "> ";
      sub (read_line ())

(*Converts text command into our command type.*)
let rec get_command () =
  print_text_file "data/graphics/options.txt" ANSITerminal.cyan;
  print_string "> ";
  match read_line () with
  | exception End_of_file -> NA
  | command
    when let lower = command |> String.lowercase_ascii in
         lower = "frequencies" || lower = "1" ->
      Frequencies
  | command
    when let lower = command |> String.lowercase_ascii in
         lower = "stemmer" || lower = "2" ->
      Stemmer
  | command
    when let lower = command |> String.lowercase_ascii in
         lower = "encoder" || lower = "3" ->
      Encoder
  | command
    when let lower = command |> String.lowercase_ascii in
         lower = "popularity" || lower = "4" ->
      Popularity
  | command
    when let lower = command |> String.lowercase_ascii in
         lower = "text prediction" || lower = "5" ->
      Prediction
  | command
    when let lower = command |> String.lowercase_ascii in
         lower = "upvote prediction" || lower = "6" ->
      UPrediction
  | command
    when let lower = command |> String.lowercase_ascii in
         lower = "theme analysis" || lower = "7" ->
      ThemeAnalysis
  | command
    when let lower = command |> String.lowercase_ascii in
         lower = "theme prediction" || lower = "8" ->
      ThemePrediction
  | command
    when let lower = command |> String.lowercase_ascii in
         lower = "quit" || lower = "9" ->
      NA
  | _ ->
      print_endline "Did not recognize command. Please try again.\n";
      get_command ()

let get_absolute_max array =
  let current_max = ref 1 in
  for i = 0 to Array.length array - 1 do
    if array.(i) < 0 then
      if array.(i) * -1 > !current_max then
        current_max := array.(i) * -1
      else ()
    else if array.(i) > !current_max then current_max := array.(i)
    else ()
  done;
  !current_max

let generate_spaces num =
  let str = ref "" in
  for i = 0 to num do
    str := !str ^ " "
  done;
  !str

let create_column (predicted_vote : float) (actual_vote : float) =
  let col = " |" in
  if predicted_vote < actual_vote then
    let pred_spaces = generate_spaces (int_of_float predicted_vote) in
    let col = col ^ pred_spaces ^ "⊡" in
    let actual_spaces =
      generate_spaces (int_of_float (actual_vote -. predicted_vote))
    in
    let extra_spaces =
      generate_spaces (int_of_float (50.0 -. actual_vote))
    in
    let col = col ^ actual_spaces ^ "⊙" ^ extra_spaces in
    col
  else
    let actual_spaces = generate_spaces (int_of_float actual_vote) in
    let col = col ^ actual_spaces ^ "⊙" in
    let pred_spaces =
      generate_spaces (int_of_float (predicted_vote -. actual_vote))
    in
    let extra_spaces =
      generate_spaces (int_of_float (50.0 -. predicted_vote))
    in
    let col = col ^ pred_spaces ^ "⊡" ^ extra_spaces in
    col

let graph_results
    (predicted_upvotes : int array)
    (actual_upvotes : float array) =
  let actual_upvotes_int =
    Array.map (fun e -> int_of_float e) actual_upvotes
  in
  let max_predicted = get_absolute_max predicted_upvotes in
  let max_actual = get_absolute_max actual_upvotes_int in
  let max =
    if max_predicted <= max_actual then max_actual else max_predicted
  in
  let scaling_factor = 50.0 /. float_of_int max in
  let columns =
    [ "  ―――――――――――――――――――――――――――――――――――――――――――――――˃" ]
  in

  let predicted_upvotes =
    Array.map (fun e -> float_of_int e) predicted_upvotes
  in
  print_newline ();
  print_newline ();
  print_string "     ⊙- Actual Upvotes, ⊡- Predicted Upvotes";
  print_newline ();

  let additional_cols =
    Array.mapi
      (fun i e ->
        create_column (scaling_factor *. e)
          (scaling_factor *. actual_upvotes.(i)))
      predicted_upvotes
  in
  let all_columns = columns @ Array.to_list additional_cols in
  List.iter
    (fun i ->
      print_string i;
      print_newline ())
    all_columns;
  print_string " |";
  print_newline ();
  print_string " ˅";
  print_newline ();
  print_newline ()

(*Current representation of 1 instance of a word in a subreddit for the
  text based graphic.*)
let occurence_string = "---"

(*Extracts the top 5 words appearing in a subreddit.*)
let rec extract_top5 lst count =
  if count >= 5 then ()
  else
    match lst with
    | [] -> print_newline ()
    | (k, v) :: t ->
        print_string (k ^ " (" ^ string_of_int v ^ ")" ^ ": ");
        for i = 1 to v do
          print_string occurence_string
        done;
        print_newline ();
        extract_top5 t (count + 1)

let print_left name total =
  let num_spaces = total - String.length name in
  for i = 1 to num_spaces do
    print_string " "
  done;
  print_string name

let print_podium lst =
  let arr =
    Array.of_list
      (lst |> List.map (fun x -> (fst x |> String.trim, snd x)))
  in
  let input = open_in "data/graphics/podium.txt" in
  for i = 1 to 13 do
    if i = 5 then print_left (fst arr.(2)) 33
    else if i = 9 then print_left (fst arr.(4)) 23;
    print_string (input_line input);
    if i = 1 then print_string (fst arr.(0))
    else if i = 3 then print_string (fst arr.(1))
    else if i = 7 then print_string (fst arr.(3));
    print_newline ()
  done

(*Prints the top 5 users in a subreddit, and their total upvotes. let
  rec print_users lst count = if count >= 5 then () else match lst with
  | [] -> print_newline () | (k, v) :: t -> print_endline ("User Name: "
  ^ k ^ "\nTotal Upvotes: " ^ string_of_int v ^ "\n"); print_users t
  (count + 1) *)
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
      ("data" ^ Filename.dir_sep ^ "subredditVocabJsons"
     ^ Filename.dir_sep ^ subreddit_name ^ ".json")
  in
  let encoded_matrix =
    encode_subreddit
      ("data" ^ Filename.dir_sep ^ "subredditVocabJsons"
       ^ Filename.dir_sep ^ subreddit_name ^ ".json"
      |> Yojson.Basic.from_file)
      stem_text
      (Yojson.Basic.from_file
         ("data" ^ Filename.dir_sep ^ subreddit_name ^ ".json"))
      upvotes
  in
  let frequency_list =
    find_frequencies json (Array.of_list encoded_matrix)
  in
  print_endline
    ("Finding the most frequent stemmed words r" ^ Filename.dir_sep
   ^ subreddit_name);
  extract_top5 frequency_list 0

(*Prints the stemmed text of the most recent post in a subreddit.*)
let print_stemmer post =
  let original_text = post |> selftext in
  let stemmed_text = original_text |> WordProcessor.stem_paragraph in
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
      ("data" ^ Filename.dir_sep ^ "subredditVocabJsons"
       ^ Filename.dir_sep ^ subreddit_name ^ ".json"
      |> Yojson.Basic.from_file)
      WordProcessor.stem_text
      ("data" ^ Filename.dir_sep ^ subreddit_name ^ ".json"
      |> Yojson.Basic.from_file)
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

(*Calculates predicted upvotes based on encoded input array and encoded
  subreddit.*)
let predict_upvotes encoded_arr encoded_subreddit =
  let weights =
    CustomRegression.get_weights encoded_subreddit 0.0 OLS
  in
  let float_array = Array.map (fun x -> float_of_int x) encoded_arr in
  let upvotes =
    CustomRegression.calc_upvotes
      (Mat.of_array float_array 1 (Array.length float_array))
      weights
  in
  upvotes.(0)

let graph_error subreddit_name =
  let encoded_subreddit =
    WordEncoding.encode_subreddit
      ("data" ^ Filename.dir_sep ^ "subredditVocabJsons"
       ^ Filename.dir_sep ^ subreddit_name ^ ".json"
      |> Yojson.Basic.from_file)
      WordProcessor.stem_text
      (Yojson.Basic.from_file
         ("data" ^ Filename.dir_sep ^ subreddit_name ^ ".json"))
      upvotes
  in

  let matrix = CustomRegression.create_matrix encoded_subreddit in
  let train_test_data =
    CustomRegression.get_training_data matrix 0.75
  in
  let actual_upvotes = train_test_data.output_testing in

  let weights =
    CustomRegression.get_weights encoded_subreddit 0.75 OLS
  in
  let predicted_upvotes =
    CustomRegression.calc_upvotes train_test_data.features_test weights
  in

  let predicted_upvotes =
    Array.map (fun e -> int_of_float e) predicted_upvotes
  in
  graph_results predicted_upvotes (Mat.to_array actual_upvotes)

(*Retrieves encoding of text and subreddit based on inputted subreddit
  name and text.*)
let get_both_arrays subreddit_name input_text =
  let converted_to_post =
    Intake.post_of_text (input_text |> WordProcessor.stem_paragraph)
  in
  let vocab_json =
    "data" ^ Filename.dir_sep ^ "subredditVocabJsons" ^ Filename.dir_sep
    ^ subreddit_name ^ ".json"
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
      ("data" ^ Filename.dir_sep ^ "subredditVocabJsons"
       ^ Filename.dir_sep ^ subreddit_name ^ ".json"
      |> Yojson.Basic.from_file)
      WordProcessor.stem_text
      (Yojson.Basic.from_file
         ("data" ^ Filename.dir_sep ^ subreddit_name ^ ".json"))
      upvotes
  in
  (encoded_arr, encoded_subreddit)

(*Prints prediction algorithm results.*)
let print_prediction subreddit_name =
  let input_text =
    print_endline
      "Enter text to predict how many upvotes it would receive: ";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ""
    | text -> text
  in
  let encoded_arr1, encoded_subreddit1 =
    get_both_arrays subreddit_name input_text
  in
  let encoded_arr2, encoded_subreddit2 =
    get_both_arrays (subreddit_name ^ "_new") input_text
  in
  let avg_upvotes =
    (predict_upvotes encoded_arr1 encoded_subreddit1
    +. predict_upvotes encoded_arr2 encoded_subreddit2)
    /. 2.
  in
  print_endline
    ("Your text stemmed: "
    ^ (input_text |> WordProcessor.stem_paragraph));
  print_newline ();
  print_endline
    ("This post would get "
    ^ string_of_int (int_of_float avg_upvotes)
    ^ " upvotes!")

let subreddit_theme_breakdown_to_assoc subreddit =
  let subreddit_json =
    "data" ^ Filename.dir_sep ^ subreddit ^ ".json"
    |> Yojson.Basic.from_file
  in
  let theme_breakdown =
    ThemeEncoder.theme_breakdown_of_subreddit
      ("data" ^ Filename.dir_sep ^ "themes")
      subreddit_json
    |> Array.to_list
  in
  let themes =
    "data/themes" |> ThemeEncoder.get_themes
    |> ThemeEncoder.themes_no_suffix |> Array.to_list
  in
  let with_misc = "miscellaneous" :: themes in
  let sorted =
    with_misc
    |> List.map String.lowercase_ascii
    |> List.sort_uniq Stdlib.compare
  in
  List.combine sorted theme_breakdown

let print_theme_analysis subreddit =
  print_string "Here is the Theme Breakdown for This Subreddit";
  print_endline "";
  print_endline "";
  let subreddit_breakdown =
    subreddit_theme_breakdown_to_assoc subreddit
  in
  List.iter
    (fun (theme, percentage) ->
      let width = percentage /. 10. in
      for i = 0 to int_of_float width do
        print_string "#"
      done;
      print_string (" " ^ theme ^ ": ");
      print_int (int_of_float percentage);
      print_endline "";
      print_endline "")
    subreddit_breakdown

let print_theme_prediction subreddit =
  let subreddit_json =
    "data" ^ Filename.dir_sep ^ subreddit ^ ".json"
    |> Yojson.Basic.from_file
  in
  let input_text =
    print_endline "Enter text to for theme prediction: ";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ""
    | text -> text
  in
  let theme_breakdown =
    let themes =
      "data/themes" |> ThemeEncoder.get_themes
      |> ThemeEncoder.themes_no_suffix
    in
    let dummy_post =
      {
        author = "";
        created_utc = 0.;
        subreddit = "";
        id = "";
        num_comments = 0;
        num_crossposts = 0;
        selftext = input_text;
        spoiler = false;
        title = "";
        upvotes = 0;
      }
    in
    let table =
      ThemeEncoder.theme_table_of_post dummy_post themes "themes"
    in
    ThemeEncoder.theme_breakdown_of_post dummy_post table
  in
  let theme_encoded =
    ThemeEncoder.encoded_theme_breakdown_matrix_of_subreddit
      ("data" ^ Filename.dir_sep ^ "themes")
      subreddit_json
  in
  let upvotes =
    predict_upvotes
      (theme_breakdown |> List.split |> snd |> List.map int_of_float
     |> Array.of_list)
      (theme_encoded
      |> List.map (List.map (fun elt -> int_of_float elt))
      |> List.map (fun lst -> Array.of_list lst))
  in
  print_newline ();
  print_endline "Here is a theme breakdown of the text in your post: ";
  print_newline ();
  List.iter
    (fun (th, perc) ->
      print_endline (th ^ ": " ^ string_of_float perc ^ "%"))
    theme_breakdown;
  print_endline
    ("Based on this post's theme breakdown it would get" ^ " "
    ^ string_of_int (int_of_float upvotes)
    ^ " upvotes!")

let list_of_subs =
  [ "cornell"; "csmajors"; "ocaml"; "running"; "college" ]

let convert_to_name input =
  match int_of_string_opt input with
  | None -> input
  | Some x -> List.nth list_of_subs (x - 1)

(*Runs the terminal interface that gets a command after specifying
  subreddit.*)
let run subreddit_name =
  let subreddit = sub (subreddit_name |> convert_to_name) in
  let fixed_sub_name =
    subreddit |> Intake.recent_post |> Intake.subreddit_name
    |> String.lowercase_ascii
  in
  while true do
    match get_command () with
    | Frequencies -> print_frequencies fixed_sub_name
    | Stemmer -> print_stemmer (subreddit |> recent_post)
    | Encoder -> print_encoder fixed_sub_name
    | Popularity -> print_podium (subreddit |> posts |> top_users)
    | Prediction -> print_prediction fixed_sub_name
    | UPrediction -> graph_error fixed_sub_name
    | ThemeAnalysis -> print_theme_analysis fixed_sub_name
    | ThemePrediction -> print_theme_prediction fixed_sub_name
    | NA ->
        print_text_file "data/graphics/waves.txt" ANSITerminal.blue;
        exit 0
  done

(*Runs the initial terminal that allows a subreddit to be selected.
  Surfer art from https://www.asciiart.eu/sports-and-outdoors/surfing*)
let terminal () =
  print_text_file "data/graphics/logo.txt" ANSITerminal.blue;
  print_text_file "data/graphics/subreddits.txt" ANSITerminal.green;
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | subreddit_name -> run (subreddit_name |> String.lowercase_ascii)

(*Starts the executable program.*)
let () = terminal ()