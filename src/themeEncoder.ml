open Yojson.Basic.Util

let get_themes theme_dir = Sys.readdir theme_dir

let themes_no_suffix themes =
  Array.map (fun s -> String.sub s 0 (String.length s - 5)) themes

let compare_string_tuples (k1, v2) (k2, v2) =
  if String.compare k1 k2 > 0 then 1
  else if String.compare k1 k2 < 0 then -1
  else 0

let num_theme_words_of_post
    (post : Intake.post)
    (theme_json : Yojson.Basic.t) : int =
  let stemmed_post_text_list =
    WordProcessor.stem_text (WordEncoding.post_text post)
  in
  let theme_words =
    theme_json |> member "words" |> to_list |> List.map to_string
    |> List.map String.lowercase_ascii
    |> WordProcessor.stem_word_list |> WordProcessor.extract_stemmed
  in
  let filtered_post =
    List.filter
      (fun elt1 ->
        List.exists
          (fun elt2 ->
            if String.compare elt1 elt2 = 0 then true else false)
          theme_words)
      stemmed_post_text_list
  in
  List.length filtered_post

let theme_table_of_post
    (post : Intake.post)
    (themes : string array)
    (theme_dir : string) : (string, int) Hashtbl.t =
  let theme_length = Array.length themes in
  let theme_table = Hashtbl.create (theme_length * 2) in
  let i = ref 0 in
  while !i < theme_length do
    let current_theme = themes.(!i) in
    let theme_json =
      "data" ^ Filename.dir_sep ^ theme_dir ^ Filename.dir_sep
      ^ current_theme ^ ".json"
      |> Yojson.Basic.from_file
    in
    let theme_words_in_post = num_theme_words_of_post post theme_json in
    Hashtbl.add theme_table current_theme theme_words_in_post;
    i := !i + 1
  done;
  theme_table

let rec theme_breakdown_of_post_acc
    (themes : string list)
    (acc : (string * float) list)
    (theme_table : (string, int) Hashtbl.t)
    (num_words_post : int) =
  match themes with
  | [] -> acc
  | h :: t ->
      let float_of_num_theme_words =
        h |> Hashtbl.find theme_table |> float_of_int
      in
      let percentage =
        Float.round
          (float_of_num_theme_words
          /. float_of_int num_words_post
          *. 100.)
      in
      theme_breakdown_of_post_acc t
        ((h, percentage) :: acc)
        theme_table num_words_post

let remaining_percentage (theme_percentages : (string * float) list) :
    float =
  let rec total_percentage_acc percentages acc =
    match percentages with
    | [] -> acc
    | (s, percent) :: t -> total_percentage_acc t (percent +. acc)
  in
  let total_percent = total_percentage_acc theme_percentages 0. in
  100. -. total_percent

let theme_breakdown_of_post
    (post : Intake.post)
    (theme_table : (string, int) Hashtbl.t) : (string * float) list =
  let themes =
    List.sort String.compare
      (Hashtbl.fold (fun k v acc -> acc @ [ k ]) theme_table [])
  in
  let post_words =
    WordProcessor.stem_text (WordEncoding.post_text post)
  in
  let num_words = List.length post_words in
  let _ = print_int num_words in
  let theme_percentages =
    theme_breakdown_of_post_acc themes [] theme_table num_words
  in
  let misc_percentage = remaining_percentage theme_percentages in
  List.sort compare_string_tuples
    (("miscellaneous", misc_percentage) :: theme_percentages)

let encoded_theme_breakdown_matrix_of_subreddit
    (theme_dir : string)
    (subreddit_json : Yojson.Basic.t) : float list list =
  let subreddit = Intake.from_json subreddit_json in
  let posts = Intake.posts subreddit in
  let themes = themes_no_suffix (get_themes theme_dir) in
  List.fold_left
    (fun acc post ->
      let theme_table = theme_table_of_post post themes "themes" in
      ((theme_breakdown_of_post post theme_table |> List.split |> snd)
      @ [ float_of_int (Intake.upvotes post) ])
      :: acc)
    [] posts

let theme_breakdown_of_subreddit
    (theme_dir : string)
    (subreddit_json : Yojson.Basic.t) : float array =
  let theme_breakdown =
    encoded_theme_breakdown_matrix_of_subreddit "themes" subreddit_json
  in
  let percentage_matrix =
    Array.of_list (List.map Array.of_list theme_breakdown)
  in
  let themes = get_themes theme_dir in
  let breakdown = Array.make (Array.length themes) 0. in
  let posts = subreddit_json |> Intake.from_json |> Intake.posts in
  let posts_size = List.length posts in
  for col = 0 to Array.length themes do
    let total_percentage = ref 0. in
    for row = 0 to Array.length percentage_matrix do
      total_percentage :=
        !total_percentage +. percentage_matrix.(row).(col)
    done;
    breakdown.(col) <- !total_percentage
  done;
  let breakdown_percent =
    Array.map (fun x -> x /. float_of_int posts_size) breakdown
  in
  breakdown_percent
