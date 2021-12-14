(**This module contains the functions and types necessary for theme
   analysis of subreddits*)

val get_themes : string -> string array
(**[get_themes theme_dir] is the array of theme filenames in theme_dir*)

val themes_no_suffix : string array -> string array
(**[theme_no_suffix themes] is the array of filenames with suffixes
   removed*)

val theme_breakdown_of_post :
  Intake.post -> (string, int) Hashtbl.t -> (string * float) list
(**[theme_breakdown_of_post post theme_table] is the association list
   where the keys are themes and the values are the percentages of the
   themes in the post*)

val encoded_theme_breakdown_matrix_of_subreddit :
  string -> Yojson.Basic.t -> float list list
(**[encoded_theme_breakdown_matrix_of_subreddit theme_dir subreddit_json]
   is the encoding matrix for a subreddit where each row represents a
   post, and each column represents a theme. The values at each index is
   the percentage of the post that involves that theme. The last column
   is the upvotes that each post has*)

val theme_breakdown_of_subreddit :
  string -> Yojson.Basic.t -> float array
(**[theme_breakdown_of_subreddit theme theme_dir subreddit_json] is an
   array of floats where each index corresponds to a theme and the
   values at each index are the percentages of the corresponding theme
   in the subreddit*)

val num_theme_words_of_post : Intake.post -> Yojson.Basic.t -> int
(**[num_theme_words_of_post post theme_json] is the number of words in
   post that correspond to a word in theme_json*)

val theme_table_of_post :
  Intake.post -> string array -> string -> (string, int) Hashtbl.t
(**[theme_table_of_post post themes theme_dir] is the Hashtbl where the
   keys are themes and the values are the number of words corresponding
   to that theme in post*)
