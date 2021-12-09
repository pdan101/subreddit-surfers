(**This module contains the functions and types necessary for theme
   analysis of subreddits*)

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
