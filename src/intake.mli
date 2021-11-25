(** This module contains the representation for the intake file. This
    file extracts data from JSON's that satisfy the schema.json file and
    converts it into more usable types, which can be used in function
    applications. *)

type post
(** The abstract type of values representing a specific reddit post. *)

type subreddit
(** The abstract type of values representing a specific subreddit.*)

val post_of_text : string -> post
(** [post_of_text text] converts text into a dummy post, so it can be
    used in other functions.*)

val from_json : Yojson.Basic.t -> subreddit
(** [from_json json] is the subreddit that [json] represents. Requires:
    [json] is a valid JSON subreddit representation. *)

val recent_post : subreddit -> post
(** [recent_post subreddit] is the most recent post in [subreddit].
    Fails with "No posts in this subreddit" if there are no posts.*)

val posts : subreddit -> post list
(** [posts subreddit] is the list of posts in [subreddit].*)

val author : post -> string
(** [author post] is the author of [post].*)

val created_utc : post -> float
(** [created_utc post] is the creation time of [post].*)

val id : post -> string
(** [id post] is the id of [post].*)

val num_comments : post -> int
(** [num_comments post] is the number of comments of [post].*)

val num_crossposts : post -> int
(** [num_crossposts post] is the number of crossposts of [post].*)

val selftext : post -> string
(** [selftext post] is the text of [post].*)

val spoiler : post -> bool
(** [spoiler post] indicates if [post] has a spoiler.*)

val upvotes : post -> int
(** [author post] is the number of upvotes of [post].*)

val subreddit_name : post -> string
(**[subreddit post] is the name of the subreddit that [post] belongs to.*)

val title : post -> string
(**[title post] is the title of [post]*)
