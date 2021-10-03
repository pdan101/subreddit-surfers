(** This module contains the representation for the intake file. This
    file extracts data from JSON's that satisfy the schema.json file and
    converts it into more usable types, which can be used in function
    applications. *)

type author = string
(** The type of author name. *)

type post
(** The abstract type of values representing a specific reddit post. *)

type subreddit
(** The abstract type of values representing a specific subreddit.*)

val from_json : Yojson.Basic.t -> subreddit
(** [from_json json] is the subreddit that [json] represents. Requires:
    [j] is a valid JSON subreddit representation. *)

val subreddit_name : subreddit -> string
(** [subreddit_name subreddit] is the name of the subreddit in
    [subreddit]. *)

val subreddit_subscribers : subreddit -> int
(** [subreddit_subscribers subreddit] is the number of subscribers in
    [subreddit]. *)

val subreddit_type : subreddit -> string
(** [subreddit_type subreddit] is the publicity type of [subreddit]. *)

val post_ids : subreddit -> string list
(** [post_ids subreddit] is a set-like list of the posts in subreddit
    [subreddit] sorted by their ids.*)

val author : subreddit -> string -> string
(** [author subreddit post_id] is the author of a post with [post_id] in
    [subreddit].*)

val created_utc : subreddit -> string -> int
(** [created_utc subreddit post_id] is the Unix epoch time in UTC of a
    post with [post_id] in [subreddit].*)

val id : subreddit -> string -> string
(** [id subreddit post] is the id of a post of [post_id] in [subreddit].*)

val num_comments : subreddit -> string -> int
(** [num_comments subreddit post_id] is the number of comments to
    [post_id] in [subreddit].*)

val num_crossposts : subreddit -> string -> int
(** [num_crossposts subreddit post_id] is the number of crossposts for
    [post_id] in [subreddit].*)

val selftext : subreddit -> string -> string
(** [selftext subreddit post_id] is the text in the body of the post
    with [post_id] in [subreddit].*)

val spoiler : subreddit -> string -> bool
(** [spoiler subreddit post_id] is true is the post [post_id] in
    [subreddit] is tagged as a spoiler.*)

val title : subreddit -> string -> string
(** [title subreddit post_id] is the title of the post [post_id] in
    [subreddit].*)
