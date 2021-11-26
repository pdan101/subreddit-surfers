open Yojson.Basic.Util

type post = {
  author : string;
  created_utc : float;
  subreddit : string;
  id : string;
  num_comments : int;
  num_crossposts : int;
  selftext : string;
  spoiler : bool;
  title : string;
  upvotes : int;
}

type subreddit = post list

let post_of_json json =
  let next = json |> member "data" in
  {
    author = next |> member "author" |> to_string;
    created_utc = next |> member "created_utc" |> to_float;
    subreddit = next |> member "subreddit" |> to_string;
    id = next |> member "id" |> to_string;
    num_comments = next |> member "num_comments" |> to_int;
    num_crossposts = next |> member "num_crossposts" |> to_int;
    selftext = next |> member "selftext" |> to_string;
    spoiler = next |> member "spoiler" |> to_bool;
    title = next |> member "title" |> to_string;
    upvotes = next |> member "score" |> to_int;
  }

let post_of_text text : post =
  {
    author = "";
    created_utc = 0.0;
    subreddit = "";
    id = "";
    num_comments = 0;
    num_crossposts = 0;
    selftext = text;
    spoiler = false;
    title = "";
    upvotes = 0;
  }

let from_json json =
  json |> member "data" |> member "children" |> to_list
  |> List.map post_of_json

let recent_post (subreddit : subreddit) : post =
  match subreddit with
  | [] -> failwith "No posts available"
  | h :: _ -> h

let posts subreddit : post list = subreddit

let author post = post.author

let created_utc post = post.created_utc

let id post = post.id

let num_comments post = post.num_comments

let num_crossposts post = post.num_crossposts

let selftext post = post.selftext

let spoiler post = post.spoiler

let title post = post.title

let upvotes post = post.upvotes

let subreddit_name post = post.subreddit
