open Yojson.Basic.Util

type author = string

type post = {
  author : author;
  created_utc : int;
  id : string;
  num_comments : int;
  num_crossposts : int;
  selftext : string;
  spoiler : bool;
  title : string;
}

type subreddit = {
  posts : post list;
  subreddit_name : string;
  subreddit_id : string;
  subreddit_subscribers : int;
  subreddit_type : string;
}

let post_of_json json =
  {
    author = json |> member "author" |> to_string;
    created_utc = json |> member "created_utc" |> to_int;
    id = json |> member "id" |> to_string;
    num_comments = json |> member "num_comments" |> to_int;
    num_crossposts = json |> member "num_crossposts" |> to_int;
    selftext = json |> member "selftext" |> to_string;
    spoiler = json |> member "spoiler" |> to_bool;
    title = json |> member "title" |> to_string;
  }

let from_json json =
  {
    posts = json |> member "posts" |> to_list |> List.map post_of_json;
    subreddit_name = json |> member "subreddit_name" |> to_string;
    subreddit_id = json |> member "subreddit_id" |> to_string;
    subreddit_subscribers =
      json |> member "subreddit_subscribers" |> to_int;
    subreddit_type = json |> member "subreddit_type" |> to_string;
  }

let subreddit_name subreddit = subreddit.subreddit_name

let subreddit_id subreddit = subreddit.subreddit_id

let subreddit_subscribers subreddit = subreddit.subreddit_subscribers

let subreddit_type subreddit = subreddit.subreddit_type

let post_ids subreddit =
  List.sort_uniq compare (List.map (fun x -> x.id) subreddit.posts)

let find_post post_id posts = List.find (fun x -> x.id = post_id) posts

let author subreddit post_id =
  (find_post post_id subreddit.posts).author

let created_utc subreddit post_id =
  (find_post post_id subreddit.posts).created_utc

let id subreddit post_id = (find_post post_id subreddit.posts).id

let num_comments subreddit post_id =
  (find_post post_id subreddit.posts).num_comments

let num_crossposts subreddit post_id =
  (find_post post_id subreddit.posts).num_crossposts

let selftext subreddit post_id =
  (find_post post_id subreddit.posts).selftext

let spoiler subreddit post_id =
  (find_post post_id subreddit.posts).spoiler

let title subreddit post_id = (find_post post_id subreddit.posts).title
