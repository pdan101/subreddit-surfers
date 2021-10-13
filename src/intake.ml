open Yojson.Basic.Util

type author = string

type post = {
  author : author;
  created_utc : float;
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
    id = next |> member "id" |> to_string;
    num_comments = next |> member "num_comments" |> to_int;
    num_crossposts = next |> member "num_crossposts" |> to_int;
    selftext = next |> member "selftext" |> to_string;
    spoiler = next |> member "spoiler" |> to_bool;
    title = next |> member "title" |> to_string;
    upvotes = next |> member "score" |> to_int;
  }

let from_json json =
  json |> member "data" |> member "children" |> to_list
  |> List.map post_of_json

let posts subreddit : post list = subreddit

let recent_post (subreddit : subreddit) : post =
  match subreddit with
  | [] -> failwith "No posts available"
  | h :: _ -> h

let post_ids subreddit =
  List.sort_uniq compare (List.map (fun x -> x.id) subreddit)

let find_post post_id posts = List.find (fun x -> x.id = post_id) posts

let author subreddit post_id = (find_post post_id subreddit).author

let created_utc subreddit post_id =
  (find_post post_id subreddit).created_utc

let id subreddit post_id = (find_post post_id subreddit).id

let num_comments subreddit post_id =
  (find_post post_id subreddit).num_comments

let num_crossposts subreddit post_id =
  (find_post post_id subreddit).num_crossposts

let selftext post = post.selftext

let spoiler subreddit post_id = (find_post post_id subreddit).spoiler

let title subreddit post_id = (find_post post_id subreddit).title
