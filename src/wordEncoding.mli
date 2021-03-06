(**This module contains that functions and types necessary for word
   encoding*)

val post_text : Intake.post -> string
(**[post_text post] is the text of the [post] including the title and
   selftext*)

val word_json_to_array : Yojson.Basic.t -> string array
(**[word_json_to_array vocab_json] is the array of words in the
   [vocab_json]*)

val subreddit_json_to_words : Yojson.Basic.t -> string list
(**[subreddit_json_to_words subreddit_json] parses the words of every
   post in the subreddit and creates unqiue list of words*)

val subreddit_json_to_stemmed_words : Yojson.Basic.t -> string list
(**[subreddit_json_to_words subreddit_json] parses and stems every word
   of the post in the subreddit and creates a unique list of words*)

val write_words_to_json : out_channel -> string list -> unit
(**[write_words_to_json file words] writes all the words in the list to
   a json [file] that is a list of the words. Requires open brack for
   json has been written to file*)

val subreddit_json_to_word_json :
  (Yojson.Basic.t -> string list) -> Yojson.Basic.t -> string -> unit
(**[subreddit_json_to_word_json processor_function subreddit_json filename]
   writes all the words of every post in the subreddit to a json file.
   The way each of these words is processed is based on
   [processor_function]*)

val encode_post :
  Yojson.Basic.t ->
  (string -> string list) ->
  Intake.post ->
  (Intake.post -> int) ->
  int array
(**[encode_post vocab_json processor_function post] is the encoded
   version of [post] with respect to the processing function and the
   vocabulary json*)

val encode_subreddit :
  Yojson.Basic.t ->
  (string -> string list) ->
  Yojson.Basic.t ->
  (Intake.post -> int) ->
  int array list
(**[encode_subreddit word_json processor_function subreddit_json] is a
   2d matrix representing the posts of [subreddit_json]. Each row in the
   matrix is a vector representing the number of occurence of each word
   in a post*)

val find_frequencies :
  Yojson.Basic.t -> int array array -> (string * int) list
(**[find_frequencies word_json encoding] is an association list. Each
   item is a key (the vocab word) and a value (how many posts it appears
   in)*)