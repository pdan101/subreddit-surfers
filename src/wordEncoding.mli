(**This module contains that functions and types necessary for word
   encoding*)

val write_words_to_json : out_channel -> string list -> unit
(**[write_words_to_json file words] writes all the words in the list to
   a json file that is a list of the words. Requires open brack for json
   has been written to file*)
