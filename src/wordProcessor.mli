(**This module contains all the word processing functions for text*)

type word_data
(**the abstract type containing data about a word*)

type vocabulary
(**the abstract type containing all the words seen so far*)

val parse : string -> string list
(**[parse text] is the list of words contained in text excluding
   trailing whitespace and punctuation, but including the punctuation of
   a conjunction. Requires text is in English*)

val stem : string -> string
(**[stem word] is the stemmed version of word*)

val parse_sentence : string -> string list
(**[parse text] is the list of words contained in text exlcuding
   trailing whitespace and punctuation Requires: text is in English and
   follows English grammar rules*)
