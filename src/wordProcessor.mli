(**This module contains all the word processing functions for text*)

type word_data
(**the abstract type containing data about a word*)

type vocabulary
(**the abstract type containing all the words seen so far*)

val parse : string -> string list
(**[parse text] is the list of words contained in text excluding
   trailing whitespace and punctuation, but including the punctuation of
   a conjunction. Requires text is in English*)

val create_units : string -> string
(**[create_units] is the string with all vowels and consonants grouped
   and summarized with either V or C*)

val calc_vc : string -> int
(**[calc_vc] is the number of VC pairs*)

val remove_plurals : string -> string
(**[remove_plurals] removes the following suffixes: SSES, IES, SS, S*)

val remove_past_participles : string -> int -> string
(**[remove_past_particples] removes the following suffixes: EED (if
   there is at least one VC pair), ED (if this leaves at least one vowel
   in the stem), ING (if this leaves at leat one vowel in the stem)*)

val stem : string -> string
(**[stem word] is the stemmed version of word*)

val parse_sentence : string -> string list
(**[parse text] is the list of words contained in text exlcuding
   trailing whitespace and punctuation Requires: text is in English and
   follows English grammar rules*)
