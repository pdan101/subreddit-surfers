(**This module contains all the word processing functions for text*)

type stemmed_word = {
  original_word : string;
  units : string;
  num_vcs : int;
  stemmed : string;
}
(**the abstract type containing data about a word*)

type vocabulary
(**the abstract type containing all the words seen so far*)

type text_block = {
  original_text : string;
  stemmed_text : string;
}

val parse : string -> string list
(**[parse text] is the list of words contained in text excluding
   trailing whitespace and punctuation, but including the punctuation of
   a conjunction. Requires text is in English*)

val stemmer : string -> stemmed_word
(**[stemer word] is the stemmed version of word*)

val parse_sentence : string -> string list
(**[parse text] is the list of words contained in text exlcuding
   trailing whitespace and punctuation Requires: text is in English and
   follows English grammar rules*)

val stem_word_list : string list -> stemmed_word list
(**[stem_word_list words] is the list of stemmed_words contained in
   words. Requires: text is in English*)

val extract_stemmed : stemmed_word list -> string list
(**[extract_stemmed stemmed_words] is the list of the stemmed version of
   words contained in stemmed_words*)

val process_sentence : string -> string
(**[processed_sentence sentence] is the stemmed version of sentence*)

val remove_stop_words : string list -> string list
(**[remove_stop_words words] is words with stop words removed*)

val make_text_block : string -> text_block
(**[make_text_block text] is the text_block of text*)

val stemmed_text_block : text_block -> string
(**[stemmed_text_block] is the stemmed text of text_block*)

val create_units : string -> string
(**[create_units] has all vowels and consonants summarized with either V
   or C*)

val calc_vc : string -> int
(**[calc_vc] is the number of VC pairs. This is the same as m in the
   official porter stemmer paper*)

val remove_plurals : string -> string
(**[remove_plurals] removes the following suffixes: SSES, IES, SS, S.
   This is the same as Step 1a in the official porter stemer paper*)

val remove_past_participles : string -> string
(**[remove_past_particples] removes the following suffixes: EED (if
   there is at least one VC pair), ED (if this leaves at least one vowel
   in the stem), ING (if this leaves at leat one vowel in the stem).
   This is the same as Step 1b in the official porter stemmer paper*)

val finalize_plurals_past_participles : string -> string
(**[finalize_plurals_past_participles] checks if the last 3 characters
   in the stemmed word follow the pattern CVC without grouping. Should
   only be used if remove_plurals or remove_past_participles has altered
   the word*)

val replace_suffix : string -> string
(**[replace_suffix word] is the adjusted [word] after checking and
   replacing known suffixes in step 2, 3, and 4 of our porter stemmer.
   Returns [word] if no suffixes are found.*)

val create_simplified_units : string -> string -> string
(**[create_simplified_units] has all vowels and consonants grouped
   together and represented with either V or C*)

val fix_y : string -> string
(**[fix_y] adds and "i" to a stemmed word if it contains a vowel. This
   is Step 1c in the porter stemmer paper*)

val remove_e : string -> string
(**[remove_e] adds "e" to the end of a stemmed word if the number of
   VC's is greater than 1 or the number of VC's is 1 and does not end
   with CVC. This is Step 5a in the porter stemmer paper*)

val check_double_consonant : string -> string
(**[check_double_consonant] removes double consonants from the end of
   stemmed words if the number VC's is greater than 1*)
