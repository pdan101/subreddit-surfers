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

val parse : string -> string list
(**[parse text] is the list of words contained in [text] excluding
   trailing whitespace and punctuation, but including the punctuation of
   a conjunction. Requires text is in English*)

val stemmer : string -> stemmed_word
(**[stemer word] is the stemmed version of [word]*)

val parse_sentence : string -> string list
(**[parse text] is the list of words contained in [text] excluding
   trailing whitespace and punctuation Requires: [text] is in English
   and follows English grammar rules*)

val stem_word_list : string list -> stemmed_word list
(**[stem_word_list words] is the list of stemmed_words contained in
   [words]. Requires: text is in English*)

val stem_text : string -> string list
(**[stem_text text] is the list of words in [text] stemmed*)

val extract_stemmed : stemmed_word list -> string list
(**[extract_stemmed stemmed_words] is the list of the stemmed version of
   words contained in [stemmed_words]*)

val process_sentence : string -> string
(**[processed_sentence sentence] is the stemmed version of [sentence]*)

val is_stopword : string -> bool
(**[is_stopword word] is true if the [word] is a stopword based on
   NLTK's stopword list*)

val replace_suffix : string -> string
(**[replace_suffix word] is the adjusted [word] after checking and
   replacing known suffixes in step 2, 3, and 4 of our porter stemmer.
   Returns [word] if no suffixes are found.*)

val stem_paragraph : string -> string
(**[stem_paragraph paragraph] returns the stemmed version of a
   [paragraph].*)
