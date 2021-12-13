(**This module contains all the word processing functions specific to
   the stemmer for text*)

val create_units : string -> string
(**[create_units word] has all vowels and consonants of [word]
   summarized with either V or C*)

val calc_vc : string -> int
(**[calc_vc word] is the number of VC pairs in [word]. This is the same
   as m in the official porter stemmer paper*)

val remove_plurals : string -> string
(**[remove_plurals word] removes the following suffixes: SSES, IES, SS,
   S from [word]. This is the same as Step 1a in the official porter
   stemer paper*)

val remove_past_participles : string -> string
(**[remove_past_particples word] removes the following suffixes from
   [word]: EED (if there is at least one VC pair), ED (if this leaves at
   least one vowel in the stem), ING (if this leaves at leat one vowel
   in the stem). This is the same as Step 1b in the official porter
   stemmer paper*)

val finalize_plurals_past_participles : string -> string
(**[finalize_plurals_past_participles word] checks if the last 3
   characters in the stemmed [word] follow the pattern CVC without
   grouping. Should only be used if remove_plurals or
   remove_past_participles has altered the word*)

val create_simplified_units : string -> string -> string
(**[create_simplified_units raw_units acc] has all vowels and consonants
   of [raw_units] grouped together and represented with either V or C*)

val fix_y : string -> string
(**[fix_y word] adds and "i" to a stemmed [word] if it contains a vowel.
   This is Step 1c in the porter stemmer paper*)

val remove_e : string -> string
(**[remove_e word] adds "e" to the end of a stemmed [word] if the number
   of VC's is greater than 1 or the number of VC's is 1 and does not end
   with CVC. This is Step 5a in the porter stemmer paper*)

val check_double_consonant : string -> string
(**[check_double_consonant word] removes double consonants from the end
   of the stemmed [word] if the number VC's is greater than 1*)

val apply_finalize : string -> bool
(**[apply_finalize word] determines if
   [finalize_plurals_past_participles word] should be called based on
   the result of [remove_past_participles word]*)
