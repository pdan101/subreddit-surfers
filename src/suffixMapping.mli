val hashtbl_step2_3 : (string, string) Hashtbl.t
(**[hashtbl_step2_3] is the abstract hashtable that contains key value
   pairs corresponding to the suffix mappings necessary for the second
   and third steps in our porter stemmer implementation.*)

val hashtbl_step4 : (string, string) Hashtbl.t
(**[hashtbl_step2_3] is the abstract hashtable that contains key value
   pairs corresponding to the suffix mappings necessary for the fourth
   step in our porter stemmer implementation.*)

val find_suffix_binding :
  (string, string) Hashtbl.t -> string -> string * int
