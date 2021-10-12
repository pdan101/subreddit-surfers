(** This module contains the representation for the sentiment file. This
    file uses vaderSentiment to calculate the polarity score of input
    sentences. *)

val polarity_score : string -> float
(**[polarity_score] determines the sentiment of a string of text. *)
