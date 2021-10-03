(** This module contains the representation for the intake file. This
    file extracts data from JSON's that satisfy the schema.json file and
    converts it into more usable types, which can be used in function
    applications. *)

type t
(** The abstract type of values representing adventures. *)

type room_id = string
(** The type of room identifiers. *)

type exit_name = string
(** The type of exit names. *)

exception UnknownRoom of room_id
(** Raised when an unknown room is encountered. *)

exception UnknownExit of exit_name
(** Raised when an unknown exit is encountered. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the adventure that [j] represents. Requires: [j] is
    a valid JSON adventure representation. *)

val start_room : t -> room_id
(** [start_room a] is the identifier of the starting room in adventure
    [a]. *)

val room_ids : t -> room_id list
(** [room_ids a] is a set-like list of all of the room identifiers in
    adventure [a]. *)

val description : t -> room_id -> string
(** [description a r] is the description of room [r] in adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)

val exits : t -> room_id -> exit_name list
(** [exits a r] is a set-like list of all exit names from room [r] in
    adventure [a]. Raises [UnknownRoom r] if [r] is not a room
    identifier in [a]. *)

val next_room : t -> room_id -> exit_name -> room_id
(** [next_room a r e] is the room to which [e] exits from room [r] in
    adventure [a]. Raises [UnknownRoom r] if [r] is not a room
    identifier in [a]. Raises [UnknownExit e] if [e] is not an exit from
    room [r] in [a]. *)

val next_rooms : t -> room_id -> room_id list
(** [next_rooms a r] is a set-like list of all rooms to which there is
    an exit from [r] in adventure [a]. Raises [UnknownRoom r] if [r] is
    not a room identifier in [a].*)
